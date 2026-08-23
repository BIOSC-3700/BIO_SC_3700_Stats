# Assumption checks. Each check returns a list with a status
# ("ok", "warn", "fail", "info"), a one-line title, a plain-language
# detail string, and optionally a table to display.
#
# Levene's test is implemented here rather than pulled from car: it is
# a one-way ANOVA on absolute deviations from the group medians, which
# is ten lines and saves a package download in the browser.

levene_test <- function(value, group) {
  keep <- !is.na(value) & !is.na(group)
  value <- value[keep]
  group <- droplevels(factor(group[keep]))
  if (nlevels(group) < 2 || length(value) <= nlevels(group)) {
    return(NULL)
  }
  medians <- tapply(value, group, stats::median)
  z <- abs(value - medians[as.character(group)])
  if (stats::var(z) == 0) {
    return(NULL)
  }
  fit <- stats::aov(z ~ group)
  tab <- summary(fit)[[1]]
  out <- list(
    statistic = tab[1, "F value"],
    df1 = tab[1, "Df"],
    df2 = tab[2, "Df"],
    p.value = tab[1, "Pr(>F)"]
  )
  return(out)
}

# Shapiro-Wilk per group, skipping groups the test cannot handle.
shapiro_by_group <- function(data) {
  groups <- levels(droplevels(data$group))
  rows <- lapply(groups, function(g) {
    x <- data$value[data$group == g & !is.na(data$value)]
    n <- length(x)
    if (n < 3 || n > 5000 || stats::var(x) == 0) {
      return(tibble::tibble(
        group = g, n = n, W = NA_real_, p = NA_real_
      ))
    }
    res <- try(stats::shapiro.test(x), silent = TRUE)
    if (inherits(res, "try-error")) {
      return(tibble::tibble(
        group = g, n = n, W = NA_real_, p = NA_real_
      ))
    }
    return(tibble::tibble(
      group = g, n = n,
      W = unname(res$statistic), p = res$p.value
    ))
  })
  return(dplyr::bind_rows(rows))
}

check_normality <- function(data) {
  tab <- shapiro_by_group(data)
  testable <- tab[!is.na(tab$p), ]
  if (nrow(testable) == 0) {
    return(list(
      status = "info",
      title = "Normality could not be tested",
      detail = paste(
        "Shapiro-Wilk needs at least 3 values per group, and some",
        "variation among them. Look at the QQ plot instead."
      ),
      table = tab
    ))
  }
  flagged <- testable[testable$p < 0.05, ]
  min_n <- min(testable$n)
  if (nrow(flagged) == 0) {
    return(list(
      status = "ok",
      title = "No evidence against normality",
      detail = glue::glue(
        "Shapiro-Wilk is non-significant in every group ",
        "(smallest p = {fmt_p(min(testable$p))}). Points in the QQ ",
        "plot should sit close to the diagonal line."
      ),
      table = tab
    ))
  }
  names_flagged <- paste(flagged$group, collapse = ", ")
  if (min_n >= 30) {
    return(list(
      status = "warn",
      title = "Shapiro-Wilk is significant, but your samples are large",
      detail = glue::glue(
        "Normality is rejected in: {names_flagged}. With n ≥ 30 per ",
        "group the t-test and ANOVA are fairly robust to non-normal ",
        "data, and Shapiro-Wilk detects trivial departures at large ",
        "n. Check the QQ plot for how bad the departure actually is ",
        "before worrying."
      ),
      table = tab
    ))
  }
  return(list(
    status = "fail",
    title = "Normality is questionable in a small sample",
    detail = glue::glue(
      "Normality is rejected in: {names_flagged}, and the smallest ",
      "group has n = {min_n}. Small samples are where non-normality ",
      "actually matters. Look at the QQ plot, check whether an ",
      "outlier or a skew is driving it, and ask your instructor ",
      "whether a rank-based test is more appropriate."
    ),
    table = tab
  ))
}

# For a factorial model, normality applies to the residuals, not to
# each cell on its own -- factorial cells are usually far too small for
# a per-cell Shapiro-Wilk to say anything useful. Reuses
# shapiro_by_group() by handing it the residuals as a single group.
residual_frame <- function(fit) {
  return(tibble::tibble(
    value = unname(stats::residuals(fit)),
    group = factor("Residuals")
  ))
}

check_normality_residuals <- function(fit) {
  data <- residual_frame(fit)
  tab <- shapiro_by_group(data)
  n <- tab$n[1]
  if (is.na(tab$p[1])) {
    return(list(
      status = "info",
      title = "Normality of residuals could not be tested",
      detail = paste(
        "Shapiro-Wilk needs at least 3 residuals with some variation",
        "among them. Look at the QQ plot instead."
      ),
      table = NULL
    ))
  }
  if (tab$p[1] >= 0.05) {
    return(list(
      status = "ok",
      title = "No evidence against normal residuals",
      detail = glue::glue(
        "Shapiro-Wilk on the {n} model residuals is non-significant ",
        "({fmt_p_inline(tab$p[1])}). Points in the QQ plot should sit ",
        "close to the diagonal line."
      ),
      table = NULL
    ))
  }
  if (n >= 50) {
    return(list(
      status = "warn",
      title = "Shapiro-Wilk is significant, but you have many residuals",
      detail = glue::glue(
        "Normality of the residuals is rejected ",
        "({fmt_p_inline(tab$p[1])}), though with {n} residuals the ",
        "test detects departures too small to matter. ANOVA is fairly ",
        "robust at this size. Check the QQ plot for how bad the ",
        "departure actually looks before worrying."
      ),
      table = NULL
    ))
  }
  return(list(
    status = "fail",
    title = "Residuals do not look normal",
    detail = glue::glue(
      "Shapiro-Wilk on the {n} model residuals is significant ",
      "({fmt_p_inline(tab$p[1])}), and with this few residuals that ",
      "matters. Check the QQ plot, and look at whether one cell or a ",
      "single extreme point is driving it."
    ),
    table = NULL
  ))
}

check_variance <- function(data, welch_used = TRUE) {
  res <- levene_test(data$value, data$group)
  if (is.null(res)) {
    return(list(
      status = "info",
      title = "Equal variance could not be tested",
      detail = paste(
        "Levene's test needs at least two groups with variation in",
        "each. Compare the SD column in the summary table instead."
      ),
      table = NULL
    ))
  }
  sds <- data |>
    dplyr::filter(!is.na(.data$value)) |>
    dplyr::group_by(.data$group) |>
    dplyr::summarize(sd = stats::sd(.data$value), .groups = "drop")
  ratio <- max(sds$sd, na.rm = TRUE) / min(sds$sd, na.rm = TRUE)
  stat_txt <- glue::glue(
    "Levene's test: F({res$df1}, {res$df2}) = ",
    "{fmt_num(res$statistic, 2)}, {fmt_p_inline(res$p.value)}. ",
    "Largest SD is {fmt_num(ratio, 1)}× the smallest."
  )
  if (is.na(res$p.value) || res$p.value >= 0.05) {
    return(list(
      status = "ok",
      title = "No evidence against equal variances",
      detail = glue::glue(
        "{stat_txt} The spread looks similar across groups."
      ),
      table = NULL
    ))
  }
  relief <- if (welch_used) {
    paste(
      "This analysis does not assume equal variances, so the result",
      "below already accounts for it."
    )
  } else {
    paste(
      "This analysis assumes equal variances, so switch that option",
      "off, or read the Welch result reported alongside it."
    )
  }
  return(list(
    status = "warn",
    title = "Variances differ across groups",
    detail = glue::glue("{stat_txt} {relief}"),
    table = NULL
  ))
}

# 1.5 x IQR within each group. Flagging is not permission to delete.
outlier_flags <- function(data) {
  out <- data |>
    dplyr::group_by(.data$group) |>
    dplyr::mutate(
      .q1 = stats::quantile(.data$value, 0.25, na.rm = TRUE),
      .q3 = stats::quantile(.data$value, 0.75, na.rm = TRUE),
      .iqr = .data$.q3 - .data$.q1,
      is_outlier = !is.na(.data$value) & .data$.iqr > 0 &
        (.data$value < .data$.q1 - 1.5 * .data$.iqr |
           .data$value > .data$.q3 + 1.5 * .data$.iqr)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-".q1", -".q3", -".iqr")
  return(out)
}

# mention_plot is FALSE on the two-way tab, whose dodged boxplot draws
# the points but does not ring them -- the panel should not promise a
# marking that is not there.
check_outliers <- function(data, mention_plot = TRUE) {
  flagged <- outlier_flags(data)
  n_out <- sum(flagged$is_outlier)
  if (n_out == 0) {
    return(list(
      status = "ok",
      title = "No outliers flagged",
      detail = paste(
        "No value sits more than 1.5 × IQR outside its group's",
        "quartiles."
      ),
      table = NULL
    ))
  }
  where <- flagged |>
    dplyr::filter(.data$is_outlier) |>
    dplyr::count(.data$group, name = "n") |>
    dplyr::mutate(txt = glue::glue("{group} ({n})")) |>
    dplyr::pull("txt") |>
    paste(collapse = ", ")
  return(list(
    status = "warn",
    title = glue::glue(
      "{n_out} {ifelse(n_out == 1, 'point', 'points')} flagged as ",
      "{ifelse(n_out == 1, 'an outlier', 'outliers')}"
    ),
    detail = glue::glue(
      "Flagged in: {where}. ",
      "{ifelse(mention_plot, 'These are circled in red on the plot. ', '')}",
      "A flag is not permission to delete the point. Check it against ",
      "your notes first — a real measurement stays in, a transcription ",
      "error gets fixed."
    ),
    table = NULL
  ))
}

check_independence <- function() {
  return(list(
    status = "info",
    title = "Independence cannot be tested",
    detail = paste(
      "No statistic can tell you whether your observations are",
      "independent — only your study design can. Ask yourself: did",
      "each measurement come from a different individual, plant, or",
      "plate? Repeated measurements on the same subject are not",
      "independent and need a paired or repeated-measures analysis."
    ),
    table = NULL
  ))
}
