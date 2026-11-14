# Number, p-value, and prose formatting shared across all tabs.

# base R gained %||% in 4.4.0; define it locally so the app does not
# depend on which R version webR happens to ship.
`%||%` <- function(x, y) {
  return(if (is.null(x)) y else x)
}

# Chart and badge palette.
pal <- list(
  accent = "#2a78d6",
  muted = "#898781",
  critical = "#d03b3b",
  warning = "#fab219",
  good = "#0ca30c",
  ink = "#0b0b0b",
  ink2 = "#52514e",
  grid = "#e1e0d9",
  axis = "#c3c2b7",
  surface = "#ffffff",

  # Categorical slots, used only where color carries identity rather
  # than emphasis -- currently just the two-way interaction plot, where
  # each line is a level of a factor.
  categorical = c(
    "#2a78d6",
    "#eb6834",
    "#1baf7a",
    "#eda100",
    "#e87ba4",
    "#008300",
    "#4a3aa7",
    "#e34948"
  ),

  # Point shapes pair with the categorical slots so identity survives
  # a colorblind reader or a grayscale printout.
  shapes = c(16, 17, 15, 18, 8, 7, 10, 4)
)

# Format numbers for display. Keeps small values readable instead of
# collapsing them to 0.000.
fmt_num <- function(x, digits = 3) {
  if (length(x) == 0) {
    return(character(0))
  }
  out <- vapply(
    x,
    function(v) {
      if (is.na(v)) {
        return("—")
      }
      if (v != 0 && abs(v) < 10^(-digits)) {
        return(formatC(v, format = "e", digits = 2))
      }
      return(formatC(v, format = "f", digits = digits, big.mark = ","))
    },
    character(1)
  )
  return(unname(out))
}

# p-values get their own rule: never report "p = 0", and never imply
# more precision than the test can support.
fmt_p <- function(p, digits = 4) {
  out <- vapply(
    p,
    function(v) {
      if (is.na(v)) {
        return("—")
      }
      if (v < 0.0001) {
        return("< 0.0001")
      }
      return(formatC(v, format = "f", digits = digits))
    },
    character(1)
  )
  return(unname(out))
}

fmt_ci <- function(lo, hi, digits = 3) {
  return(glue::glue("[{fmt_num(lo, digits)}, {fmt_num(hi, digits)}]"))
}

# "p = 0.003" or "p < 0.0001", ready to drop into a sentence.
fmt_p_inline <- function(p, digits = 4) {
  txt <- fmt_p(p, digits)
  return(ifelse(
    startsWith(txt, "<"),
    glue::glue("p {txt}"),
    glue::glue("p = {txt}")
  ))
}

# Significance wording. Deliberately avoids "proves" and "no
# difference"; students reach for both.
verdict_text <- function(p, alpha = 0.05) {
  if (is.na(p)) {
    return("The test could not be computed.")
  }
  if (p < alpha) {
    return(glue::glue(
      "Reject the null hypothesis at α = {alpha}. The difference is ",
      "larger than sampling variation alone comfortably explains."
    ))
  }
  return(glue::glue(
    "Fail to reject the null hypothesis at α = {alpha}. This is not ",
    "evidence that the groups are the same — only that this sample ",
    "does not show a difference."
  ))
}

verdict_class <- function(p, alpha = 0.05) {
  if (is.na(p)) {
    return("verdict verdict-none")
  }
  return(if (p < alpha) "verdict verdict-sig" else "verdict verdict-ns")
}

# Stacks a list of one-row tibbles into one tibble. NULL entries are
# skipped, and an empty list gives NULL.
bind_tibbles <- function(rows) {
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) {
    return(NULL)
  }
  return(tibble::as_tibble(do.call(rbind, rows)))
}

# Standard error of the mean, used in every group summary table.
se_mean <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 2) {
    return(NA_real_)
  }
  return(stats::sd(x) / sqrt(length(x)))
}

# Per-group n / mean / SD / SE / min / max, the table shown under every
# preview and beside every result.
group_summary <- function(data) {
  keep <- !is.na(data$value)
  group <- data$group[keep]
  if (!is.factor(group)) {
    group <- factor(group)
  }
  present <- droplevels(group)
  by_group <- split(data$value[keep], present)
  stat <- function(fn) {
    return(unname(vapply(by_group, fn, numeric(1))))
  }
  out <- tibble::tibble(
    group = factor(levels(present), levels = levels(group)),
    n = unname(vapply(by_group, length, integer(1))),
    mean = stat(mean),
    sd = stat(stats::sd),
    se = stat(se_mean),
    min = stat(min),
    max = stat(max)
  )
  return(out)
}

# Renders a summary tibble with the numeric columns already formatted.
summary_table <- function(data, digits = 3, compact = FALSE) {
  if (compact) {
    data <- data[setdiff(names(data), c("min", "max"))]
  }
  out <- data
  for (col in setdiff(names(out), c("group", "n"))) {
    out[[col]] <- fmt_num(out[[col]], digits)
  }
  names(out) <- if (compact) {
    c("Group", "n", "Mean", "SD", "SE")
  } else {
    c("Group", "n", "Mean", "SD", "SE", "Min", "Max")
  }
  return(out)
}
