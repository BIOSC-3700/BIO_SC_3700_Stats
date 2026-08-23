# Shared plot theme and builders. Every figure is drawn on a white
# surface so that a downloaded PNG drops straight into a lab report.
#
# Color rules follow from the data's job: the x-axis already carries
# group identity on the group plots, so all groups share one accent
# color rather than each getting its own hue. Color is spent only where
# it means something — a flagged outlier, a reference line, a Tukey
# interval that clears zero.

theme_bio3700 <- function(base_size = 14) {
  out <- ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(
        fill = pal$surface, color = NA
      ),
      panel.background = ggplot2::element_rect(
        fill = pal$surface, color = NA
      ),
      panel.grid.major.y = ggplot2::element_line(
        color = pal$grid, linewidth = 0.4
      ),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(
        color = pal$axis, linewidth = 0.5
      ),
      axis.ticks = ggplot2::element_line(
        color = pal$axis, linewidth = 0.4
      ),
      axis.text = ggplot2::element_text(color = pal$ink2),
      axis.title = ggplot2::element_text(color = pal$ink2, face = "bold"),
      plot.title = ggplot2::element_text(
        color = pal$ink, face = "bold", size = ggplot2::rel(1.1),
        margin = ggplot2::margin(b = 8)
      ),
      plot.subtitle = ggplot2::element_text(
        color = pal$ink2, margin = ggplot2::margin(b = 10)
      ),
      plot.caption = ggplot2::element_text(color = pal$muted, hjust = 0),
      legend.position = "top",
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(color = pal$ink2),
      strip.text = ggplot2::element_text(color = pal$ink, face = "bold"),
      plot.margin = ggplot2::margin(12, 14, 10, 12)
    )
  return(out)
}

# Jitter is precomputed as a column rather than left to
# position_jitter(). A position object re-draws from the RNG for each
# layer it is applied to, so a 1-row outlier subset lands somewhere
# different than the same point in the full data and the highlight ring
# drifts off its point. Computing x once and reusing the column keeps
# every layer registered. The RNG state is saved and restored so the
# fixed seed does not leak into the rest of the session.
jitter_x <- function(group, width = 0.14) {
  has_seed <- exists(".Random.seed", envir = globalenv())
  if (has_seed) {
    old_seed <- get(".Random.seed", envir = globalenv())
    on.exit(assign(".Random.seed", old_seed, envir = globalenv()))
  }
  set.seed(42)
  offset <- stats::runif(length(group), -width, width)
  return(as.integer(group) + offset)
}

# Boxplot / violin / points for one measurement across groups, with
# outliers ringed in red and group means marked.
plot_groups <- function(data, style = "box", title = NULL,
                        xlab = "Group", ylab = "Value",
                        show_outliers = TRUE, show_mean = TRUE) {
  data <- outlier_flags(data) |> dplyr::filter(!is.na(.data$value))
  data$group <- droplevels(data$group)
  group_levels <- levels(data$group)
  data$x <- as.integer(data$group)
  data$xj <- jitter_x(data$group)
  p <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = .data$x, y = .data$value, group = .data$group)
  )
  if (style == "box") {
    p <- p + ggplot2::geom_boxplot(
      width = 0.55, fill = pal$accent, alpha = 0.16,
      color = pal$accent, linewidth = 0.7, outlier.shape = NA
    )
  } else if (style == "violin") {
    p <- p + ggplot2::geom_violin(
      width = 0.8, fill = pal$accent, alpha = 0.16,
      color = pal$accent, linewidth = 0.7, trim = FALSE
    )
  }
  p <- p + ggplot2::geom_point(
    ggplot2::aes(x = .data$xj), size = 2.2, alpha = 0.75,
    color = pal$accent
  )
  if (show_outliers && any(data$is_outlier)) {
    p <- p + ggplot2::geom_point(
      data = dplyr::filter(data, .data$is_outlier),
      ggplot2::aes(x = .data$xj), shape = 21, size = 4.2, stroke = 1.1,
      color = pal$critical, fill = NA
    )
  }
  if (show_mean) {
    p <- p + ggplot2::stat_summary(
      fun = mean, geom = "point", shape = 23, size = 3.4,
      fill = pal$surface, color = pal$ink, stroke = 1
    )
  }
  p <- p +
    ggplot2::scale_x_continuous(
      breaks = seq_along(group_levels), labels = group_levels,
      limits = c(0.5, length(group_levels) + 0.5)
    ) +
    ggplot2::labs(x = xlab, y = ylab, title = title) +
    theme_bio3700()
  return(p)
}

# Before/after slope plot for paired data.
plot_paired <- function(data, title = NULL, xlab = "Condition",
                        ylab = "Value") {
  p <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = .data$group, y = .data$value, group = .data$pair_id)
  ) +
    ggplot2::geom_line(color = pal$muted, linewidth = 0.5, alpha = 0.8) +
    ggplot2::geom_point(color = pal$accent, size = 2.4, alpha = 0.85) +
    ggplot2::stat_summary(
      ggplot2::aes(group = 1), fun = mean, geom = "line",
      color = pal$ink, linewidth = 1.1
    ) +
    ggplot2::stat_summary(
      ggplot2::aes(group = 1), fun = mean, geom = "point",
      shape = 23, size = 3.4, fill = pal$surface, color = pal$ink,
      stroke = 1
    ) +
    ggplot2::labs(
      x = xlab, y = ylab, title = title,
      caption = paste(
        "Thin lines join paired observations;",
        "the dark line is the mean."
      )
    ) +
    theme_bio3700()
  return(p)
}

# One-sample layout: the observations on one row, the mean and its CI
# on another, and a dashed reference line at the hypothesized mean.
plot_one_sample <- function(values, mu0, ci_low, ci_high, mean_val,
                            title = NULL, xlab = "Value",
                            conf_level = 0.95) {
  values <- values[!is.na(values)]
  ci_row <- glue::glue("Mean ({round(conf_level * 100)}% CI)")
  levels_y <- c(as.character(ci_row), "Observations")
  obs <- tibble::tibble(
    value = values,
    row = factor("Observations", levels = levels_y)
  )
  est <- tibble::tibble(
    row = factor(as.character(ci_row), levels = levels_y),
    value = mean_val, lo = ci_low, hi = ci_high
  )
  p <- ggplot2::ggplot() +
    ggplot2::geom_vline(
      xintercept = mu0, linetype = "dashed",
      color = pal$ink2, linewidth = 0.6
    ) +
    ggplot2::geom_point(
      data = obs,
      ggplot2::aes(x = .data$value, y = .data$row),
      position = ggplot2::position_jitter(
        height = 0.12, width = 0, seed = 42
      ),
      size = 2.2, alpha = 0.7, color = pal$accent
    ) +
    ggplot2::geom_pointrange(
      data = est,
      ggplot2::aes(
        x = .data$value, y = .data$row,
        xmin = .data$lo, xmax = .data$hi
      ),
      color = pal$ink, linewidth = 0.8, size = 0.9
    ) +
    ggplot2::scale_y_discrete(limits = rev(levels_y), drop = FALSE) +
    ggplot2::labs(
      x = xlab, y = NULL, title = title,
      caption = glue::glue(
        "Dashed line marks the hypothesized mean μ₀ = {fmt_num(mu0, 3)}."
      )
    ) +
    theme_bio3700() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
  return(p)
}

# QQ plot per group, the visual companion to Shapiro-Wilk.
plot_qq <- function(data, title = NULL) {
  data <- dplyr::filter(data, !is.na(.data$value))
  p <- ggplot2::ggplot(data, ggplot2::aes(sample = .data$value)) +
    ggplot2::stat_qq_line(color = pal$ink2, linewidth = 0.6) +
    ggplot2::stat_qq(color = pal$accent, size = 2, alpha = 0.8) +
    ggplot2::facet_wrap(ggplot2::vars(.data$group), scales = "free") +
    ggplot2::labs(
      x = "Theoretical quantiles", y = "Sample quantiles", title = title,
      caption = "Normal data falls close to the line."
    ) +
    theme_bio3700(base_size = 12) +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_line(
      color = pal$grid, linewidth = 0.4
    ))
  return(p)
}

# Tukey confidence intervals. Intervals clearing zero are drawn in the
# accent color and the rest recede to gray; the position relative to
# the zero line and the printed p-value both carry the same
# information, so color is never the only cue.
plot_tukey <- function(tukey_tbl, conf_level = 0.95, title = NULL,
                       ylab = "Difference in means") {
  data <- tukey_tbl |>
    dplyr::mutate(
      sig = factor(
        ifelse(
          .data$p_adj < 1 - conf_level, "Significant", "Not significant"
        ),
        levels = c("Significant", "Not significant")
      ),
      comparison = factor(
        .data$comparison, levels = rev(.data$comparison)
      ),
      label = as.character(fmt_p_inline(.data$p_adj))
    )
  span <- diff(range(c(data$lwr, data$upr, 0)))
  pad <- if (span > 0) span * 0.36 else 1
  p <- ggplot2::ggplot(
    data,
    ggplot2::aes(
      y = .data$comparison, x = .data$diff,
      xmin = .data$lwr, xmax = .data$upr, color = .data$sig
    )
  ) +
    ggplot2::geom_vline(
      xintercept = 0, color = pal$ink2, linewidth = 0.6,
      linetype = "dashed"
    ) +
    ggplot2::geom_pointrange(linewidth = 0.8, size = 0.8) +
    ggplot2::geom_text(
      ggplot2::aes(x = .data$upr, label = .data$label),
      hjust = -0.18, size = 3.6, color = pal$ink2, show.legend = FALSE
    ) +
    ggplot2::scale_color_manual(
      values = c(
        "Significant" = pal$accent, "Not significant" = pal$muted
      ),
      limits = c("Significant", "Not significant"),
      drop = FALSE
    ) +
    ggplot2::expand_limits(x = max(data$upr) + pad) +
    ggplot2::labs(
      x = ylab, y = NULL, title = title,
      caption = glue::glue(
        "Intervals that do not cross the dashed zero line are ",
        "significant at α = {round(1 - conf_level, 3)}."
      )
    ) +
    theme_bio3700() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
  return(p)
}

# ---- two-way factorial -----------------------------------------------
#
# These are the only plots in the app where color carries identity
# rather than emphasis, so they draw from pal$categorical. Identity
# never rests on hue alone: each level also gets its own point shape
# and, on the interaction plot, a label at the end of its line. That
# matters past three levels, where the categorical slots stop clearing
# the all-pairs separation gates.

cat_colors <- function(levels_vec) {
  n <- length(levels_vec)
  out <- pal$categorical[seq_len(min(n, length(pal$categorical)))]
  if (n > length(pal$categorical)) {
    out <- rep_len(pal$categorical, n)
  }
  names(out) <- levels_vec
  return(out)
}

cat_shapes <- function(levels_vec) {
  n <- length(levels_vec)
  out <- rep_len(pal$shapes, n)
  names(out) <- levels_vec
  return(out)
}

# Cell means with standard errors, the table behind both plots.
cell_means <- function(cells) {
  out <- cells |>
    dplyr::filter(!is.na(.data$value)) |>
    dplyr::group_by(.data$a, .data$b) |>
    dplyr::summarize(
      n = dplyr::n(),
      mean = mean(.data$value),
      se = se_mean(.data$value),
      .groups = "drop"
    )
  return(out)
}

# The headline figure for a factorial design: one line per level of the
# color factor. Parallel lines mean no interaction; lines that converge,
# diverge, or cross are the interaction made visible.
plot_interaction <- function(cells, xlab = "Factor A", ylab = "Value",
                             color_lab = "Factor B", title = NULL) {
  means <- cell_means(cells)
  b_levels <- levels(means$b)
  dodge <- ggplot2::position_dodge(width = 0.12)
  last_x <- levels(means$a)[nlevels(means$a)]
  ends <- dplyr::filter(means, .data$a == last_x)
  p <- ggplot2::ggplot(
    means,
    ggplot2::aes(
      x = .data$a, y = .data$mean, color = .data$b,
      shape = .data$b, group = .data$b
    )
  ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        ymin = .data$mean - .data$se, ymax = .data$mean + .data$se
      ),
      width = 0.08, linewidth = 0.6, position = dodge,
      show.legend = FALSE
    ) +
    ggplot2::geom_line(linewidth = 1, position = dodge) +
    ggplot2::geom_point(size = 3.2, position = dodge) +
    ggplot2::geom_text(
      data = ends,
      ggplot2::aes(label = .data$b),
      position = dodge, hjust = -0.25, size = 4, show.legend = FALSE
    ) +
    ggplot2::scale_color_manual(values = cat_colors(b_levels)) +
    ggplot2::scale_shape_manual(values = cat_shapes(b_levels)) +
    ggplot2::scale_x_discrete(
      expand = ggplot2::expansion(mult = c(0.10, 0.28))
    ) +
    ggplot2::labs(
      x = xlab, y = glue::glue("Mean {ylab} (± 1 SE)"), title = title,
      color = color_lab, shape = color_lab
    ) +
    theme_bio3700() +
    ggplot2::theme(legend.position = "top")
  return(p)
}

# Boxplots of the response across the x factor, dodged by the color
# factor, with the raw points overlaid.
plot_grouped_boxes <- function(cells, style = "box", xlab = "Factor A",
                               ylab = "Value", color_lab = "Factor B",
                               title = NULL) {
  cells <- dplyr::filter(cells, !is.na(.data$value))
  b_levels <- levels(cells$b)
  dodge_width <- 0.78
  p <- ggplot2::ggplot(
    cells,
    ggplot2::aes(x = .data$a, y = .data$value, color = .data$b)
  )
  if (style == "box") {
    p <- p + ggplot2::geom_boxplot(
      ggplot2::aes(fill = .data$b), alpha = 0.16, linewidth = 0.7,
      outlier.shape = NA, width = 0.66,
      position = ggplot2::position_dodge(width = dodge_width)
    )
  } else if (style == "violin") {
    p <- p + ggplot2::geom_violin(
      ggplot2::aes(fill = .data$b), alpha = 0.16, linewidth = 0.7,
      trim = FALSE, width = 0.78,
      position = ggplot2::position_dodge(width = dodge_width)
    )
  }
  p <- p +
    ggplot2::geom_point(
      ggplot2::aes(shape = .data$b), size = 2, alpha = 0.75,
      position = ggplot2::position_jitterdodge(
        jitter.width = 0.16, dodge.width = dodge_width, seed = 42
      )
    ) +
    ggplot2::scale_color_manual(values = cat_colors(b_levels)) +
    ggplot2::scale_fill_manual(values = cat_colors(b_levels)) +
    ggplot2::scale_shape_manual(values = cat_shapes(b_levels)) +
    ggplot2::labs(
      x = xlab, y = ylab, title = title, color = color_lab,
      fill = color_lab, shape = color_lab
    ) +
    theme_bio3700() +
    ggplot2::theme(legend.position = "top")
  return(p)
}
