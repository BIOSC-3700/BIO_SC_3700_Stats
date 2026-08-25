# Two-sample t-test tab.
#
# Welch is the default rather than the pooled-variance test: it costs
# almost nothing when variances really are equal and it is the correct
# test when they are not, so "assume equal variances" is an opt-in
# checkbox instead of a default.

mod_ttest2_ui <- function(id) {
  ns <- shiny::NS(id)
  out <- bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 360,
      shiny::uiOutput(ns("setup")),
      shiny::hr(),
      shiny::tags$details(
        shiny::tags$summary("Plot options"),
        plot_controls(ns)
      )
    ),
    shiny::uiOutput(ns("body"))
  )
  return(out)
}

mod_ttest2_server <- function(id, data,
                              show_plots = TRUE) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    group_levels <- shiny::reactive({
      tidy <- data$tidy()
      if (is.null(tidy)) {
        return(character(0))
      }
      return(levels(tidy$group))
    })

    output$setup <- shiny::renderUI({
      groups <- group_levels()
      if (length(groups) < 2) {
        return(shiny::p(
          class = "hint",
          "Load data with at least two groups on the Data tab."
        ))
      }
      return(shiny::tagList(
        shiny::selectInput(
          ns("g1"), "First group", choices = groups, selected = groups[1]
        ),
        shiny::selectInput(
          ns("g2"), "Second group", choices = groups, selected = groups[2]
        ),
        shiny::checkboxInput(
          ns("paired"), "Paired samples (same subject measured twice)",
          value = FALSE
        ),
        shiny::checkboxInput(
          ns("var_equal"), "Assume equal variances", value = FALSE
        ),
        shiny::p(
          class = "hint",
          "Leave equal variances unchecked unless you have a reason. ",
          "The default is Welch's t-test, which does not require them."
        ),
        shiny::selectInput(
          ns("alt"), "Alternative hypothesis",
          choices = c(
            "The means differ (two-sided)" = "two.sided",
            "First group is smaller" = "less",
            "First group is larger" = "greater"
          ),
          selected = "two.sided"
        ),
        shiny::numericInput(
          ns("conf"), "Confidence level", value = 0.95,
          min = 0.5, max = 0.999, step = 0.01
        )
      ))
    })

    conf_level <- shiny::reactive({
      value <- input$conf
      if (is.null(value) || is.na(value) || value <= 0.5 || value >= 1) {
        return(0.95)
      }
      return(value)
    })

    # Subset to the two chosen groups, ordered so that "first group" in
    # the sidebar really is the first argument to t.test().
    pair_data <- shiny::reactive({
      tidy <- data$tidy()
      shiny::req(tidy, input$g1, input$g2)
      chosen <- c(input$g1, input$g2)
      out <- tidy |>
        dplyr::filter(.data$group %in% chosen) |>
        dplyr::mutate(group = factor(.data$group, levels = chosen))
      return(out)
    })

    problems <- shiny::reactive({
      tidy <- data$tidy()
      if (is.null(tidy)) {
        return(NULL)
      }
      out <- character(0)
      if (nlevels(tidy$group) < 2) {
        out <- c(out, paste(
          "A two-sample t-test needs two groups. This data has only",
          "one. Use the one-sample tab instead."
        ))
        return(out)
      }
      if (is.null(input$g1) || is.null(input$g2)) {
        return(character(0))
      }
      if (identical(input$g1, input$g2)) {
        out <- c(out, "Pick two different groups.")
        return(out)
      }
      counts <- table(pair_data()$group)
      thin <- names(counts)[counts < 2]
      if (length(thin) > 0) {
        out <- c(out, glue::glue(
          "Each group needs at least 2 values. Too few in: ",
          "{paste(thin, collapse = ', ')}."
        ))
      }
      if (isTRUE(input$paired) && length(counts) == 2 &&
            counts[[1]] != counts[[2]]) {
        out <- c(out, glue::glue(
          "A paired test needs the same number of values in each group, ",
          "but there are {counts[[1]]} and {counts[[2]]}. Either the ",
          "data is not paired, or some rows are missing."
        ))
      }
      return(out)
    })

    result <- shiny::reactive({
      shiny::req(length(problems()) == 0)
      pd <- pair_data()
      x <- pd$value[pd$group == input$g1]
      y <- pd$value[pd$group == input$g2]
      if (isTRUE(input$paired)) {
        out <- stats::t.test(
          x, y, paired = TRUE, alternative = input$alt,
          conf.level = conf_level()
        )
      } else {
        out <- stats::t.test(
          x, y, paired = FALSE, var.equal = isTRUE(input$var_equal),
          alternative = input$alt, conf.level = conf_level()
        )
      }
      return(out)
    })

    test_name <- shiny::reactive({
      if (isTRUE(input$paired)) {
        return("Paired t-test")
      }
      if (isTRUE(input$var_equal)) {
        return("Two-sample t-test (pooled variance)")
      }
      return("Welch's two-sample t-test")
    })

    # ---- results -------------------------------------------------------
    output$verdict <- shiny::renderUI({
      res <- result()
      pd <- pair_data()
      stats_by_group <- group_summary(pd)
      m1 <- stats_by_group$mean[stats_by_group$group == input$g1]
      m2 <- stats_by_group$mean[stats_by_group$group == input$g2]
      n1 <- stats_by_group$n[stats_by_group$group == input$g1]
      n2 <- stats_by_group$n[stats_by_group$group == input$g2]
      headline <- glue::glue(
        "{input$g1} (mean {fmt_num(m1)}, n = {n1}) vs. {input$g2} ",
        "(mean {fmt_num(m2)}, n = {n2}): {test_name()} gives ",
        "t({fmt_num(res$parameter, 1)}) = {fmt_num(res$statistic, 3)}, ",
        "{fmt_p_inline(res$p.value)}."
      )
      return(verdict_box(res$p.value, 1 - conf_level(), headline))
    })

    output$stats <- shiny::renderUI({
      res <- result()
      diff_label <- if (isTRUE(input$paired)) {
        "Mean of the differences"
      } else {
        "Difference in means"
      }
      estimate <- if (isTRUE(input$paired)) {
        unname(res$estimate)
      } else {
        unname(res$estimate[1] - res$estimate[2])
      }
      keys <- c(
        "Test", diff_label,
        glue::glue("{round(conf_level() * 100)}% confidence interval"),
        "t statistic", "Degrees of freedom", "p-value"
      )
      values <- c(
        test_name(),
        fmt_num(estimate),
        fmt_ci(res$conf.int[1], res$conf.int[2]),
        fmt_num(res$statistic, 3),
        fmt_num(res$parameter, 2),
        fmt_p(res$p.value)
      )
      return(kv_table(keys, values))
    })

    output$summary <- shiny::renderTable(
      {
        summary_table(group_summary(pair_data()), compact = TRUE)
      },
      striped = TRUE, spacing = "xs", align = "lrrrr"
    )

    output$assumptions <- shiny::renderUI({
      pd <- pair_data()
      shiny::req(length(problems()) == 0)
      checks <- list(check_normality(pd))
      if (!isTRUE(input$paired)) {
        checks <- c(
          checks,
          list(check_variance(pd, welch_used = !isTRUE(input$var_equal)))
        )
      }
      checks <- c(checks, list(check_outliers(pd), check_independence()))
      return(shiny::tagList(lapply(checks, check_block)))
    })

    output$qq <- shiny::renderPlot({
      shiny::req(length(problems()) == 0)
      return(plot_qq(pair_data()))
    })

    the_plot <- shiny::reactive({
      pd <- pair_data()
      labs <- data$labels()
      title <- label_or(input$plot_title, NULL)
      xlab <- label_or(input$plot_xlab, labs$group)
      ylab <- label_or(input$plot_ylab, labs$value)
      if (isTRUE(input$paired)) {
        paired_long <- pd |>
          dplyr::group_by(.data$group) |>
          dplyr::mutate(pair_id = dplyr::row_number()) |>
          dplyr::ungroup()
        return(plot_paired(paired_long, title, xlab, ylab))
      }
      return(plot_groups(
        pd, style = input$plot_style %||% "box",
        title = title, xlab = xlab, ylab = ylab
      ))
    })

    output$plot <- shiny::renderPlot({
      shiny::req(length(problems()) == 0)
      return(the_plot())
    })

    output$download_plot <- plot_download_handler(
      the_plot, "two-sample-t"
    )

    code_text <- shiny::reactive({
      labs <- data$labels()
      shiny::req(labs)
      if (isTRUE(input$paired)) {
        return(glue::glue(
          "t.test({labs$value} ~ {labs$group},\n",
          "       data = my_data,\n",
          "       paired = TRUE,\n",
          '       alternative = "{input$alt}",\n',
          "       conf.level = {conf_level()})"
        ))
      }
      return(glue::glue(
        "t.test({labs$value} ~ {labs$group},\n",
        "       data = my_data,\n",
        "       var.equal = ",
        "{toupper(isTRUE(input$var_equal))},\n",
        '       alternative = "{input$alt}",\n',
        "       conf.level = {conf_level()})"
      ))
    })

    output$body <- shiny::renderUI({
      if (is.null(data$tidy())) {
        return(no_data_panel("a two-sample t-test"))
      }
      probs <- problems()
      if (length(probs) > 0) {
        return(cannot_run_panel(probs))
      }
      plot_card <- if (show_plots) {
        bslib::card(
          bslib::card_header("Plot"),
          plot_panel(ns)
        )
      }
      widths <- if (show_plots) c(5, 7) else 12
      return(shiny::tagList(
        shiny::uiOutput(ns("verdict")),
        bslib::layout_columns(
          col_widths = widths,
          bslib::card(
            bslib::card_header("Result"),
            shiny::uiOutput(ns("stats")),
            shiny::tags$h6("Group summary"),
            shiny::div(class = "table-scroll",
                       shiny::tableOutput(ns("summary")))
          ),
          plot_card
        ),
        bslib::accordion(
          open = TRUE,
          bslib::accordion_panel(
            "Assumptions — check these before trusting the p-value",
            shiny::uiOutput(ns("assumptions")),
            shiny::tags$h6("QQ plot for the normality check"),
            shiny::plotOutput(ns("qq"), height = "260px")
          )
        ),
        code_accordion(code_text())
      ))
    })
  })
}
