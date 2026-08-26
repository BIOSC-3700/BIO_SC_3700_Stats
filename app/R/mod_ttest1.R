# One-sample t-test tab.
#
# Also hosts the "difference between two columns" case. That is
# arithmetically the same test — a one-sample t-test on the paired
# differences against a null of zero — and students meet it in that
# form, so it belongs here rather than as a second paired option.

mod_ttest1_ui <- function(id) {
  ns <- shiny::NS(id)
  out <- bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 360,
      shiny::radioButtons(
        ns("mode"), "What are you testing?",
        choices = c(
          "One group against a fixed value" = "single",
          "The difference between two columns" = "diff"
        ),
        selected = "single"
      ),
      shiny::uiOutput(ns("setup")),
      shiny::hr(),
      shiny::tags$details(
        shiny::tags$summary("Plot options"),
        plot_controls(ns, style_choices = FALSE)
      )
    ),
    shiny::uiOutput(ns("body"))
  )
  return(out)
}

mod_ttest1_server <- function(id, data,
                              show_plots = TRUE) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    confirmed <- shiny::reactiveVal(FALSE)
    shiny::observeEvent(data$raw(), {
      confirmed(FALSE)
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$run_analysis, {
      confirmed(TRUE)
    })

    numeric_columns <- shiny::reactive({
      raw <- data$raw()
      if (is.null(raw)) {
        return(character(0))
      }
      return(names(raw)[vapply(raw, is.numeric, logical(1))])
    })

    output$setup <- shiny::renderUI({
      if (identical(input$mode, "diff")) {
        cols <- numeric_columns()
        if (length(cols) < 2) {
          return(shiny::p(class = "hint", paste(
            "This mode needs two numeric columns in your data, for",
            "example a 'before' column and an 'after' column."
          )))
        }
        return(shiny::tagList(
          shiny::selectInput(
            ns("col1"), "Subtract this column", choices = cols,
            selected = cols[2]
          ),
          shiny::selectInput(
            ns("col2"), "From this column", choices = cols,
            selected = cols[1]
          ),
          shiny::p(class = "hint", paste(
            "Rows are paired top to bottom, so each row must be the",
            "same subject measured twice."
          )),
          common_controls(ns, mu_label = "Hypothesized difference (μ₀)")
        ))
      }
      tidy <- data$tidy()
      if (is.null(tidy)) {
        return(shiny::p(
          class = "hint", "Load your measurements on the Data tab."
        ))
      }
      groups <- levels(tidy$group)
      return(shiny::tagList(
        shiny::selectInput(
          ns("group"), "Which group?",
          choices = c("All data combined" = "__all__", groups),
          selected = if (length(groups) == 1) groups[1] else "__all__"
        ),
        common_controls(ns, mu_label = "Hypothesized mean (μ₀)")
      ))
    })

    conf_level <- shiny::reactive({
      value <- input$conf
      if (is.null(value) || is.na(value) || value <= 0.5 || value >= 1) {
        return(0.95)
      }
      return(value)
    })

    mu0 <- shiny::reactive({
      value <- input$mu
      if (is.null(value) || is.na(value)) {
        return(0)
      }
      return(value)
    })

    # The values under test, plus a label for the plot and tables.
    sample_values <- shiny::reactive({
      if (identical(input$mode, "diff")) {
        raw <- data$raw()
        shiny::req(raw, input$col1, input$col2)
        shiny::req(input$col1 %in% names(raw), input$col2 %in% names(raw))
        values <- raw[[input$col2]] - raw[[input$col1]]
        return(list(
          values = values[!is.na(values)],
          label = glue::glue("{input$col2} − {input$col1}")
        ))
      }
      tidy <- data$tidy()
      shiny::req(tidy)
      chosen <- input$group %||% "__all__"
      if (identical(chosen, "__all__")) {
        return(list(values = tidy$value, label = "All data"))
      }
      shiny::req(chosen %in% levels(tidy$group))
      return(list(
        values = tidy$value[tidy$group == chosen], label = chosen
      ))
    })

    # A one-column tidy frame so the shared assumption checks and QQ
    # plot work unchanged.
    check_data <- shiny::reactive({
      sample <- sample_values()
      return(tibble::tibble(
        value = sample$values,
        group = factor(sample$label)
      ))
    })

    problems <- shiny::reactive({
      if (is.null(data$tidy()) && is.null(data$raw())) {
        return(NULL)
      }
      out <- character(0)
      if (identical(input$mode, "diff")) {
        if (length(numeric_columns()) < 2) {
          return(paste(
            "Testing a difference needs two numeric columns. Load data",
            "with a 'before' and an 'after' column, or switch to the",
            "one-group mode."
          ))
        }
        if (identical(input$col1, input$col2)) {
          return("Pick two different columns.")
        }
      }
      values <- sample_values()$values
      if (length(values) < 2) {
        out <- c(out, glue::glue(
          "A t-test needs at least 2 values, but this selection has ",
          "{length(values)}."
        ))
        return(out)
      }
      if (stats::var(values) == 0) {
        out <- c(out, paste(
          "Every value in this selection is identical, so there is no",
          "variation to test against."
        ))
      }
      return(out)
    })

    result <- shiny::reactive({
      shiny::req(length(problems()) == 0)
      out <- stats::t.test(
        sample_values()$values, mu = mu0(),
        alternative = input$alt %||% "two.sided",
        conf.level = conf_level()
      )
      return(out)
    })

    output$verdict <- shiny::renderUI({
      res <- result()
      sample <- sample_values()
      headline <- glue::glue(
        "{sample$label}: mean {fmt_num(unname(res$estimate))} ",
        "(n = {length(sample$values)}) against μ₀ = {fmt_num(mu0())}. ",
        "t({fmt_num(res$parameter, 0)}) = {fmt_num(res$statistic, 3)}, ",
        "{fmt_p_inline(res$p.value)}."
      )
      return(verdict_box(res$p.value, 1 - conf_level(), headline))
    })

    output$stats <- shiny::renderUI({
      res <- result()
      values <- sample_values()$values
      keys <- c(
        "n", "Sample mean", "Standard deviation", "Standard error",
        "Hypothesized mean (μ₀)",
        glue::glue("{round(conf_level() * 100)}% confidence interval"),
        "t statistic", "Degrees of freedom", "p-value"
      )
      display <- c(
        as.character(length(values)),
        fmt_num(mean(values)),
        fmt_num(stats::sd(values)),
        fmt_num(se_mean(values)),
        fmt_num(mu0()),
        fmt_ci(res$conf.int[1], res$conf.int[2]),
        fmt_num(res$statistic, 3),
        fmt_num(res$parameter, 0),
        fmt_p(res$p.value)
      )
      return(kv_table(keys, display))
    })

    output$assumptions <- shiny::renderUI({
      shiny::req(length(problems()) == 0)
      checks <- list(
        check_normality(check_data()),
        check_outliers(check_data()),
        check_independence()
      )
      return(shiny::tagList(lapply(checks, check_block)))
    })

    output$qq <- shiny::renderPlot({
      shiny::req(length(problems()) == 0)
      return(plot_qq(check_data()))
    })

    the_plot <- shiny::reactive({
      res <- result()
      sample <- sample_values()
      labs <- data$labels()
      title <- label_or(input$plot_title, NULL)
      default_x <- if (identical(input$mode, "diff")) {
        as.character(sample$label)
      } else {
        labs$value
      }
      xlab <- label_or(input$plot_xlab, default_x)
      return(plot_one_sample(
        values = sample$values, mu0 = mu0(),
        ci_low = res$conf.int[1], ci_high = res$conf.int[2],
        mean_val = unname(res$estimate), title = title, xlab = xlab,
        conf_level = conf_level()
      ))
    })

    output$plot <- shiny::renderPlot({
      shiny::req(length(problems()) == 0)
      return(the_plot())
    })

    output$download_plot <- plot_download_handler(
      the_plot, "one-sample-t"
    )

    code_text <- shiny::reactive({
      alt <- input$alt %||% "two.sided"
      cl <- conf_level()
      if (identical(input$mode, "diff")) {
        c1 <- input$col1
        c2 <- input$col2
        shiny::req(c1, c2)
        return(glue::glue(
          "t.test(my_data${c2} - my_data${c1},\n",
          "       mu = {mu0()},\n",
          '       alternative = "{alt}",\n',
          "       conf.level = {cl})"
        ))
      }
      sample <- sample_values()
      return(glue::glue(
        "t.test(x,\n",
        "       mu = {mu0()},\n",
        '       alternative = "{alt}",\n',
        "       conf.level = {cl})"
      ))
    })

    output$body <- shiny::renderUI({
      intro <- test_intro(
        title = "One-sample t-test",
        description = paste(
          "A one-sample t-test determines whether",
          "the mean of a sample is significantly",
          "different from a known or hypothesized",
          "value. It can also test the mean",
          "difference between paired measurements."
        ),
        hypotheses = shiny::tagList(
          shiny::tags$dt("H\u2080 (null)"),
          shiny::tags$dd(paste(
            "\u03bc = \u03bc\u2080 \u2014",
            "the population mean equals the",
            "hypothesized value."
          )),
          shiny::tags$dt("H\u2090 (alternative)"),
          shiny::tags$dd(paste(
            "\u03bc \u2260 \u03bc\u2080 \u2014",
            "the population mean differs from the",
            "hypothesized value."
          ))
        ),
        ns = ns, confirmed = confirmed()
      )
      if (is.null(data$raw())) {
        return(shiny::tagList(
          intro, no_data_panel("a one-sample t-test")
        ))
      }
      if (!confirmed()) {
        return(intro)
      }
      probs <- problems()
      if (length(probs) > 0) {
        return(shiny::tagList(
          intro, cannot_run_panel(probs)
        ))
      }
      plot_card <- if (show_plots) {
        bslib::card(
          bslib::card_header("Plot"),
          plot_panel(ns, height = "300px")
        )
      }
      widths <- if (show_plots) c(5, 7) else 12
      return(shiny::tagList(
        intro,
        shiny::uiOutput(ns("verdict")),
        bslib::layout_columns(
          col_widths = widths,
          bslib::card(
            bslib::card_header("Result"),
            shiny::uiOutput(ns("stats"))
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

# μ₀, the alternative, and the confidence level are identical in both
# modes of this tab.
common_controls <- function(ns, mu_label) {
  out <- shiny::tagList(
    shiny::numericInput(ns("mu"), mu_label, value = 0, step = 0.1),
    shiny::selectInput(
      ns("alt"), "Alternative hypothesis",
      choices = c(
        "The mean differs from μ₀ (two-sided)" = "two.sided",
        "The mean is smaller than μ₀" = "less",
        "The mean is larger than μ₀" = "greater"
      ),
      selected = "two.sided"
    ),
    shiny::numericInput(
      ns("conf"), "Confidence level", value = 0.95,
      min = 0.5, max = 0.999, step = 0.01
    )
  )
  return(out)
}
