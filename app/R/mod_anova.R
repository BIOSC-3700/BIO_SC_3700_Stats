# ANOVA tab, with optional Tukey HSD.
#
# When Levene's test flags unequal variances the tab additionally
# reports Welch's ANOVA (stats::oneway.test) and says which of the two
# to read, rather than leaving a result whose assumptions it has just
# told are violated.

mod_anova_ui <- function(id) {
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

mod_anova_server <- function(id, data, show_plots = TRUE) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    confirmed <- shiny::reactiveVal(FALSE)
    shiny::observeEvent(
      data$tidy(),
      {
        confirmed(FALSE)
      },
      ignoreInit = TRUE
    )
    shiny::observeEvent(input$run_analysis, {
      confirmed(TRUE)
    })

    output$setup <- shiny::renderUI({
      tidy <- data$tidy()
      if (is.null(tidy)) {
        return(shiny::p(
          class = "hint",
          "Load your measurements on the Data tab."
        ))
      }
      return(shiny::tagList(
        shiny::p(
          class = "hint",
          glue::glue(
            "Comparing {nlevels(tidy$group)} groups: ",
            "{paste(levels(tidy$group), collapse = ', ')}."
          )
        ),
        shiny::checkboxInput(
          ns("tukey"),
          "Run Tukey's HSD post-hoc test",
          value = TRUE
        ),
        shiny::p(
          class = "hint",
          paste(
            "Tukey tells you which pairs of groups differ. It only makes",
            "sense to look at it when the overall ANOVA is significant."
          )
        ),
        shiny::numericInput(
          ns("conf"),
          "Confidence level",
          value = 0.95,
          min = 0.5,
          max = 0.999,
          step = 0.01
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

    problems <- shiny::reactive({
      tidy <- data$tidy()
      if (is.null(tidy)) {
        return(NULL)
      }
      out <- character(0)
      n_groups <- nlevels(tidy$group)
      if (n_groups < 3) {
        out <- c(
          out,
          glue::glue(
            "ANOVA compares three or more groups, but this data has ",
            "{n_groups}. With two groups, use the two-sample t-test tab ",
            "instead — for two groups the two tests are equivalent."
          )
        )
        return(out)
      }
      counts <- table(tidy$group)
      thin <- names(counts)[counts < 2]
      if (length(thin) > 0) {
        out <- c(
          out,
          glue::glue(
            "Every group needs at least 2 values. Too few in: ",
            "{paste(thin, collapse = ', ')}."
          )
        )
      }
      return(out)
    })

    fit <- shiny::reactive({
      shiny::req(length(problems()) == 0)
      tidy <- data$tidy()
      return(stats::aov(value ~ group, data = tidy))
    })

    anova_row <- shiny::reactive({
      tab <- summary(fit())[[1]]
      return(list(
        df1 = tab[1, "Df"],
        df2 = tab[2, "Df"],
        f = tab[1, "F value"],
        p = tab[1, "Pr(>F)"],
        table = tab
      ))
    })

    variance_check <- shiny::reactive({
      return(check_variance(data$tidy(), welch_used = FALSE))
    })

    welch_result <- shiny::reactive({
      shiny::req(length(problems()) == 0)
      out <- try(
        stats::oneway.test(
          value ~ group,
          data = data$tidy(),
          var.equal = FALSE
        ),
        silent = TRUE
      )
      if (inherits(out, "try-error")) {
        return(NULL)
      }
      return(out)
    })

    # TukeyHSD names its rows "B-A", which is ambiguous when a group
    # name itself contains a hyphen. combn() reproduces the same order
    # from the factor levels, so labels are built rather than parsed.
    tukey_table <- shiny::reactive({
      shiny::req(length(problems()) == 0)
      tidy <- data$tidy()
      raw <- as.data.frame(
        stats::TukeyHSD(fit(), conf.level = conf_level())$group
      )
      pairs <- utils::combn(levels(tidy$group), 2)
      out <- tibble::tibble(
        comparison = paste(pairs[2, ], "−", pairs[1, ]),
        diff = raw$diff,
        lwr = raw$lwr,
        upr = raw$upr,
        p_adj = raw[["p adj"]]
      )
      return(out)
    })

    output$verdict <- shiny::renderUI({
      res <- anova_row()
      tidy <- data$tidy()
      headline <- glue::glue(
        "One-way ANOVA across {nlevels(tidy$group)} groups: ",
        "F({res$df1}, {res$df2}) = {fmt_num(res$f, 3)}, ",
        "{fmt_p_inline(res$p)}."
      )
      return(verdict_box(res$p, 1 - conf_level(), headline))
    })

    output$anova_table <- shiny::renderTable(
      {
        tab <- anova_row()$table
        labs <- data$labels()
        out <- tibble::tibble(
          Source = c(labs$group, "Residuals"),
          df = as.integer(tab$Df),
          `Sum Sq` = fmt_num(tab$`Sum Sq`, 2),
          `Mean Sq` = fmt_num(tab$`Mean Sq`, 2),
          `F` = c(fmt_num(tab$`F value`[1], 3), "—"),
          `p` = c(fmt_p(tab$`Pr(>F)`[1]), "—")
        )
        return(out)
      },
      striped = TRUE,
      spacing = "xs",
      align = "lrrrrr"
    )

    output$welch <- shiny::renderUI({
      check <- variance_check()
      if (!identical(check$status, "warn")) {
        return(NULL)
      }
      res <- welch_result()
      if (is.null(res)) {
        return(NULL)
      }
      out <- shiny::div(
        class = "alert-box alert-warn",
        shiny::strong("Variances differ, so also read this. "),
        glue::glue(
          "Welch's ANOVA does not assume equal variances: ",
          "F({fmt_num(res$parameter[1], 0)}, ",
          "{fmt_num(res$parameter[2], 1)}) = ",
          "{fmt_num(res$statistic, 3)}, {fmt_p_inline(res$p.value)}. ",
          "When the two ANOVAs disagree, trust this one."
        )
      )
      return(out)
    })

    output$summary <- shiny::renderTable(
      {
        summary_table(group_summary(data$tidy()), compact = TRUE)
      },
      striped = TRUE,
      spacing = "xs",
      align = "lrrrr"
    )

    output$tukey_table <- shiny::renderTable(
      {
        tk <- tukey_table()
        out <- tibble::tibble(
          Comparison = tk$comparison,
          Difference = fmt_num(tk$diff),
          Lower = fmt_num(tk$lwr),
          Upper = fmt_num(tk$upr),
          `Adjusted p` = fmt_p(tk$p_adj),
          Significant = ifelse(
            tk$p_adj < 1 - conf_level(),
            "yes",
            "no"
          )
        )
        return(out)
      },
      striped = TRUE,
      spacing = "xs",
      align = "lrrrrc"
    )

    output$assumptions <- shiny::renderUI({
      shiny::req(length(problems()) == 0)
      tidy <- data$tidy()
      checks <- list(
        check_normality(tidy),
        variance_check(),
        check_outliers(tidy),
        check_independence()
      )
      return(shiny::tagList(lapply(checks, check_block)))
    })

    output$qq <- shiny::renderPlot({
      shiny::req(length(problems()) == 0)
      return(plot_qq(data$tidy()))
    })

    the_plot <- shiny::reactive({
      tidy <- data$tidy()
      labs <- data$labels()
      title <- label_or(input$plot_title, NULL)
      xlab <- label_or(input$plot_xlab, labs$group)
      ylab <- label_or(input$plot_ylab, labs$value)
      return(plot_groups(
        tidy,
        style = input$plot_style %||% "box",
        title = title,
        xlab = xlab,
        ylab = ylab
      ))
    })

    the_tukey_plot <- shiny::reactive({
      labs <- data$labels()
      ylab <- glue::glue(
        "Difference in {label_or(input$plot_ylab, labs$value)}"
      )
      return(plot_tukey(
        tukey_table(),
        conf_level = conf_level(),
        title = NULL,
        ylab = as.character(ylab)
      ))
    })

    output$plot <- shiny::renderPlot({
      shiny::req(length(problems()) == 0)
      return(the_plot())
    })

    output$tukey_plot <- shiny::renderPlot({
      shiny::req(length(problems()) == 0, isTRUE(input$tukey))
      return(the_tukey_plot())
    })

    output$download_plot <- plot_download_handler(
      the_plot,
      "anova"
    )
    output$download_tukey <- plot_download_handler(
      the_tukey_plot,
      "tukey-hsd"
    )

    code_text <- shiny::reactive({
      labs <- data$labels()
      shiny::req(labs)
      lines <- c(
        glue::glue(
          "model <- aov({labs$value} ~ {labs$group},",
          " data = my_data)"
        ),
        "summary(model)"
      )
      if (isTRUE(input$tukey)) {
        lines <- c(
          lines,
          glue::glue(
            "TukeyHSD(model,",
            " conf.level = {conf_level()})"
          )
        )
      }
      return(paste(lines, collapse = "\n"))
    })

    output$body <- shiny::renderUI({
      intro <- test_intro(
        title = "One-way ANOVA",
        description = paste(
          "A one-way analysis of variance (ANOVA)",
          "tests whether the means of three or more",
          "independent groups are all equal. It",
          "extends the two-sample t-test to more",
          "than two groups."
        ),
        hypotheses = shiny::tagList(
          shiny::tags$dt("H\u2080 (null)"),
          shiny::tags$dd(paste(
            "\u03bc\u2081 = \u03bc\u2082 =",
            "\u2026 = \u03bc\u2096 \u2014",
            "all group means are equal."
          )),
          shiny::tags$dt("H\u2090 (alternative)"),
          shiny::tags$dd(paste(
            "At least one group mean differs",
            "from the others."
          ))
        ),
        ns = ns,
        confirmed = confirmed()
      )
      if (is.null(data$tidy())) {
        return(shiny::tagList(
          intro,
          no_data_panel("an ANOVA")
        ))
      }
      if (!confirmed()) {
        return(intro)
      }
      probs <- problems()
      if (length(probs) > 0) {
        return(shiny::tagList(
          intro,
          cannot_run_panel(probs)
        ))
      }
      tukey_section <- if (isTRUE(input$tukey)) {
        tukey_content <- if (show_plots) {
          bslib::layout_columns(
            col_widths = c(6, 6),
            shiny::div(
              class = "table-scroll",
              shiny::tableOutput(ns("tukey_table"))
            ),
            plot_panel(
              ns,
              height = "360px",
              plot_id = "tukey_plot",
              dl_id = "download_tukey"
            )
          )
        } else {
          shiny::div(
            class = "table-scroll",
            shiny::tableOutput(ns("tukey_table"))
          )
        }
        bslib::card(
          bslib::card_header(
            "Tukey's HSD \u2014 which pairs differ?"
          ),
          tukey_content
        )
      } else {
        NULL
      }
      plot_card <- if (show_plots) {
        bslib::card(
          bslib::card_header("Plot"),
          plot_panel(ns)
        )
      }
      widths <- if (show_plots) c(6, 6) else 12
      return(shiny::tagList(
        intro,
        shiny::uiOutput(ns("verdict")),
        shiny::uiOutput(ns("welch")),
        bslib::layout_columns(
          col_widths = widths,
          bslib::card(
            bslib::card_header("ANOVA table"),
            shiny::div(
              class = "table-scroll",
              shiny::tableOutput(ns("anova_table"))
            ),
            shiny::tags$h6("Group summary"),
            shiny::div(
              class = "table-scroll",
              shiny::tableOutput(ns("summary"))
            )
          ),
          plot_card
        ),
        tukey_section,
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
