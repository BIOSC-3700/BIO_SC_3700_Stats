# Two-way factorial ANOVA with an interaction term.
#
# Two things drive the design of this tab:
#
# 1. The interaction comes first. If the effect of one factor depends
#    on the other, the main effects are averages over a factor they
#    interact with, and reading them on their own misleads. So the
#    verdict leads with the interaction and the post-hoc offered
#    changes depending on whether it is significant.
#
# 2. Sums of squares are Type I, matching summary(aov()) and textbook
#    worked examples. Type I is computed sequentially, so on an
#    unbalanced design the table depends on which factor is entered
#    first. Balanced designs are unaffected, so the tab checks balance
#    and says so plainly when it is missing.
#
# Column pickers live here rather than on the Data tab: the shared
# tidy() carries a single grouping factor, and a factorial design needs
# two. mod_ttest1.R reads data$raw() the same way for its paired mode.

mod_anova2_ui <- function(id) {
  ns <- shiny::NS(id)
  out <- bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 360,
      shiny::uiOutput(ns("setup")),
      shiny::hr(),
      shiny::tags$details(
        shiny::tags$summary("Plot options"),
        shiny::checkboxInput(
          ns("swap_axes"),
          "Swap which factor is on the x-axis", value = FALSE
        ),
        plot_controls(ns)
      )
    ),
    shiny::uiOutput(ns("body"))
  )
  return(out)
}

mod_anova2_server <- function(id, data,
                              show_plots = TRUE) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    all_columns <- shiny::reactive({
      raw <- data$raw()
      if (is.null(raw)) {
        return(character(0))
      }
      return(names(raw))
    })

    numeric_columns <- shiny::reactive({
      raw <- data$raw()
      if (is.null(raw)) {
        return(character(0))
      }
      return(names(raw)[vapply(raw, is.numeric, logical(1))])
    })

    output$setup <- shiny::renderUI({
      raw <- data$raw()
      if (is.null(raw)) {
        return(shiny::p(
          class = "hint", "Load your measurements on the Data tab."
        ))
      }
      cols <- all_columns()
      nums <- numeric_columns()
      if (length(nums) == 0 || length(cols) < 3) {
        return(shiny::p(class = "hint", paste(
          "A factorial ANOVA needs at least three columns: one",
          "measurement and two grouping factors."
        )))
      }
      labs <- data$labels()
      # Start from whatever the student already told the Data tab, so
      # they only have to name the factor that is genuinely new.
      response_default <- if (labs$value %in% nums) labs$value else nums[1]
      a_default <- if (labs$group %in% setdiff(cols, response_default)) {
        labs$group
      } else {
        setdiff(cols, c(response_default, nums))[1] %||%
          setdiff(cols, response_default)[1]
      }
      b_choices <- setdiff(cols, c(response_default, a_default))
      b_default <- setdiff(b_choices, nums)[1] %||% b_choices[1]
      return(shiny::tagList(
        shiny::selectInput(
          ns("response"), "Measurement column", choices = nums,
          selected = response_default
        ),
        shiny::selectInput(
          ns("fac_a"), "Factor A", choices = cols, selected = a_default
        ),
        shiny::selectInput(
          ns("fac_b"), "Factor B", choices = cols, selected = b_default
        ),
        shiny::p(class = "hint", paste(
          "The two factors are the things you deliberately varied.",
          "The measurement is what you recorded."
        )),
        shiny::checkboxInput(
          ns("tukey"), "Run post-hoc comparisons", value = TRUE
        ),
        shiny::radioButtons(
          ns("simple_dir"), "Simple effects to show",
          choices = c(
            "Factor A within each level of B" = "a_within_b",
            "Factor B within each level of A" = "b_within_a"
          ),
          selected = "a_within_b"
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

    alpha <- shiny::reactive({
      return(1 - conf_level())
    })

    col_names <- shiny::reactive({
      return(list(
        response = input$response, a = input$fac_a, b = input$fac_b
      ))
    })

    # value / a / b / cell, with the same non-numeric reporting the
    # Data tab uses so a column of "12.4 g" strings explains itself.
    cells_result <- shiny::reactive({
      raw <- data$raw()
      cn <- col_names()
      shiny::req(raw, cn$response, cn$a, cn$b)
      if (!all(c(cn$response, cn$a, cn$b) %in% names(raw))) {
        return(NULL)
      }
      problems <- character(0)
      values <- raw[[cn$response]]
      before <- !is.na(values)
      values <- suppressWarnings(as.numeric(as.character(values)))
      broke <- sum(before & is.na(values))
      if (broke > 0) {
        problems <- c(problems, glue::glue(
          "{broke} value{ifelse(broke == 1, '', 's')} in ",
          "{cn$response} could not be read as a number and ",
          "{ifelse(broke == 1, 'was', 'were')} dropped."
        ))
      }
      out <- tibble::tibble(
        value = values,
        a = as.character(raw[[cn$a]]),
        b = as.character(raw[[cn$b]])
      )
      out <- out[!is.na(out$value) & !is.na(out$a) & !is.na(out$b), ]
      if (nrow(out) == 0) {
        return(list(data = NULL, problems = c(
          problems, "No usable rows are left after cleaning."
        )))
      }
      out$a <- factor(out$a, levels = sort(unique(out$a)))
      out$b <- factor(out$b, levels = sort(unique(out$b)))
      out$cell <- interaction(out$a, out$b, sep = " / ", drop = TRUE)
      return(list(data = out, problems = problems))
    })

    cells <- shiny::reactive({
      res <- cells_result()
      if (is.null(res)) {
        return(NULL)
      }
      return(res$data)
    })

    counts <- shiny::reactive({
      cd <- cells()
      shiny::req(cd)
      return(table(cd$a, cd$b))
    })

    is_balanced <- shiny::reactive({
      tab <- counts()
      return(length(unique(as.vector(tab))) == 1)
    })

    problems <- shiny::reactive({
      raw <- data$raw()
      if (is.null(raw)) {
        return(NULL)
      }
      cn <- col_names()
      if (is.null(cn$response) || is.null(cn$a) || is.null(cn$b)) {
        return(character(0))
      }
      chosen <- c(cn$response, cn$a, cn$b)
      if (anyDuplicated(chosen) > 0) {
        return(paste(
          "The measurement column and the two factors must be three",
          "different columns."
        ))
      }
      res <- cells_result()
      if (is.null(res)) {
        return(character(0))
      }
      if (is.null(res$data)) {
        return(res$problems)
      }
      cd <- res$data
      out <- character(0)
      if (nlevels(cd$a) < 2) {
        out <- c(out, glue::glue(
          "{cn$a} has only one level, so it cannot be a factor. Pick a ",
          "column with at least two groups."
        ))
      }
      if (nlevels(cd$b) < 2) {
        out <- c(out, glue::glue(
          "{cn$b} has only one level, so it cannot be a factor. Pick a ",
          "column with at least two groups."
        ))
      }
      if (length(out) > 0) {
        return(out)
      }
      tab <- table(cd$a, cd$b)
      # An empty cell makes the interaction inestimable and aov()
      # quietly returns NA rather than complaining.
      if (any(tab == 0)) {
        empty <- which(tab == 0, arr.ind = TRUE)
        named <- paste(
          rownames(tab)[empty[, 1]], "/", colnames(tab)[empty[, 2]]
        )
        out <- c(out, glue::glue(
          "Every combination of the two factors needs at least one ",
          "observation, but these are empty: ",
          "{paste(named, collapse = ', ')}. A factorial ANOVA cannot ",
          "estimate an interaction for a combination you did not ",
          "measure."
        ))
        return(out)
      }
      if (any(tab < 2)) {
        thin <- which(tab < 2, arr.ind = TRUE)
        named <- paste(
          rownames(tab)[thin[, 1]], "/", colnames(tab)[thin[, 2]]
        )
        out <- c(out, glue::glue(
          "Every combination needs at least 2 observations to leave ",
          "any variation for the interaction to be measured against. ",
          "Only one in: {paste(named, collapse = ', ')}."
        ))
      }
      return(out)
    })

    ready <- shiny::reactive({
      probs <- problems()
      return(!is.null(probs) && length(probs) == 0 && !is.null(cells()))
    })

    fit <- shiny::reactive({
      shiny::req(ready())
      return(stats::aov(value ~ a * b, data = cells()))
    })

    # The saturated two-way model and a one-way model on the cells span
    # the same column space, so this has identical residual SS and df
    # and lets the cell-level Tukey reuse the one-way machinery.
    cell_fit <- shiny::reactive({
      shiny::req(ready())
      return(stats::aov(value ~ cell, data = cells()))
    })

    anova_terms <- shiny::reactive({
      tab <- summary(fit())[[1]]
      return(list(
        table = tab,
        df_resid = tab[4, "Df"],
        f = tab[1:3, "F value"],
        p = tab[1:3, "Pr(>F)"],
        df = tab[1:3, "Df"]
      ))
    })

    interaction_p <- shiny::reactive({
      return(anova_terms()$p[3])
    })

    interaction_sig <- shiny::reactive({
      p <- interaction_p()
      return(!is.na(p) && p < alpha())
    })

    # ---- post-hoc -------------------------------------------------------
    tukey_from <- function(model, which_term, level_names, conf) {
      raw <- as.data.frame(
        stats::TukeyHSD(model, which = which_term, conf.level = conf)[[1]]
      )
      pairs <- utils::combn(level_names, 2)
      out <- tibble::tibble(
        comparison = paste(pairs[2, ], "−", pairs[1, ]),
        diff = raw$diff, lwr = raw$lwr, upr = raw$upr,
        p_adj = raw[["p adj"]]
      )
      return(out)
    }

    tukey_cells <- shiny::reactive({
      shiny::req(ready())
      return(tukey_from(
        cell_fit(), "cell", levels(cells()$cell), conf_level()
      ))
    })

    tukey_main <- function(term) {
      cd <- cells()
      return(tukey_from(
        fit(), term, levels(cd[[term]]), conf_level()
      ))
    }

    simple_effects <- shiny::reactive({
      shiny::req(ready())
      cd <- cells()
      direction <- input$simple_dir %||% "a_within_b"
      if (identical(direction, "a_within_b")) {
        split_col <- "b"
        effect_col <- "a"
      } else {
        split_col <- "a"
        effect_col <- "b"
      }
      rows <- lapply(levels(cd[[split_col]]), function(lvl) {
        sub <- cd[cd[[split_col]] == lvl, ]
        sub[[effect_col]] <- droplevels(sub[[effect_col]])
        if (nlevels(sub[[effect_col]]) < 2 ||
              nrow(sub) <= nlevels(sub[[effect_col]])) {
          return(NULL)
        }
        model <- stats::aov(
          stats::reformulate(effect_col, "value"), data = sub
        )
        tab <- summary(model)[[1]]
        return(tibble::tibble(
          level = lvl, df1 = tab[1, "Df"], df2 = tab[2, "Df"],
          f = tab[1, "F value"], p = tab[1, "Pr(>F)"]
        ))
      })
      return(dplyr::bind_rows(rows))
    })

    # ---- rendered output ------------------------------------------------
    output$verdict <- shiny::renderUI({
      res <- anova_terms()
      cn <- col_names()
      if (interaction_sig()) {
        headline <- glue::glue(
          "The effect of {cn$a} depends on {cn$b}: interaction ",
          "F({res$df[3]}, {res$df_resid}) = {fmt_num(res$f[3], 3)}, ",
          "{fmt_p_inline(res$p[3])}. Read the main effects with care — ",
          "each one averages over a factor it interacts with."
        )
        return(verdict_box(res$p[3], alpha(), headline))
      }
      headline <- glue::glue(
        "No interaction: F({res$df[3]}, {res$df_resid}) = ",
        "{fmt_num(res$f[3], 3)}, {fmt_p_inline(res$p[3])}. The two ",
        "factors act independently, so the main effects can be read ",
        "on their own: {cn$a} {fmt_p_inline(res$p[1])}, {cn$b} ",
        "{fmt_p_inline(res$p[2])}."
      )
      return(verdict_box(res$p[3], alpha(), headline))
    })

    output$balance <- shiny::renderUI({
      shiny::req(ready())
      if (is_balanced()) {
        return(shiny::div(
          class = "alert-box alert-ok",
          glue::glue(
            "Balanced design: {as.vector(counts())[1]} observations in ",
            "every combination. All sums-of-squares conventions agree ",
            "on a balanced design, so the table below is unambiguous."
          )
        ))
      }
      cn <- col_names()
      return(shiny::div(
        class = "alert-box alert-warn",
        shiny::strong("Unbalanced design. "),
        glue::glue(
          "The combinations do not all have the same number of ",
          "observations (see the cell counts below). This app reports ",
          "Type I sums of squares, which are calculated in sequence: ",
          "{cn$a} first, then {cn$b} after allowing for {cn$a}, then ",
          "the interaction. On an unbalanced design that order changes ",
          "the answer. Swap your Factor A and Factor B selections to ",
          "see how much, and report which order you used."
        )
      ))
    })

    output$anova_table <- shiny::renderTable(
      {
        tab <- anova_terms()$table
        cn <- col_names()
        out <- tibble::tibble(
          Source = c(
            cn$a, cn$b, glue::glue("{cn$a} × {cn$b}"), "Residuals"
          ),
          df = as.integer(tab$Df),
          `Sum Sq` = fmt_num(tab$`Sum Sq`, 2),
          `Mean Sq` = fmt_num(tab$`Mean Sq`, 2),
          `F` = c(fmt_num(tab$`F value`[1:3], 3), "—"),
          `p` = c(fmt_p(tab$`Pr(>F)`[1:3]), "—")
        )
        return(out)
      },
      striped = TRUE, spacing = "xs", align = "lrrrrr"
    )

    output$cell_table <- shiny::renderTable(
      {
        cn <- col_names()
        out <- cell_means(cells()) |>
          dplyr::transmute(
            !!cn$a := as.character(.data$a),
            !!cn$b := as.character(.data$b),
            n = .data$n,
            Mean = fmt_num(.data$mean),
            SE = fmt_num(.data$se)
          )
        return(out)
      },
      striped = TRUE, spacing = "xs", align = "llrrr"
    )

    output$assumptions <- shiny::renderUI({
      shiny::req(ready())
      cd <- cells()
      cell_frame <- tibble::tibble(
        value = cd$value, group = cd$cell
      )
      checks <- list(
        check_normality_residuals(fit()),
        check_variance(cell_frame, welch_used = FALSE),
        check_outliers(cell_frame, mention_plot = FALSE),
        check_independence()
      )
      return(shiny::tagList(lapply(checks, check_block)))
    })

    output$qq <- shiny::renderPlot({
      shiny::req(ready())
      return(plot_qq(residual_frame(fit())))
    })

    # ---- plots ------------------------------------------------------------
    plot_roles <- shiny::reactive({
      cd <- cells()
      cn <- col_names()
      if (isTRUE(input$swap_axes)) {
        return(list(
          data = tibble::tibble(value = cd$value, a = cd$b, b = cd$a),
          xlab = cn$b, color_lab = cn$a
        ))
      }
      return(list(
        data = tibble::tibble(value = cd$value, a = cd$a, b = cd$b),
        xlab = cn$a, color_lab = cn$b
      ))
    })

    too_many_colors <- shiny::reactive({
      return(nlevels(plot_roles()$data$b) > length(pal$categorical))
    })

    plot_labels <- shiny::reactive({
      roles <- plot_roles()
      return(list(
        title = label_or(input$plot_title, NULL),
        xlab = label_or(input$plot_xlab, roles$xlab),
        ylab = label_or(input$plot_ylab, col_names()$response),
        color_lab = roles$color_lab
      ))
    })

    the_plot <- shiny::reactive({
      shiny::req(!too_many_colors())
      labs <- plot_labels()
      return(plot_interaction(
        plot_roles()$data, xlab = labs$xlab, ylab = labs$ylab,
        color_lab = labs$color_lab, title = labs$title
      ))
    })

    the_box_plot <- shiny::reactive({
      shiny::req(!too_many_colors())
      labs <- plot_labels()
      return(plot_grouped_boxes(
        plot_roles()$data, style = input$plot_style %||% "box",
        xlab = labs$xlab, ylab = labs$ylab,
        color_lab = labs$color_lab, title = labs$title
      ))
    })

    output$plot <- shiny::renderPlot({
      shiny::req(ready())
      return(the_plot())
    })

    output$box_plot <- shiny::renderPlot({
      shiny::req(ready())
      return(the_box_plot())
    })

    output$plot_note <- shiny::renderUI({
      if (!too_many_colors()) {
        return(NULL)
      }
      roles <- plot_roles()
      return(shiny::div(
        class = "alert-box alert-warn",
        glue::glue(
          "{roles$color_lab} has {nlevels(roles$data$b)} levels. No ",
          "set of colors stays distinguishable past ",
          "{length(pal$categorical)}, so the plot is not drawn. If the ",
          "other factor has fewer levels, tick “Swap which factor is ",
          "on the x-axis” under Plot options. The tables above are ",
          "unaffected."
        )
      ))
    })

    output$download_plot <- plot_download_handler(the_plot, "interaction")
    output$download_box <- plot_download_handler(
      the_box_plot, "two-way-groups"
    )

    # ---- post-hoc rendering ------------------------------------------------
    output$posthoc_intro <- shiny::renderUI({
      cn <- col_names()
      if (interaction_sig()) {
        return(shiny::p(class = "hint", glue::glue(
          "Because the interaction is significant, the comparison that ",
          "means something is between individual combinations, not ",
          "between {cn$a} or {cn$b} on their own. The simple effects ",
          "below ask the same question one level at a time."
        )))
      }
      return(shiny::p(class = "hint", paste(
        "The interaction is not significant, so each factor can be",
        "compared on its own, pooling over the other."
      )))
    })

    output$tukey_cells_table <- shiny::renderTable(
      {
        tk <- tukey_cells()
        out <- tibble::tibble(
          Comparison = tk$comparison,
          Difference = fmt_num(tk$diff),
          Lower = fmt_num(tk$lwr),
          Upper = fmt_num(tk$upr),
          `Adjusted p` = fmt_p(tk$p_adj),
          Significant = ifelse(tk$p_adj < alpha(), "yes", "no")
        )
        return(out)
      },
      striped = TRUE, spacing = "xs", align = "lrrrrc"
    )

    output$tukey_cells_plot <- shiny::renderPlot({
      shiny::req(ready(), isTRUE(input$tukey), interaction_sig())
      return(plot_tukey(
        tukey_cells(), conf_level = conf_level(),
        ylab = glue::glue("Difference in {col_names()$response}")
      ))
    })

    output$download_tukey <- plot_download_handler(
      shiny::reactive({
        plot_tukey(
          tukey_cells(), conf_level = conf_level(),
          ylab = glue::glue("Difference in {col_names()$response}")
        )
      }),
      "tukey-cells"
    )

    output$simple_table <- shiny::renderTable(
      {
        se <- simple_effects()
        cn <- col_names()
        direction <- input$simple_dir %||% "a_within_b"
        split_name <- if (identical(direction, "a_within_b")) {
          cn$b
        } else {
          cn$a
        }
        effect_name <- if (identical(direction, "a_within_b")) {
          cn$a
        } else {
          cn$b
        }
        out <- tibble::tibble(
          Level = se$level,
          df = glue::glue("{se$df1}, {se$df2}"),
          `F` = fmt_num(se$f, 3),
          `p` = fmt_p(se$p),
          Significant = ifelse(se$p < alpha(), "yes", "no")
        )
        names(out)[1] <- glue::glue("{split_name} level")
        names(out)[3] <- glue::glue("F for {effect_name}")
        return(out)
      },
      striped = TRUE, spacing = "xs", align = "lrrrc"
    )

    output$main_effect_tukey <- shiny::renderUI({
      res <- anova_terms()
      cn <- col_names()
      cards <- list()
      for (i in seq_len(2)) {
        term <- c("a", "b")[i]
        term_name <- c(cn$a, cn$b)[i]
        if (is.na(res$p[i]) || res$p[i] >= alpha()) {
          next
        }
        if (nlevels(cells()[[term]]) < 3) {
          next
        }
        cards <- c(cards, list(shiny::tagList(
          shiny::tags$h6(glue::glue("Tukey HSD for {term_name}")),
          shiny::div(
            class = "table-scroll",
            shiny::tableOutput(ns(glue::glue("tukey_{term}")))
          )
        )))
      }
      if (length(cards) == 0) {
        return(shiny::p(class = "hint", paste(
          "No main effect needs a post-hoc test: either none is",
          "significant, or the significant ones have only two levels,",
          "in which case the ANOVA row already tells you the two group",
          "means differ."
        )))
      }
      return(shiny::tagList(cards))
    })

    output$tukey_a <- shiny::renderTable(
      {
        tk <- tukey_main("a")
        return(tibble::tibble(
          Comparison = tk$comparison,
          Difference = fmt_num(tk$diff),
          Lower = fmt_num(tk$lwr), Upper = fmt_num(tk$upr),
          `Adjusted p` = fmt_p(tk$p_adj),
          Significant = ifelse(tk$p_adj < alpha(), "yes", "no")
        ))
      },
      striped = TRUE, spacing = "xs", align = "lrrrrc"
    )

    output$tukey_b <- shiny::renderTable(
      {
        tk <- tukey_main("b")
        return(tibble::tibble(
          Comparison = tk$comparison,
          Difference = fmt_num(tk$diff),
          Lower = fmt_num(tk$lwr), Upper = fmt_num(tk$upr),
          `Adjusted p` = fmt_p(tk$p_adj),
          Significant = ifelse(tk$p_adj < alpha(), "yes", "no")
        ))
      },
      striped = TRUE, spacing = "xs", align = "lrrrrc"
    )

    output$counts_table <- shiny::renderTable(
      {
        tab <- counts()
        cn <- col_names()
        out <- tibble::as_tibble(
          as.data.frame.matrix(tab), rownames = cn$a
        )
        return(out)
      },
      striped = TRUE, spacing = "xs"
    )

    # ---- body -------------------------------------------------------------
    posthoc_section <- function() {
      if (!isTRUE(input$tukey)) {
        return(NULL)
      }
      if (interaction_sig()) {
        tukey_content <- if (show_plots) {
          bslib::layout_columns(
            col_widths = c(6, 6),
            shiny::div(
              class = "table-scroll",
              shiny::tableOutput(ns("tukey_cells_table"))
            ),
            plot_panel(
              ns, height = "360px",
              plot_id = "tukey_cells_plot",
              dl_id = "download_tukey"
            )
          )
        } else {
          shiny::div(
            class = "table-scroll",
            shiny::tableOutput(ns("tukey_cells_table"))
          )
        }
        return(bslib::card(
          bslib::card_header(
            "Post-hoc: which combinations differ?"
          ),
          shiny::uiOutput(ns("posthoc_intro")),
          tukey_content,
          shiny::tags$h6("Simple effects"),
          shiny::div(
            class = "table-scroll",
            shiny::tableOutput(ns("simple_table"))
          )
        ))
      }
      return(bslib::card(
        bslib::card_header("Post-hoc: main effects"),
        shiny::uiOutput(ns("posthoc_intro")),
        shiny::uiOutput(ns("main_effect_tukey"))
      ))
    }

    output$body <- shiny::renderUI({
      if (is.null(data$raw())) {
        return(no_data_panel("a two-way factorial ANOVA"))
      }
      probs <- problems()
      if (length(probs) > 0) {
        return(cannot_run_panel(probs))
      }
      shiny::req(ready())
      interaction_card <- if (show_plots) {
        bslib::card(
          bslib::card_header("Interaction plot"),
          shiny::uiOutput(ns("plot_note")),
          plot_panel(ns, height = "380px"),
          shiny::p(class = "hint", paste(
            "Parallel lines mean no interaction.",
            "Lines that converge, diverge, or cross",
            "are what an interaction looks like."
          ))
        )
      }
      group_card <- if (show_plots) {
        bslib::card(
          bslib::card_header("Group comparison"),
          plot_panel(
            ns, height = "420px",
            plot_id = "box_plot",
            dl_id = "download_box"
          )
        )
      }
      widths <- if (show_plots) c(6, 6) else 12
      return(shiny::tagList(
        shiny::uiOutput(ns("verdict")),
        shiny::uiOutput(ns("balance")),
        bslib::layout_columns(
          col_widths = widths,
          bslib::card(
            bslib::card_header("ANOVA table"),
            shiny::div(
              class = "table-scroll",
              shiny::tableOutput(ns("anova_table"))
            ),
            shiny::tags$h6("Cell means"),
            shiny::div(
              class = "table-scroll",
              shiny::tableOutput(ns("cell_table"))
            ),
            shiny::tags$h6("Cell counts"),
            shiny::div(
              class = "table-scroll",
              shiny::tableOutput(ns("counts_table"))
            )
          ),
          interaction_card
        ),
        group_card,
        posthoc_section(),
        bslib::accordion(
          open = TRUE,
          bslib::accordion_panel(
            "Assumptions — check these before trusting the p-value",
            shiny::uiOutput(ns("assumptions")),
            shiny::tags$h6(
              "QQ plot of the model residuals"
            ),
            shiny::plotOutput(ns("qq"), height = "260px")
          )
        )
      ))
    })
  })
}
