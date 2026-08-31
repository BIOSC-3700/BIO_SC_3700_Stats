# Standalone Plot tab.
#
# Students can build bar plots, boxplots, interaction plots,
# histograms, and XY scatterplots here, independent of any
# statistical test. The tab is always visible regardless of
# the show_stat_plots flag.

mod_plot_ui <- function(id) {
  ns <- shiny::NS(id)
  out <- bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 360,
      shiny::selectInput(
        ns("type"),
        "Plot type",
        choices = c(
          "Boxplot" = "box",
          "Bar plot (means \u00b1 SE)" = "bar",
          "Interaction plot" = "interaction",
          "Histogram" = "hist",
          "XY scatterplot" = "scatter"
        ),
        selected = "box"
      ),
      shiny::hr(),
      shiny::conditionalPanel(
        "input.type == 'box'",
        ns = ns,
        shiny::radioButtons(
          ns("box_style"),
          "Style",
          choices = c(
            "Boxplot" = "box",
            "Violin" = "violin",
            "Points only" = "points"
          ),
          selected = "box",
          inline = TRUE
        )
      ),
      shiny::conditionalPanel(
        paste0(
          "input.type == 'box' || ",
          "input.type == 'bar' || ",
          "input.type == 'interaction'"
        ),
        ns = ns,
        shiny::uiOutput(ns("color_by_ui"))
      ),
      shiny::conditionalPanel(
        "input.type == 'hist'",
        ns = ns,
        shiny::uiOutput(ns("hist_col_ui")),
        shiny::uiOutput(ns("hist_facet_ui")),
        shiny::numericInput(
          ns("bins"),
          "Number of bins",
          value = 20,
          min = 5,
          max = 100,
          step = 1
        )
      ),
      shiny::conditionalPanel(
        "input.type == 'scatter'",
        ns = ns,
        shiny::uiOutput(ns("scatter_col_ui"))
      ),
      shiny::hr(),
      shiny::textInput(
        ns("plot_title"),
        "Plot title",
        value = ""
      ),
      shiny::textInput(
        ns("plot_xlab"),
        "X-axis label",
        value = ""
      ),
      shiny::textInput(
        ns("plot_ylab"),
        "Y-axis label",
        value = ""
      )
    ),
    shiny::uiOutput(ns("body"))
  )
  return(out)
}

mod_plot_server <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    numeric_cols <- shiny::reactive({
      raw <- data$raw()
      if (is.null(raw)) {
        return(character(0))
      }
      return(names(raw)[vapply(
        raw,
        is.numeric,
        logical(1)
      )])
    })

    # ---- color-by / interaction selectors ----

    output$color_by_ui <- shiny::renderUI({
      raw <- data$raw()
      shiny::req(raw)
      type <- input$type %||% "box"
      labs <- data$labels()
      all_cols <- names(raw)
      nums <- numeric_cols()
      non_num <- setdiff(all_cols, nums)

      if (type == "interaction") {
        resp_default <- if (!is.null(labs$value) && labs$value %in% nums) {
          labs$value
        } else {
          nums[1]
        }
        a_default <- if (!is.null(labs$group) && labs$group %in% all_cols) {
          labs$group
        } else {
          (non_num[1] %||% all_cols[1])
        }
        b_choices <- setdiff(all_cols, c(a_default))
        b_non_num <- intersect(b_choices, non_num)
        b_default <- if (length(b_non_num) > 0) {
          b_non_num[1]
        } else {
          b_choices[1]
        }
        return(shiny::tagList(
          shiny::selectInput(
            ns("interact_response"),
            "Measurement column",
            choices = nums,
            selected = resp_default
          ),
          shiny::selectInput(
            ns("interact_x"),
            "X-axis variable",
            choices = all_cols,
            selected = a_default
          ),
          shiny::selectInput(
            ns("interact_color"),
            "Color variable",
            choices = all_cols,
            selected = b_default
          ),
          shiny::textInput(
            ns("color_lab"),
            "Color variable label",
            value = ""
          )
        ))
      }

      # box or bar: optional color-by
      exclude <- c(labs$value, labs$group)
      candidates <- setdiff(all_cols, exclude)
      if (length(candidates) == 0) {
        return(shiny::p(
          class = "hint",
          paste(
            "No additional columns available",
            "for a second grouping variable."
          )
        ))
      }
      choices <- c(
        "None" = "__none__",
        stats::setNames(candidates, candidates)
      )
      return(shiny::tagList(
        shiny::selectInput(
          ns("color_by"),
          "Color by (second variable)",
          choices = choices,
          selected = "__none__"
        ),
        shiny::conditionalPanel(
          "input.color_by != '__none__'",
          ns = ns,
          shiny::textInput(
            ns("color_lab"),
            "Color variable label",
            value = ""
          )
        )
      ))
    })

    has_color_var <- shiny::reactive({
      type <- input$type %||% "box"
      if (type == "interaction") {
        return(TRUE)
      }
      color <- input$color_by %||% "__none__"
      return(!identical(color, "__none__"))
    })

    cells <- shiny::reactive({
      type <- input$type %||% "box"
      raw <- data$raw()
      shiny::req(raw)

      if (type == "interaction") {
        resp <- input$interact_response
        a_col <- input$interact_x
        b_col <- input$interact_color
        shiny::req(resp, a_col, b_col)
        shiny::req(
          resp %in% names(raw),
          a_col %in% names(raw),
          b_col %in% names(raw)
        )
      } else {
        labs <- data$labels()
        resp <- labs$value
        a_col <- labs$group
        b_col <- input$color_by
        shiny::req(
          b_col,
          !identical(b_col, "__none__")
        )
        shiny::req(
          resp %in% names(raw),
          a_col %in% names(raw),
          b_col %in% names(raw)
        )
      }

      values <- suppressWarnings(
        as.numeric(as.character(raw[[resp]]))
      )
      out <- tibble::tibble(
        value = values,
        a = factor(as.character(raw[[a_col]])),
        b = factor(as.character(raw[[b_col]]))
      )
      out <- out[
        !is.na(out$value) &
          !is.na(out$a) &
          !is.na(out$b),
      ]
      out$a <- droplevels(out$a)
      out$b <- droplevels(out$b)
      return(out)
    })

    # ---- histogram / scatter selectors ----

    output$hist_col_ui <- shiny::renderUI({
      cols <- numeric_cols()
      choices <- c(
        "Grouped values" = "__tidy__",
        stats::setNames(cols, cols)
      )
      return(shiny::selectInput(
        ns("hist_col"),
        "Column to plot",
        choices = choices,
        selected = "__tidy__"
      ))
    })

    output$hist_facet_ui <- shiny::renderUI({
      raw <- data$raw()
      shiny::req(raw)
      all_cols <- names(raw)
      nums <- numeric_cols()
      non_num <- setdiff(all_cols, nums)
      candidates <- if (length(non_num) > 0) {
        non_num
      } else {
        all_cols
      }
      choices <- c(
        "None" = "__none__",
        stats::setNames(candidates, candidates)
      )
      return(shiny::selectInput(
        ns("hist_facet"),
        "Facet by",
        choices = choices,
        selected = "__none__"
      ))
    })

    output$scatter_col_ui <- shiny::renderUI({
      cols <- numeric_cols()
      if (length(cols) < 2) {
        return(shiny::p(
          class = "hint",
          paste(
            "A scatterplot needs at least two numeric",
            "columns. This data has fewer than two."
          )
        ))
      }
      return(shiny::tagList(
        shiny::selectInput(
          ns("scatter_x"),
          "X column",
          choices = cols,
          selected = cols[1]
        ),
        shiny::selectInput(
          ns("scatter_y"),
          "Y column",
          choices = cols,
          selected = cols[min(2, length(cols))]
        )
      ))
    })

    # ---- validation ----

    problems <- shiny::reactive({
      type <- input$type %||% "box"
      if (type %in% c("box", "bar")) {
        tidy <- data$tidy()
        if (is.null(tidy)) {
          return("Load data on the Data tab first.")
        }
        if (nlevels(tidy$group) < 1) {
          return("No groups found in the data.")
        }
        if (has_color_var()) {
          b_col <- input$color_by
          if (
            is.null(b_col) ||
              identical(b_col, "__none__")
          ) {
            return(character(0))
          }
          cd <- cells()
          if (is.null(cd) || nrow(cd) == 0) {
            return("No usable rows after cleaning.")
          }
          if (nlevels(cd$b) < 2) {
            return(paste(
              b_col,
              "needs at least 2 levels."
            ))
          }
          if (nlevels(cd$b) > length(pal$categorical)) {
            return(paste0(
              b_col,
              " has ",
              nlevels(cd$b),
              " levels; the palette supports at most ",
              length(pal$categorical),
              "."
            ))
          }
        }
      }
      if (type == "interaction") {
        if (is.null(data$raw())) {
          return("Load data on the Data tab first.")
        }
        resp <- input$interact_response
        a_col <- input$interact_x
        b_col <- input$interact_color
        if (is.null(resp) || is.null(a_col) || is.null(b_col)) {
          return(character(0))
        }
        chosen <- c(resp, a_col, b_col)
        if (anyDuplicated(chosen) > 0) {
          return(paste(
            "The measurement, x-axis, and color",
            "columns must all be different."
          ))
        }
        cd <- cells()
        if (is.null(cd) || nrow(cd) == 0) {
          return("No usable rows after cleaning.")
        }
        if (nlevels(cd$a) < 2) {
          return(paste(
            a_col,
            "needs at least 2 levels."
          ))
        }
        if (nlevels(cd$b) < 2) {
          return(paste(
            b_col,
            "needs at least 2 levels."
          ))
        }
        if (nlevels(cd$b) > length(pal$categorical)) {
          return(paste0(
            b_col,
            " has ",
            nlevels(cd$b),
            " levels; the palette supports at most ",
            length(pal$categorical),
            "."
          ))
        }
      }
      if (type == "hist") {
        if (is.null(data$raw())) {
          return("Load data on the Data tab first.")
        }
      }
      if (type == "scatter") {
        if (is.null(data$raw())) {
          return("Load data on the Data tab first.")
        }
        if (length(numeric_cols()) < 2) {
          return(paste(
            "A scatterplot needs at least two",
            "numeric columns."
          ))
        }
      }
      return(character(0))
    })

    # ---- histogram helper ----

    hist_values <- shiny::reactive({
      col <- input$hist_col %||% "__tidy__"
      if (identical(col, "__tidy__")) {
        tidy <- data$tidy()
        shiny::req(tidy)
        return(list(
          values = tidy$value,
          label = data$labels()$value
        ))
      }
      raw <- data$raw()
      shiny::req(raw, col %in% names(raw))
      return(list(values = raw[[col]], label = col))
    })

    # ---- plot dispatch ----

    the_plot <- shiny::reactive({
      shiny::req(length(problems()) == 0)
      type <- input$type %||% "box"
      labs <- data$labels()
      title <- label_or(input$plot_title, NULL)
      xlab_in <- input$plot_xlab
      ylab_in <- input$plot_ylab

      if (type == "box") {
        if (has_color_var()) {
          cd <- cells()
          color_col <- input$color_by
          color_label <- label_or(
            input$color_lab,
            color_col
          )
          return(plot_grouped_boxes(
            cd,
            style = input$box_style %||% "box",
            xlab = label_or(xlab_in, labs$group),
            ylab = label_or(ylab_in, labs$value),
            color_lab = color_label,
            title = title
          ))
        }
        tidy <- data$tidy()
        return(plot_groups(
          tidy,
          style = input$box_style %||% "box",
          title = title,
          xlab = label_or(xlab_in, labs$group),
          ylab = label_or(ylab_in, labs$value)
        ))
      }

      if (type == "bar") {
        if (has_color_var()) {
          cd <- cells()
          color_col <- input$color_by
          color_label <- label_or(
            input$color_lab,
            color_col
          )
          return(plot_grouped_bar(
            cd,
            xlab = label_or(xlab_in, labs$group),
            ylab = label_or(ylab_in, labs$value),
            color_lab = color_label,
            title = title
          ))
        }
        tidy <- data$tidy()
        return(plot_bar(
          tidy,
          title = title,
          xlab = label_or(xlab_in, labs$group),
          ylab = label_or(ylab_in, labs$value)
        ))
      }

      if (type == "interaction") {
        cd <- cells()
        a_col <- input$interact_x
        b_col <- input$interact_color
        resp <- input$interact_response
        color_label <- label_or(
          input$color_lab,
          b_col
        )
        return(plot_interaction(
          cd,
          xlab = label_or(xlab_in, a_col),
          ylab = label_or(ylab_in, resp),
          color_lab = color_label,
          title = title
        ))
      }

      if (type == "hist") {
        hv <- hist_values()
        bins <- input$bins %||% 20
        bins <- max(5, min(100, bins))
        facet <- input$hist_facet %||% "__none__"
        if (
          !identical(facet, "__none__") &&
            facet %in% names(data$raw())
        ) {
          return(plot_faceted_histogram(
            data$raw(),
            value_col = hv$label,
            facet_col = facet,
            bins = bins,
            title = title,
            xlab = label_or(xlab_in, hv$label)
          ))
        }
        return(plot_histogram(
          hv$values,
          bins = bins,
          title = title,
          xlab = label_or(xlab_in, hv$label)
        ))
      }

      if (type == "scatter") {
        raw <- data$raw()
        x_col <- input$scatter_x
        y_col <- input$scatter_y
        shiny::req(x_col, y_col)
        shiny::req(
          x_col %in% names(raw),
          y_col %in% names(raw)
        )
        return(plot_scatter(
          raw,
          x_col,
          y_col,
          title = title,
          xlab = label_or(xlab_in, x_col),
          ylab = label_or(ylab_in, y_col)
        ))
      }
    })

    output$plot <- shiny::renderPlot({
      return(the_plot())
    })

    output$download_plot <- plot_download_handler(
      the_plot,
      "plot"
    )

    # ---- code text ----

    code_text <- shiny::reactive({
      shiny::req(length(problems()) == 0)
      type <- input$type %||% "box"
      labs <- data$labels()
      title_arg <- input$plot_title
      has_title <- !is.null(title_arg) &&
        nzchar(title_arg)

      if (type == "box") {
        if (has_color_var()) {
          color_col <- input$color_by
          style <- input$box_style %||% "box"
          geom <- switch(
            style,
            box = glue::glue(
              "geom_boxplot(",
              "aes(fill = {color_col}))"
            ),
            violin = glue::glue(
              "geom_violin(",
              "aes(fill = {color_col}))"
            ),
            points = glue::glue(
              "geom_point(",
              "aes(shape = {color_col}))"
            )
          )
          code <- glue::glue(
            "ggplot(my_data,\n",
            "       aes(x = {labs$group},",
            " y = {labs$value},\n",
            "           color = {color_col})) +\n",
            "  {geom}"
          )
          if (has_title) {
            code <- glue::glue(
              '{code} +\n',
              '  labs(title = "{title_arg}")'
            )
          }
          return(code)
        }
        style <- input$box_style %||% "box"
        geom <- switch(
          style,
          box = "geom_boxplot()",
          violin = "geom_violin()",
          points = "geom_point()"
        )
        code <- glue::glue(
          "ggplot(my_data,\n",
          "       aes(x = {labs$group},",
          " y = {labs$value})) +\n",
          "  {geom}"
        )
        if (has_title) {
          code <- glue::glue(
            '{code} +\n',
            '  labs(title = "{title_arg}")'
          )
        }
        return(code)
      }

      if (type == "bar") {
        if (has_color_var()) {
          color_col <- input$color_by
          code <- glue::glue(
            "group_means <- my_data |>\n",
            "  group_by({labs$group},",
            " {color_col}) |>\n",
            "  summarize(\n",
            "    mean = mean({labs$value}),\n",
            "    se = sd({labs$value})",
            " / sqrt(n()),\n",
            "    .groups = \"drop\"\n",
            "  )\n\n",
            "ggplot(group_means,\n",
            "       aes(x = {labs$group},",
            " y = mean,\n",
            "           fill = {color_col})) +\n",
            "  geom_col(",
            "position = position_dodge(",
            "0.7)) +\n",
            "  geom_errorbar(\n",
            "    aes(ymin = mean - se,",
            " ymax = mean + se),\n",
            "    width = 0.15,\n",
            "    position = position_dodge(",
            "0.7))"
          )
          if (has_title) {
            code <- glue::glue(
              '{code} +\n',
              '  labs(title = "{title_arg}")'
            )
          }
          return(code)
        }
        code <- glue::glue(
          "group_means <- my_data |>\n",
          "  group_by({labs$group}) |>\n",
          "  summarize(\n",
          "    mean = mean({labs$value}),\n",
          "    se = sd({labs$value}) /",
          " sqrt(n())\n",
          "  )\n\n",
          "ggplot(group_means,\n",
          "       aes(x = {labs$group},",
          " y = mean)) +\n",
          "  geom_col() +\n",
          "  geom_errorbar(",
          "aes(ymin = mean - se,\n",
          "                  ",
          "ymax = mean + se),\n",
          "               width = 0.15)"
        )
        if (has_title) {
          code <- glue::glue(
            '{code} +\n',
            '  labs(title = "{title_arg}")'
          )
        }
        return(code)
      }

      if (type == "interaction") {
        resp <- input$interact_response
        a_col <- input$interact_x
        b_col <- input$interact_color
        shiny::req(resp, a_col, b_col)
        code <- glue::glue(
          "cell_means <- my_data |>\n",
          "  group_by({a_col}, {b_col}) |>\n",
          "  summarize(\n",
          "    mean = mean({resp}),\n",
          "    se = sd({resp})",
          " / sqrt(n()),\n",
          "    .groups = \"drop\"\n",
          "  )\n\n",
          "ggplot(cell_means,\n",
          "       aes(x = {a_col},",
          " y = mean,\n",
          "           color = {b_col},",
          " group = {b_col})) +\n",
          "  geom_errorbar(\n",
          "    aes(ymin = mean - se,",
          " ymax = mean + se),\n",
          "    width = 0.08) +\n",
          "  geom_line() +\n",
          "  geom_point(",
          "aes(shape = {b_col}),",
          " size = 3)"
        )
        if (has_title) {
          code <- glue::glue(
            '{code} +\n',
            '  labs(title = "{title_arg}")'
          )
        }
        return(code)
      }

      if (type == "hist") {
        hv <- hist_values()
        bins <- input$bins %||% 20
        facet <- input$hist_facet %||% "__none__"
        code <- glue::glue(
          "ggplot(my_data,",
          " aes(x = {hv$label})) +\n",
          "  geom_histogram(bins = {bins})"
        )
        if (!identical(facet, "__none__")) {
          code <- glue::glue(
            "{code} +\n",
            "  facet_grid(rows = vars({facet}))"
          )
        }
        if (has_title) {
          code <- glue::glue(
            '{code} +\n',
            '  labs(title = "{title_arg}")'
          )
        }
        return(code)
      }

      if (type == "scatter") {
        x_col <- input$scatter_x
        y_col <- input$scatter_y
        shiny::req(x_col, y_col)
        code <- glue::glue(
          "ggplot(my_data,\n",
          "       aes(x = {x_col},",
          " y = {y_col})) +\n",
          "  geom_point()"
        )
        if (has_title) {
          code <- glue::glue(
            '{code} +\n',
            '  labs(title = "{title_arg}")'
          )
        }
        return(code)
      }
    })

    # ---- body output ----

    output$body <- shiny::renderUI({
      if (is.null(data$raw())) {
        return(no_data_panel("a plot"))
      }
      probs <- problems()
      if (length(probs) > 0) {
        return(cannot_run_panel(probs))
      }
      return(shiny::tagList(
        bslib::card(
          bslib::card_header("Plot"),
          plot_panel(ns)
        ),
        code_accordion(code_text())
      ))
    })
  })
}
