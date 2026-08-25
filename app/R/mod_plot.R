# Standalone Plot tab.
#
# Students can build bar plots, boxplots, histograms, and XY
# scatterplots here, independent of any statistical test. The tab
# is always visible regardless of the show_stat_plots flag.

mod_plot_ui <- function(id) {
  ns <- shiny::NS(id)
  out <- bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 360,
      shiny::selectInput(
        ns("type"), "Plot type",
        choices = c(
          "Boxplot" = "box",
          "Bar plot (means \u00b1 SE)" = "bar",
          "Histogram" = "hist",
          "XY scatterplot" = "scatter"
        ),
        selected = "box"
      ),
      shiny::hr(),
      shiny::conditionalPanel(
        "input.type == 'box'", ns = ns,
        shiny::radioButtons(
          ns("box_style"), "Style",
          choices = c(
            "Boxplot" = "box",
            "Violin" = "violin",
            "Points only" = "points"
          ),
          selected = "box", inline = TRUE
        )
      ),
      shiny::conditionalPanel(
        "input.type == 'hist'", ns = ns,
        shiny::uiOutput(ns("hist_col_ui")),
        shiny::numericInput(
          ns("bins"), "Number of bins",
          value = 20, min = 5, max = 100, step = 1
        )
      ),
      shiny::conditionalPanel(
        "input.type == 'scatter'", ns = ns,
        shiny::uiOutput(ns("scatter_col_ui"))
      ),
      shiny::hr(),
      shiny::textInput(
        ns("plot_title"), "Plot title", value = ""
      ),
      shiny::textInput(
        ns("plot_xlab"), "X-axis label", value = ""
      ),
      shiny::textInput(
        ns("plot_ylab"), "Y-axis label", value = ""
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
        raw, is.numeric, logical(1)
      )])
    })

    output$hist_col_ui <- shiny::renderUI({
      cols <- numeric_cols()
      choices <- c(
        "Grouped values" = "__tidy__",
        stats::setNames(cols, cols)
      )
      return(shiny::selectInput(
        ns("hist_col"), "Column to plot",
        choices = choices, selected = "__tidy__"
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
          ns("scatter_x"), "X column",
          choices = cols, selected = cols[1]
        ),
        shiny::selectInput(
          ns("scatter_y"), "Y column",
          choices = cols,
          selected = cols[min(2, length(cols))]
        )
      ))
    })

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

    the_plot <- shiny::reactive({
      shiny::req(length(problems()) == 0)
      type <- input$type %||% "box"
      labs <- data$labels()
      title <- label_or(input$plot_title, NULL)
      xlab_in <- input$plot_xlab
      ylab_in <- input$plot_ylab

      if (type == "box") {
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
        tidy <- data$tidy()
        return(plot_bar(
          tidy, title = title,
          xlab = label_or(xlab_in, labs$group),
          ylab = label_or(ylab_in, labs$value)
        ))
      }

      if (type == "hist") {
        hv <- hist_values()
        bins <- input$bins %||% 20
        bins <- max(5, min(100, bins))
        return(plot_histogram(
          hv$values, bins = bins,
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
          raw, x_col, y_col,
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
      the_plot, "plot"
    )

    code_text <- shiny::reactive({
      shiny::req(length(problems()) == 0)
      type <- input$type %||% "box"
      labs <- data$labels()
      title_arg <- input$plot_title
      has_title <- !is.null(title_arg) && nzchar(title_arg)

      if (type == "box") {
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

      if (type == "hist") {
        hv <- hist_values()
        bins <- input$bins %||% 20
        code <- glue::glue(
          "ggplot(my_data,",
          " aes(x = {hv$label})) +\n",
          "  geom_histogram(bins = {bins})"
        )
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
