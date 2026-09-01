# Data tab: load measurements, declare their layout, and hand a tidy
# two-column table to the analysis tabs.
#
# The module returns a list of reactives:
#   raw()   the table exactly as loaded, or NULL
#   tidy()  tibble(value = numeric, group = factor) with NA dropped
#   info()  counts and warnings from parsing, for the alert panel

example_files <- c(
  "Horned lizard horn length — 2 independent groups" = "horned_lizards.csv",
  "Jet lag and knees — 3 independent light treatments" = "jetlag_knees.csv",
  "Blackbird antibodies — paired before/after measurements" = "blackbird_antibodies.csv",
  "Lion nose coloration — 2 continuous variables" = "lion_noses.csv",
  "Intertidal algae — height X herbivores, factorial data" = "intertidal_algae.csv"
)


mod_data_ui <- function(id) {
  ns <- shiny::NS(id)
  out <- bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 380,
      shiny::radioButtons(
        ns("source"),
        "Where is your data?",
        choices = c(
          "Upload a file" = "file",
          "Paste from a spreadsheet" = "paste",
          "Use an example dataset" = "example"
        ),
        selected = "example"
      ),
      shiny::conditionalPanel(
        "input.source == 'file'",
        ns = ns,
        shiny::fileInput(
          ns("file"),
          "Choose a CSV or Excel file",
          accept = c(".csv", ".txt", ".tsv", ".xlsx", ".xls")
        ),
        shiny::uiOutput(ns("sheet_ui"))
      ),
      shiny::conditionalPanel(
        "input.source == 'paste'",
        ns = ns,
        shiny::textAreaInput(
          ns("paste_text"),
          "Copy cells from Excel or Google Sheets and paste here",
          rows = 9,
          resize = "vertical",
          placeholder = "treatment\theight\ncontrol\t12.1\ncontrol\t11.4\n..."
        ),
        shiny::p(
          class = "hint",
          "Include the header row. Tabs, commas, and semicolons all work."
        ),
        shiny::actionButton(
          ns("paste_go"),
          "Use this data",
          class = "btn-primary btn-sm"
        )
      ),
      shiny::conditionalPanel(
        "input.source == 'example'",
        ns = ns,
        shiny::selectInput(
          ns("example"),
          "Example dataset",
          choices = example_files,
          selected = example_files[[1]]
        )
      ),
      shiny::hr(),
      shiny::uiOutput(ns("layout_ui"))
    ),
    shiny::uiOutput(ns("alerts")),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        min_height = "500px",
        bslib::card_header("Current Data"),
        DT::DTOutput(ns("preview"))
      ),
      bslib::card(
        min_height = "500px",
        bslib::card_header("Summary by group"),
        shiny::div(class = "table-scroll", shiny::tableOutput(ns("summary")))
      )
    ),
    shiny::conditionalPanel(
      "input.source == 'example'",
      ns = ns,
      shiny::hr(),
      shiny::div(
        class = "references",
        shiny::h6("Example dataset references"),
        shiny::tags$ul(
          shiny::tags$li(
            shiny::strong("Horned lizards:"),
            "Young, K. V., E. D. Brodie Jr., and E. D. Brodie III.",
            "2004. How the horned lizard got its horns.",
            shiny::em("Science"),
            "304: 65."
          ),
          shiny::tags$li(
            shiny::strong("Jet lag and knees:"),
            "Wright, K. P., Jr. and C. A. Czeisler. 2002.",
            "Absence of circadian phase resetting in",
            "response to bright light behind the knees.",
            shiny::em("Science"),
            "297: 571."
          ),
          shiny::tags$li(
            shiny::strong("Blackbird antibodies:"),
            "Hasselquist, D., J. A. Marsh, P. W. Sherman,",
            "and J. C. Wingfield. 1999. Is avian humoral",
            "immunocompetence suppressed by testosterone?",
            shiny::em("Behavioral Ecology and Sociobiology"),
            "45: 167\u2013175."
          ),
          shiny::tags$li(
            shiny::strong("Lion noses:"),
            "Whitman, K., A. M. Starfield, H. S. Quadling,",
            "and C. Packer. 2004. Sustainable trophy hunting",
            "of African lions.",
            shiny::em("Nature"),
            "428: 175\u2013178."
          ),
          shiny::tags$li(
            shiny::strong("Intertidal algae:"),
            "Harley, C. D. G. 2003. Abiotic stress and",
            "herbivory interact to set range limits across a",
            "two-dimensional stress gradient.",
            shiny::em("Ecology"),
            "84: 1477\u20131488."
          )
        ),
        shiny::p(
          class = "hint",
          "Data from Whitlock & Schluter,",
          shiny::em("The Analysis of Biological Data."),
          "R package:",
          shiny::tags$code("abdData"),
          "."
        )
      )
    )
  )
  return(out)
}

mod_data_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- file upload --------------------------------------------------
    file_ext <- shiny::reactive({
      shiny::req(input$file)
      return(tolower(tools::file_ext(input$file$name)))
    })

    sheet_names <- shiny::reactive({
      shiny::req(input$file)
      if (!file_ext() %in% c("xlsx", "xls")) {
        return(NULL)
      }
      res <- try(readxl::excel_sheets(input$file$datapath), silent = TRUE)
      if (inherits(res, "try-error")) {
        return(NULL)
      }
      return(res)
    })

    output$sheet_ui <- shiny::renderUI({
      sheets <- sheet_names()
      if (is.null(sheets) || length(sheets) < 2) {
        return(NULL)
      }
      return(shiny::selectInput(
        ns("sheet"),
        "Which sheet?",
        choices = sheets,
        selected = sheets[1]
      ))
    })

    # ---- one result object per source, always list(data, error) -------
    read_attempt <- function(expr) {
      out <- tryCatch(
        list(data = tibble::as_tibble(expr), error = NULL),
        error = function(e) list(data = NULL, error = conditionMessage(e))
      )
      return(out)
    }

    file_result <- shiny::reactive({
      shiny::req(input$file)
      ext <- file_ext()
      path <- input$file$datapath
      if (ext %in% c("xlsx", "xls")) {
        sheets <- sheet_names()
        chosen <- if (!is.null(input$sheet) && input$sheet %in% sheets) {
          input$sheet
        } else {
          sheets[1]
        }
        return(read_attempt(readxl::read_excel(path, sheet = chosen)))
      }
      if (ext == "csv") {
        return(read_attempt(
          read.csv(path, stringsAsFactors = FALSE)
        ))
      }
      return(read_attempt(
        read.table(path, header = TRUE, sep = "\t",
                   stringsAsFactors = FALSE)
      ))
    })

    paste_result <- shiny::eventReactive(input$paste_go, {
      txt <- input$paste_text
      if (is.null(txt) || !nzchar(trimws(txt))) {
        return(list(data = NULL, error = "Nothing was pasted."))
      }
      delim <- if (grepl("\t", txt, fixed = TRUE)) {
        "\t"
      } else if (grepl(";", txt, fixed = TRUE)) {
        ";"
      } else {
        ","
      }
      return(read_attempt(
        read.table(
          text = txt, header = TRUE, sep = delim,
          stringsAsFactors = FALSE, strip.white = TRUE
        )
      ))
    })

    example_result <- shiny::reactive({
      shiny::req(input$example)
      return(read_attempt(
        read.csv(
          fs::path("data", input$example),
          stringsAsFactors = FALSE
        )
      ))
    })

    raw_result <- shiny::reactive({
      switch(
        input$source,
        file = file_result(),
        paste = paste_result(),
        example = example_result()
      )
    })

    raw <- shiny::reactive({
      return(raw_result()$data)
    })

    # ---- layout inference and controls --------------------------------
    numeric_cols <- shiny::reactive({
      data <- raw()
      if (is.null(data)) {
        return(character(0))
      }
      return(names(data)[vapply(data, is.numeric, logical(1))])
    })

    inferred_layout <- shiny::reactive({
      data <- raw()
      if (is.null(data)) {
        return("long")
      }
      # Two or more numeric columns almost always means one column per
      # group; a single numeric column means the measurements are
      # stacked and something else labels them. The guess is stated in
      # the sidebar so a wrong one is easy to spot and override.
      if (length(numeric_cols()) >= 2) {
        return("wide")
      }
      return("long")
    })

    output$layout_ui <- shiny::renderUI({
      data <- raw()
      if (is.null(data) || ncol(data) == 0) {
        return(NULL)
      }
      nums <- numeric_cols()
      all_cols <- names(data)
      guess <- inferred_layout()
      layout_input <- shiny::radioButtons(
        ns("layout"),
        "How is your data arranged?",
        choices = c(
          "Long — one measurement column, one group column" = "long",
          "Wide — one column per group" = "wide"
        ),
        selected = guess
      )
      long_ui <- shiny::conditionalPanel(
        "input.layout == 'long'",
        ns = ns,
        shiny::selectInput(
          ns("value_col"),
          "Measurement column",
          choices = all_cols,
          selected = if (length(nums) > 0) nums[1] else all_cols[1]
        ),
        shiny::selectInput(
          ns("group_col"),
          "Group column",
          choices = all_cols,
          selected = {
            non_numeric <- setdiff(all_cols, nums)
            others <- setdiff(
              all_cols,
              if (length(nums) > 0) nums[1] else character(0)
            )
            if (length(non_numeric) > 0) {
              non_numeric[1]
            } else if (length(others) > 0) {
              others[1]
            } else {
              all_cols[1]
            }
          }
        )
      )
      wide_ui <- shiny::conditionalPanel(
        "input.layout == 'wide'",
        ns = ns,
        shiny::checkboxGroupInput(
          ns("wide_cols"),
          "Which columns are the groups?",
          choices = all_cols,
          selected = if (length(nums) > 0) nums else all_cols
        )
      )
      return(shiny::tagList(
        layout_input,
        long_ui,
        wide_ui,
        shiny::p(
          class = "hint",
          glue::glue(
            "The app guessed {guess} format from your column types. ",
            "Change it if that is wrong."
          )
        )
      ))
    })

    # ---- tidy ---------------------------------------------------------
    tidy_result <- shiny::reactive({
      data <- raw()
      if (is.null(data) || nrow(data) == 0) {
        return(NULL)
      }
      layout <- input$layout %||% inferred_layout()
      problems <- character(0)
      if (identical(layout, "wide")) {
        cols <- intersect(input$wide_cols %||% numeric_cols(), names(data))
        if (length(cols) < 1) {
          return(list(
            data = NULL,
            dropped = 0,
            problems = "Pick at least one group column."
          ))
        }
        # Stacked by hand rather than with tidyr::pivot_longer().
        # Column-major order also keeps each group's values in their
        # original row order, which is what the paired t-test needs.
        long <- tibble::tibble(
          group = factor(
            rep(cols, each = nrow(data)),
            levels = cols
          ),
          value = unlist(
            lapply(cols, function(column) data[[column]]),
            use.names = FALSE
          )
        )
      } else {
        nums <- numeric_cols()
        value_col <- input$value_col %||%
          (if (length(nums) > 0) nums[1] else names(data)[1])
        group_col <- input$group_col %||% names(data)[1]
        if (!all(c(value_col, group_col) %in% names(data))) {
          return(NULL)
        }
        if (identical(value_col, group_col)) {
          return(list(
            data = NULL,
            dropped = 0,
            problems = paste(
              "The measurement column and the group column cannot be",
              "the same column."
            )
          ))
        }
        long <- tibble::tibble(
          value = data[[value_col]],
          group = as.character(data[[group_col]])
        )
        long$group <- factor(
          long$group,
          levels = sort(unique(
            long$group[!is.na(long$group)]
          ))
        )
      }
      # Coerce and count what the coercion destroyed, so a column of
      # "12.1 cm" strings produces a message rather than silent NAs.
      before <- !is.na(long$value)
      long$value <- suppressWarnings(as.numeric(as.character(long$value)))
      broke <- sum(before & is.na(long$value))
      if (broke > 0) {
        problems <- c(
          problems,
          glue::glue(
            "{broke} value{ifelse(broke == 1, '', 's')} could not be read ",
            "as a number and {ifelse(broke == 1, 'was', 'were')} dropped. ",
            "Check for units, commas, or stray text in the measurement ",
            "column."
          )
        )
      }
      n_before <- nrow(long)
      long <- long[!is.na(long$value) & !is.na(long$group), ]
      long$group <- droplevels(long$group)
      dropped <- n_before - nrow(long)
      if (nrow(long) == 0) {
        problems <- c(problems, "No usable rows are left after cleaning.")
        return(list(data = NULL, dropped = dropped, problems = problems))
      }
      counts <- table(long$group)
      thin <- names(counts)[counts < 2]
      if (length(thin) > 0) {
        problems <- c(
          problems,
          glue::glue(
            "These groups have fewer than 2 values, so no spread can be ",
            "estimated for them: {paste(thin, collapse = ', ')}."
          )
        )
      }
      return(list(data = long, dropped = dropped, problems = problems))
    })

    tidy <- shiny::reactive({
      res <- tidy_result()
      if (is.null(res)) {
        return(NULL)
      }
      return(res$data)
    })

    # Original column names, so the analysis tabs can pre-fill axis
    # labels with the student's own wording instead of "Value".
    labels <- shiny::reactive({
      data <- raw()
      if (is.null(data)) {
        return(list(value = "Value", group = "Group"))
      }
      layout <- input$layout %||% inferred_layout()
      if (identical(layout, "wide")) {
        return(list(value = "Value", group = "Group"))
      }
      return(list(
        value = input$value_col %||% "Value",
        group = input$group_col %||% "Group"
      ))
    })

    # ---- alerts, preview, summary --------------------------------------
    output$alerts <- shiny::renderUI({
      res <- raw_result()
      if (!is.null(res$error)) {
        return(shiny::div(
          class = "alert-box alert-fail",
          shiny::strong("That file could not be read. "),
          shiny::span(res$error)
        ))
      }
      if (is.null(raw())) {
        return(shiny::div(
          class = "alert-box alert-info",
          "Choose a source on the left to load your measurements."
        ))
      }
      tr <- tidy_result()
      msgs <- list()
      if (!is.null(tr) && length(tr$problems) > 0) {
        msgs <- c(
          msgs,
          lapply(tr$problems, function(m) {
            shiny::div(class = "alert-box alert-warn", m)
          })
        )
      }
      if (!is.null(tr) && !is.null(tr$data)) {
        n_groups <- nlevels(tr$data$group)
        dropped_txt <- if (tr$dropped > 0) {
          glue::glue(" {tr$dropped} incomplete row(s) were left out.")
        } else {
          ""
        }
        msgs <- c(
          msgs,
          list(shiny::div(
            class = "alert-box alert-ok",
            glue::glue(
              "Ready: {nrow(tr$data)} measurements in {n_groups} ",
              "group{ifelse(n_groups == 1, '', 's')}.{dropped_txt}"
            )
          ))
        )
      }
      return(shiny::tagList(msgs))
    })

    output$preview <- DT::renderDT({
      data <- raw()
      shiny::req(data)
      DT::datatable(
        data,
        rownames = FALSE,
        options = list(pageLength = 8, scrollX = TRUE, dom = "tip")
      )
    })

    output$summary <- shiny::renderTable(
      {
        data <- tidy()
        shiny::req(data)
        summary_table(group_summary(data))
      },
      striped = TRUE,
      spacing = "xs",
      align = "lrrrrrr"
    )

    return(list(
      raw = raw,
      tidy = tidy,
      info = tidy_result,
      labels = labels
    ))
  })
}
