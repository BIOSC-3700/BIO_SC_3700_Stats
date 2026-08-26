# Small UI pieces shared by the analysis tabs.

# A status pill. Status colors always ship with a word, never color
# alone, so the meaning survives a colorblind reader or a grayscale
# printout.
status_badge <- function(status) {
  spec <- switch(
    status,
    ok = list(cls = "badge-ok", label = "OK"),
    warn = list(cls = "badge-warn", label = "Check this"),
    fail = list(cls = "badge-fail", label = "Problem"),
    list(cls = "badge-info", label = "Note")
  )
  return(shiny::span(class = paste("badge-check", spec$cls), spec$label))
}

# One assumption check, rendered as a labelled block.
check_block <- function(chk) {
  out <- shiny::div(
    class = "check-block",
    shiny::div(
      class = "check-head",
      status_badge(chk$status),
      shiny::span(class = "check-title", chk$title)
    ),
    shiny::p(class = "check-detail", chk$detail)
  )
  return(out)
}

# The headline sentence above every result table.
verdict_box <- function(p, alpha, headline) {
  out <- shiny::div(
    class = verdict_class(p, alpha),
    shiny::p(class = "verdict-headline", headline),
    shiny::p(class = "verdict-detail", verdict_text(p, alpha))
  )
  return(out)
}

# Shown on an analysis tab before any data has been loaded.
no_data_panel <- function(what) {
  out <- shiny::div(
    class = "empty-state",
    shiny::h4("No data loaded yet"),
    shiny::p(glue::glue(
      "Go to the Data tab and load your measurements, then come back ",
      "here to run {what}."
    ))
  )
  return(out)
}

# Shown when data exists but does not fit this particular test.
cannot_run_panel <- function(problems) {
  out <- shiny::div(
    class = "empty-state empty-state-warn",
    shiny::h4("This test cannot run on the current data"),
    shiny::tags$ul(lapply(problems, shiny::tags$li))
  )
  return(out)
}

# A plain two-column key/value results table.
kv_table <- function(keys, values) {
  rows <- Map(
    function(k, v) shiny::tags$tr(shiny::tags$th(k), shiny::tags$td(v)),
    keys,
    values
  )
  out <- shiny::tags$table(
    class = "kv-table",
    shiny::tags$tbody(rows)
  )
  return(out)
}

# Plot labels fall back to a sensible default until the student types
# something into the plot-options box.
label_or <- function(value, fallback) {
  if (is.null(value) || !nzchar(value)) {
    return(fallback)
  }
  return(value)
}

# Shared plot-appearance controls, used on all three analysis tabs.
plot_controls <- function(ns, style_choices = TRUE) {
  items <- list(
    shiny::textInput(ns("plot_title"), "Plot title", value = ""),
    shiny::textInput(ns("plot_xlab"), "X-axis label", value = ""),
    shiny::textInput(ns("plot_ylab"), "Y-axis label", value = "")
  )
  if (style_choices) {
    items <- c(
      list(shiny::radioButtons(
        ns("plot_style"),
        "Plot style",
        choices = c(
          "Boxplot" = "box",
          "Violin" = "violin",
          "Points only" = "points"
        ),
        selected = "box",
        inline = TRUE
      )),
      items
    )
  }
  return(items)
}

# Wraps a plot output plus its PNG download button.
plot_panel <- function(
  ns,
  height = "460px",
  plot_id = "plot",
  dl_id = "download_plot"
) {
  out <- shiny::tagList(
    shiny::plotOutput(ns(plot_id), height = height),
    shiny::div(
      class = "dl-row",
      shiny::downloadButton(
        ns(dl_id),
        "Download PNG (300 dpi)",
        class = "btn-sm btn-outline-secondary"
      )
    )
  )
  return(out)
}

# downloadHandler body shared by every tab.
plot_download_handler <- function(plot_fn, name) {
  out <- shiny::downloadHandler(
    filename = function() {
      return(glue::glue("{name}-{format(Sys.Date(), '%Y%m%d')}.png"))
    },
    content = function(file) {
      ggplot2::ggsave(
        file,
        plot = plot_fn(),
        width = 8,
        height = 5,
        dpi = 300,
        bg = pal$surface
      )
      return(invisible(NULL))
    }
  )
  return(out)
}

# Description block shown at the top of every statistics tab.
# Students must read this and click "Run analysis" before
# results appear.
test_intro <- function(title, description, hypotheses, ns, confirmed) {
  btn <- if (!confirmed) {
    shiny::actionButton(
      ns("run_analysis"),
      "Run analysis",
      class = "btn-primary mt-3"
    )
  }
  out <- shiny::div(
    class = "test-intro",
    shiny::h5(title),
    shiny::p(description),
    shiny::tags$dl(
      class = "hypotheses",
      hypotheses
    ),
    btn
  )
  return(out)
}

# Collapsible section showing the R code behind a result.
code_accordion <- function(code_text) {
  out <- bslib::accordion(
    open = FALSE,
    bslib::accordion_panel(
      "R code",
      shiny::tags$pre(
        class = "r-code",
        shiny::tags$code(code_text)
      )
    )
  )
  return(out)
}
