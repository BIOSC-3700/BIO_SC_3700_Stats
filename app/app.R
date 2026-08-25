# BIO_SC 3700 — Basic Statistics
#
# A point-and-click app for two-sample t-tests, one-sample t-tests, and
# one-way ANOVA. Runs in the browser via shinylive, so keep the package
# list short: every library() here is a download on a student's first
# visit.
#
# Files in R/ are sourced automatically by Shiny.

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(tibble)
library(readr)
library(readxl)
library(DT)
library(glue)
library(fs)

# The default Bootstrap 5 preset ships precompiled with bslib. Passing
# theme arguments here would pull in a runtime Sass compile, which is
# slow under wasm, so all custom styling lives in www/styles.css.
app_theme <- bs_theme(version = 5)

# Set to FALSE to hide the automatic plots on the statistics
# tabs. The standalone Plot tab is unaffected.
show_stat_plots <- TRUE

help_panel <- function() {
  out <- div(
    class = "help-body",
    h3("How to use this app"),
    tags$ol(
      tags$li(paste(
        "On the Data tab, load your measurements — upload a file, paste",
        "from a spreadsheet, or start from an example."
      )),
      tags$li(paste(
        "Tell the app how your data is arranged. Long format means one",
        "column of measurements and one column of group labels. Wide",
        "format means one column per group."
      )),
      tags$li(paste(
        "Check the preview table. If the numbers there are not what you",
        "expect, fix that before going any further."
      )),
      tags$li(paste(
        "Pick the tab for your test, set the options, and read the",
        "assumptions panel before the p-value."
      ))
    ),
    h3("Which test do I want?"),
    tags$ul(
      tags$li(tags$b("One-sample t-test: "), paste(
        "one group of measurements compared against a fixed number you",
        "specify — an expected value from theory or a published result."
      )),
      tags$li(tags$b("Two-sample t-test: "), paste(
        "two groups compared against each other. Use the paired option",
        "when the same individual was measured twice."
      )),
      tags$li(tags$b("One-way ANOVA: "), paste(
        "three or more groups of a single factor compared at once. Add",
        "Tukey's HSD to find out which particular pairs differ."
      )),
      tags$li(tags$b("Two-way ANOVA: "), paste(
        "two factors crossed with each other — genotype and treatment,",
        "site and season. Tests each factor and, crucially, whether",
        "they interact."
      ))
    ),
    h3("Reading an interaction"),
    p(paste(
      "In a two-way ANOVA the interaction is the first thing to look",
      "at, not the last. It asks whether the effect of one factor",
      "depends on the level of the other. If it does, the main effects",
      "become misleading on their own: a main effect averages over a",
      "factor it interacts with, which can hide an effect that is",
      "large in one condition and absent in another, or report an",
      "overall difference that exists in neither."
    )),
    p(paste(
      "The interaction plot is the quickest way to see it. Parallel",
      "lines mean the factors act independently. Lines that converge,",
      "diverge, or cross mean they do not, and the comparison worth",
      "making is between individual combinations."
    )),
    h3("A warning about p-values"),
    p(paste(
      "A small p-value says your data would be surprising if the null",
      "hypothesis were true. It does not tell you the effect is large,",
      "or important, or that you have proved anything. A large p-value",
      "is not evidence that the groups are the same — it often just",
      "means the sample was too small to tell. Report the confidence",
      "interval alongside the p-value; it carries more information."
    )),
    h3("Trouble"),
    tags$ul(
      tags$li(paste(
        "Numbers read as text: check for units, commas, or footnote",
        "marks inside the measurement column."
      )),
      tags$li(paste(
        "Groups you did not expect: trailing spaces and inconsistent",
        "capitalization make 'Control' and 'control ' two groups."
      )),
      tags$li(paste(
        "Nothing happens after pasting: make sure you included the",
        "header row and pressed 'Use this data'."
      ))
    )
  )
  return(out)
}

ui <- page_navbar(
  title = "BIO_SC 3700 — Basic Statistics",
  theme = app_theme,
  fillable = FALSE,
  header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  nav_panel("Data", mod_data_ui("data")),
  nav_panel("Plot", mod_plot_ui("plot")),
  nav_panel("Two-sample t-test", mod_ttest2_ui("t2")),
  nav_panel("One-sample t-test", mod_ttest1_ui("t1")),
  nav_panel("One-way ANOVA", mod_anova_ui("anova")),
  nav_panel("Two-way ANOVA", mod_anova2_ui("anova2")),
  nav_spacer(),
  nav_panel("Help", help_panel())
)

server <- function(input, output, session) {
  data <- mod_data_server("data")
  mod_ttest2_server("t2", data, show_stat_plots)
  mod_ttest1_server("t1", data, show_stat_plots)
  mod_anova_server("anova", data, show_stat_plots)
  mod_anova2_server("anova2", data, show_stat_plots)
  mod_plot_server("plot", data)
}

shinyApp(ui, server)
