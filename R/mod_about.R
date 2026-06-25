# About screen -------------------------------------------------------------

#' @noRd
mod_about_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "gw-content-page",
    shiny::h2(shiny::tagList(fontawesome::fa("circle-info"), " About ggWizard")),
    shiny::p(
      "ggWizard is a point-and-click chart builder built on top of",
      shiny::tags$a(href = "https://ggplot2.tidyverse.org/", "ggplot2", target = "_blank"),
      "- the industry-standard R plotting library. It is designed for researchers,
      analysts, and students who want publication-quality charts without writing code.",
      class = "lead text-muted"
    ),

    shiny::hr(),

    shiny::h4("How it works"),
    shiny::p("You upload your data, map your columns to chart axes, and ggWizard assembles
             the ggplot2 code in the background. The app can also export the R script it used,
             so your workflow is fully reproducible."),

    shiny::h4("What ggWizard produces"),
    shiny::tags$ul(
      shiny::tags$li("A plot file in your chosen format (PNG, PDF, SVG, or JPEG)"),
      shiny::tags$li("A self-contained R script that reproduces the plot (optional)"),
      shiny::tags$li("A text run log covering every step of the session (optional)"),
      shiny::tags$li("An interactive HTML chart powered by plotly (optional)")
    ),

    shiny::h4("Technical details"),
    shiny::tags$ul(
      shiny::tags$li(shiny::strong("Backend:"), " R with ggplot2 for static charts, plotly for interactive charts"),
      shiny::tags$li(shiny::strong("Interface:"), " Shiny with bslib Bootstrap 5 theming"),
      shiny::tags$li(shiny::strong("Framework:"), " Golem - structured R package infrastructure for Shiny apps")
    ),

    shiny::h4("Found a bug?"),
    shiny::p(
      "Please report issues at",
      shiny::tags$a(href = "https://github.com/ebenogoe/ggwizard/issues",
                    "github.com/ebenogoe/ggwizard/issues", target = "_blank"),
      "and attach a copy of your run log (download it on the Export step or find it in your output folder)."
    ),
    shiny::p(
      "You can also email",
      shiny::tags$a(href = "mailto:ebenezerogoe@gmail.com", "ebenezerogoe@gmail.com"),
      "with a description of what went wrong."
    ),

    shiny::p(
      class = "text-muted small mt-4",
      sprintf("ggWizard version %s", tryCatch(as.character(utils::packageVersion("ggwizard")), error = function(e) "dev"))
    ),

    shiny::actionButton(ns("home"), shiny::tagList(bsicons::bs_icon("house"), " Back to home"),
                        class = "btn-outline-secondary mt-2")
  )
}

#' @noRd
mod_about_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(input$home, rv$screen <- "welcome")
  })
}
