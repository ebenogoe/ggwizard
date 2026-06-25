# Welcome screen ------------------------------------------------------------

#' @noRd
mod_welcome_ui <- function(id) {
  ns <- shiny::NS(id)

  action_card <- function(action_id, fa_icon, title, subtitle, colour, bg_colour) {
    shiny::actionLink(
      ns(action_id), class = "text-decoration-none",
      bslib::card(
        class = "gw-card-action h-100",
        bslib::card_body(
          class = "text-center",
          shiny::div(
            class = "gw-card-icon mx-auto",
            style = paste0("color:", colour, "; background:", bg_colour),
            fontawesome::fa(fa_icon, height = "1.4em")
          ),
          shiny::h4(title, class = "mt-3 mb-1 fw-bold"),
          shiny::p(subtitle, class = "text-muted mb-0 small")
        )
      )
    )
  }

  shiny::div(
    class = "gw-welcome-wrap",
    shiny::div(class = "gw-hero-bg"),
    shiny::div(
      class = "gw-hero",
      shiny::div(
        class = "gw-hero-icon",
        fontawesome::fa("chart-bar", height = "2em", fill = "white")
      ),
      shiny::h1("ggWizard", class = "gw-hero-title"),
      shiny::p(
        "Create beautiful ggplot2 charts from your data - no R coding required.",
        class = "gw-hero-tagline lead"
      )
    ),
    bslib::layout_columns(
      col_widths = c(4, 4, 4),
      action_card("get_started", "rocket",         "Get Started",  "Open the plot wizard",       "#4F46E5", "#EEF2FF"),
      action_card("go_help",     "circle-question", "Help and Docs", "Guides for first-time users", "#059669", "#ECFDF5"),
      action_card("go_about",    "circle-info",    "About",        "Credits and bug reports",    "#D97706", "#FFFBEB")
    )
  )
}

#' @noRd
mod_welcome_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(input$get_started, {
      rv$step   <- 1L
      rv$screen <- "wizard"
    })
    shiny::observeEvent(input$go_help,  rv$screen <- "help")
    shiny::observeEvent(input$go_about, rv$screen <- "about")
  })
}
