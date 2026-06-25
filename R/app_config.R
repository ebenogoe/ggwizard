#' Add external resources to the application
#'
#' @import shiny
#' @importFrom golem add_resource_path bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path("www", app_sys("app/www"))
  tags$head(
    tags$link(rel = "icon", type = "image/svg+xml", href = "www/ggwizard.svg"),
    bundle_resources(path = app_sys("app/www"), app_title = "ggWizard"),
    shinyjs::useShinyjs()
  )
}

#' The bslib Bootstrap 5 theme for ggWizard
#' @noRd
gw_theme <- function() {
  bslib::bs_theme(
    version    = 5,
    primary    = "#4F46E5",
    secondary  = "#6B7280",
    success    = "#059669",
    info       = "#0891B2",
    warning    = "#D97706",
    danger     = "#DC2626",
    font_scale = 1.05,
    base_font  = bslib::font_collection(
      bslib::font_google("Plus Jakarta Sans", local = FALSE),
      "system-ui", "sans-serif"
    )
  )
}
