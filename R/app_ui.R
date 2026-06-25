#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    bslib::page_fluid(
      theme = gw_theme(),
      shinybusy::add_busy_bar(color = "#4F46E5", height = "4px"),
      gw_topbar(),
      bslib::navset_hidden(
        id = "main_nav",
        bslib::nav_panel_hidden("welcome", mod_welcome_ui("welcome")),
        bslib::nav_panel_hidden("wizard",  gw_wizard_ui()),
        bslib::nav_panel_hidden("help",    mod_help_ui("help")),
        bslib::nav_panel_hidden("about",   mod_about_ui("about"))
      )
    )
  )
}

#' Top navigation bar
#' @noRd
gw_topbar <- function() {
  shiny::div(
    class = "gw-topbar",
    shiny::actionLink(
      "brand_home", class = "gw-brand",
      shiny::tagList(
        fontawesome::fa("chart-bar", height = "1.1em"),
        " ggWizard"
      )
    ),
    shiny::span(class = "spacer"),
    shiny::uiOutput("nav_resume", inline = TRUE),
    shiny::actionLink("nav_home",  "Home",  class = "gw-nav-link"),
    shiny::actionLink("nav_help",  "Help",  class = "gw-nav-link"),
    shiny::actionLink("nav_about", "About", class = "gw-nav-link")
  )
}

#' The wizard layout: step sidebar + hidden step navset
#' @noRd
gw_wizard_ui <- function() {
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 230, open = "always", class = "gw-sidebar",
      shiny::div(class = "gw-sidebar-label", "Import wizard"),
      shiny::uiOutput("wizard_stepper")
    ),
    shiny::div(
      class = "gw-wizard-main",
      bslib::navset_hidden(
        id = "wizard_nav",
        bslib::nav_panel_hidden("s1", mod_import_ui("import")),
        bslib::nav_panel_hidden("s2", mod_preview_ui("preview")),
        bslib::nav_panel_hidden("s3", mod_plotconfig_ui("plotconfig")),
        bslib::nav_panel_hidden("s4", mod_customize_ui("customize")),
        bslib::nav_panel_hidden("s5", mod_export_ui("export"))
      )
    )
  )
}
