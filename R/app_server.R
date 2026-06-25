#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  rv <- shiny::reactiveValues(
    screen     = "welcome",
    step       = 1L,
    raw_data   = NULL,
    col_types  = NULL,
    file_info  = list(),
    plot_cfg   = list(),
    custom_cfg = list(),
    plot_obj   = NULL,
    interactive = FALSE,
    run_dir    = NULL,
    logger     = NULL,
    script_txt = NULL
  )

  # Screen modules
  mod_welcome_server("welcome",   rv)
  mod_import_server("import",     rv)
  mod_preview_server("preview",   rv)
  mod_plotconfig_server("plotconfig", rv)
  mod_customize_server("customize",   rv)
  mod_export_server("export",     rv)
  mod_help_server("help",         rv)
  mod_about_server("about",       rv)

  # Top-bar navigation
  shiny::observeEvent(input$brand_home, rv$screen <- "welcome")
  shiny::observeEvent(input$nav_home,   rv$screen <- "welcome")
  shiny::observeEvent(input$nav_help,   rv$screen <- "help")
  shiny::observeEvent(input$nav_about,  rv$screen <- "about")

  # Resume button: visible when the user has wizard progress but is away from the wizard
  output$nav_resume <- shiny::renderUI({
    has_progress <- !is.null(rv$raw_data) || rv$step > 1L
    if (rv$screen == "wizard" || !has_progress) return(NULL)
    shiny::actionButton(
      "nav_resume_btn",
      shiny::tagList(
        fontawesome::fa("circle-play", height = "0.9em"),
        shiny::span(sprintf(" Resume (Step %d)", rv$step))
      ),
      class = "gw-resume-btn"
    )
  })

  shiny::observeEvent(input$nav_resume_btn, rv$screen <- "wizard")

  # Drive the top-level screen navset
  shiny::observeEvent(rv$screen, {
    bslib::nav_select("main_nav", rv$screen, session = session)
  })

  # Drive the wizard step navset (screen switch is handled by each module via rv$screen)
  shiny::observeEvent(rv$step, {
    bslib::nav_select("wizard_nav", paste0("s", rv$step), session = session)
    shinyjs::runjs("setTimeout(function(){
      var m=document.querySelector('.bslib-sidebar-layout > .bslib-main');
      if(m) m.scrollTop=0; window.scrollTo(0,0);
    },50);")
  })

  # Render wizard stepper in the sidebar
  output$wizard_stepper <- shiny::renderUI(gw_stepper(rv$step))
}
