#' Run the ggWizard Shiny Application
#'
#' Launches the ggWizard point-and-click chart builder in a browser window.
#'
#' @param ... Arguments passed to [golem::get_golem_options()].
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(
  onStart          = NULL,
  options          = list(),
  enableBookmarking = NULL,
  uiPattern        = "/",
  ...
) {
  with_golem_options(
    app = shinyApp(
      ui               = app_ui,
      server           = app_server,
      onStart          = onStart,
      options          = options,
      enableBookmarking = enableBookmarking,
      uiPattern        = uiPattern
    ),
    golem_opts = list(...)
  )
}
