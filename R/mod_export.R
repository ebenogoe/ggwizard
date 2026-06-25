# Step 5 - Export ----------------------------------------------------------

#' @noRd
mod_export_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    shiny::div(
      class = "gw-step-header",
      shiny::h2(shiny::tagList(bsicons::bs_icon("download"), " Export Outputs")),
      shiny::p(
        "Choose where to save your outputs. Everything goes into a timestamped folder so runs never overwrite each other.",
        class = "text-muted"
      )
    ),

    bslib::layout_columns(
      col_widths = c(6, 6),

      # Left column
      bslib::card(
        bslib::card_header(shiny::strong("Save location and format")),
        bslib::card_body(

          # Output folder
          shiny::div(
            class = "mb-3",
            shiny::tags$label(
              gw_tooltip("Output folder", "ggWizard will create a timestamped subfolder here automatically"),
              class = "form-label fw-semibold"
            ),
            shiny::div(
              class = "d-flex gap-2",
              shinyFiles::shinyDirButton(
                ns("dir_btn"), "Browse", "Choose output folder",
                icon = shiny::icon("folder-open"),
                class = "btn-outline-secondary"
              ),
              shiny::verbatimTextOutput(ns("dir_display"), placeholder = TRUE)
            )
          ),

          # Format
          shiny::div(
            class = "mb-3",
            shinyWidgets::checkboxGroupButtons(
              inputId  = ns("formats"),
              label    = "File format",
              choices  = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg", "JPEG" = "jpeg"),
              selected = c("png", "pdf"),
              status   = "primary",
              size     = "sm",
              justified = FALSE
            )
          ),

          # DPI
          shiny::div(
            class = "mb-3",
            shinyWidgets::prettyRadioButtons(
              inputId  = ns("dpi"),
              label    = gw_tooltip("Quality / DPI", "Higher DPI gives sharper raster images (PNG, JPEG) but larger file sizes"),
              choices  = c("Low (150 dpi)" = "150", "Medium (300 dpi)" = "300", "High (600 dpi)" = "600"),
              selected = "300",
              inline   = TRUE,
              status   = "primary"
            )
          ),

          # Width and height
          bslib::layout_columns(
            col_widths = c(6, 6),
            shiny::numericInput(ns("width"),  "Width (inches)",  value = 8, min = 2, max = 30, step = 0.5),
            shiny::numericInput(ns("height"), "Height (inches)", value = 6, min = 2, max = 30, step = 0.5)
          ),

          # Additional outputs
          shiny::div(
            class = "mb-3",
            shiny::tags$label("Also save", class = "form-label fw-semibold d-block"),
            shinyWidgets::materialSwitch(ns("save_script"), "R script (plot_script.R)",     value = TRUE,  status = "primary"),
            shinyWidgets::materialSwitch(ns("save_log"),    "Run log (run_log.txt)",         value = TRUE,  status = "primary"),
            shinyWidgets::materialSwitch(ns("save_html"),   "Interactive HTML (plotly)",     value = FALSE, status = "primary")
          )
        )
      ),

      # Right column
      shiny::div(
        # Plot thumbnail
        bslib::card(
          bslib::card_header(shiny::strong("Final plot")),
          bslib::card_body(
            shiny::uiOutput(ns("plot_preview"))
          )
        ),
        # Log preview
        bslib::card(
          class = "mt-2",
          bslib::card_header(shiny::strong("Run log preview")),
          bslib::card_body(
            shiny::verbatimTextOutput(ns("log_preview"))
          )
        )
      )
    ),

    shiny::hr(),

    # Action buttons
    shiny::div(
      class = "d-flex gap-2 flex-wrap mb-3",
      shiny::actionButton(
        ns("save_all"), "Save all to folder",
        icon  = shiny::icon("floppy-disk"),
        class = "btn-success btn-lg"
      ),
      shiny::downloadButton(ns("dl_plot"),   "Download Plot",     class = "btn-outline-primary"),
      shiny::downloadButton(ns("dl_script"), "Download R Script", class = "btn-outline-primary"),
      shiny::downloadButton(ns("dl_log"),    "Download Log",      class = "btn-outline-primary")
    ),

    shiny::uiOutput(ns("save_feedback")),

    gw_nav_buttons(ns, back = TRUE, next_label = "Start over", next_icon = "house",
                    next_id = "restart_btn")
  )
}

#' @noRd
mod_export_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {

    # Directory picker
    shiny::observe({
      roots <- c(Home = fs::path_home(), getwd = getwd())
      shinyFiles::shinyDirChoose(input, "dir_btn", roots = roots, session = session)
    })

    chosen_dir <- shiny::reactive({
      req(input$dir_btn)
      if (is.integer(input$dir_btn)) return(NULL)
      roots <- c(Home = fs::path_home(), getwd = getwd())
      shinyFiles::parseDirPath(roots, input$dir_btn)
    })

    output$dir_display <- shiny::renderText({
      d <- chosen_dir()
      if (is.null(d) || length(d) == 0) "No folder selected" else d
    })

    output$plot_preview <- shiny::renderUI({
      req(rv$plot_obj)
      shiny::renderPlot(rv$plot_obj, height = 220)
    })

    output$log_preview <- shiny::renderText({
      req(rv$logger)
      paste(utils::tail(rv$logger$lines(), 15), collapse = "\n")
    })

    # Save all to folder
    shiny::observeEvent(input$save_all, {
      req(rv$plot_obj)
      dir <- chosen_dir()
      if (is.null(dir) || length(dir) == 0 || !nchar(dir)) {
        output$save_feedback <- shiny::renderUI(
          gw_error_card("Please select an output folder before saving.")
        )
        return()
      }
      if (!length(input$formats)) {
        output$save_feedback <- shiny::renderUI(
          gw_error_card("Please select at least one file format.")
        )
        return()
      }

      shinybusy::show_modal_spinner(spin = "fading-circle", color = "#4F46E5", text = "Saving outputs...")

      rv$logger$log(sprintf("Output folder selected: %s", dir))
      formats <- input$formats

      out_path <- tryCatch({
        write_outputs(
          plot_obj   = rv$plot_obj,
          run_dir    = dir,
          formats    = formats,
          dpi        = as.integer(input$dpi %||% 300),
          width      = input$width  %||% 8,
          height     = input$height %||% 6,
          script_txt = if (isTRUE(input$save_script)) rv$script_txt else NULL,
          logger     = rv$logger
        )
      }, error = function(e) {
        rv$logger$log(paste("Save failed:", conditionMessage(e)), "ERROR")
        NULL
      })

      # Save interactive HTML if requested
      if (isTRUE(input$save_html) && !is.null(out_path)) {
        tryCatch({
          html_path <- fs::path(out_path, "plot_interactive.html")
          htmlwidgets::saveWidget(plotly::ggplotly(rv$plot_obj), html_path, selfcontained = TRUE)
          rv$logger$log("Saved: plot_interactive.html", "OK")
        }, error = function(e) rv$logger$log(paste("HTML save failed:", conditionMessage(e)), "WARN"))
      }

      shinybusy::remove_modal_spinner()

      rv$run_dir <- out_path
      if (!is.null(out_path)) {
        output$save_feedback <- shiny::renderUI(
          gw_success_card(paste("All outputs saved to:", out_path))
        )
      } else {
        output$save_feedback <- shiny::renderUI(
          gw_error_card("Save failed. Check that the folder is writable and try again.")
        )
      }
    })

    # Individual download handlers
    output$dl_plot <- shiny::downloadHandler(
      filename = function() paste0("ggwizard_plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
      content  = function(file) {
        req(rv$plot_obj)
        ggplot2::ggsave(file, plot = rv$plot_obj,
                        dpi = as.integer(input$dpi %||% 300),
                        width = input$width %||% 8, height = input$height %||% 6, units = "in")
      }
    )

    output$dl_script <- shiny::downloadHandler(
      filename = function() "plot_script.R",
      content  = function(file) {
        req(rv$script_txt)
        writeLines(rv$script_txt, file)
      }
    )

    output$dl_log <- shiny::downloadHandler(
      filename = function() paste0("run_log_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt"),
      content  = function(file) {
        req(rv$logger)
        writeLines(rv$logger$lines(), file)
      }
    )

    shiny::observeEvent(input$restart_btn, {
      rv$screen     <- "welcome"
      rv$step       <- 1L
      rv$raw_data   <- NULL
      rv$col_types  <- NULL
      rv$file_info  <- list()
      rv$plot_cfg   <- list()
      rv$custom_cfg <- list()
      rv$plot_obj   <- NULL
      rv$interactive <- FALSE
      rv$run_dir    <- NULL
      rv$logger     <- NULL
      rv$script_txt <- NULL
    })

    shiny::observeEvent(input$back, rv$step <- 4L)
  })
}
