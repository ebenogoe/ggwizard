# Step 1 - Import Data -----------------------------------------------------

#' @noRd
mod_import_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    shiny::div(
      class = "gw-step-header",
      shiny::h2(shiny::tagList(
        bsicons::bs_icon("upload"), " Import Data"
      )),
      shiny::p(
        "Upload a CSV or Excel file. Multiple sheets are supported.",
        class = "text-muted"
      )
    ),

    bslib::layout_columns(
      col_widths = c(6, 6),
      # Left column - controls
      shiny::div(
        shiny::div(
          class = "gw-file-drop mb-3",
          shiny::fileInput(
            ns("file"),
            label    = NULL,
            accept   = c(".csv", ".xlsx", ".xls", "text/csv",
                         "application/vnd.ms-excel",
                         "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"),
            placeholder = "Drop a file here or click to browse",
            buttonLabel = shiny::tagList(bsicons::bs_icon("folder2-open"), " Browse")
          )
        ),
        bslib::card(
          bslib::card_header(shiny::strong("Import options")),
          bslib::card_body(
            shiny::div(
              class = "mb-3",
              shinyWidgets::prettyRadioButtons(
                inputId  = ns("format"),
                label    = gw_tooltip("File format", "Leave on Auto to detect from the file extension"),
                choices  = c("Auto-detect" = "auto", "CSV" = "csv", "Excel" = "excel"),
                selected = "auto",
                inline   = TRUE,
                status   = "primary"
              )
            ),
            shiny::conditionalPanel(
              condition = sprintf("output['%s'] == true", ns("is_excel_flag")),
              shiny::selectInput(
                ns("sheet"),
                label   = gw_tooltip("Sheet", "Applies to Excel files with multiple sheets"),
                choices = NULL
              )
            ),
            shiny::conditionalPanel(
              condition = sprintf("input['%s'] == 'csv' || (input['%s'] == 'auto' && output['%s'] == false)",
                                  ns("format"), ns("format"), ns("is_excel_flag")),
              shiny::selectInput(
                ns("delim"),
                label   = gw_tooltip("Column delimiter", "The character separating columns"),
                choices = c("Auto-detect" = "", "Comma (,)" = ",", "Semicolon (;)" = ";", "Tab" = "\t")
              )
            ),
            shinyWidgets::prettyRadioButtons(
              inputId  = ns("has_header"),
              label    = "Header row",
              choices  = c("Yes - first row is the header" = "yes", "No - no header row" = "no"),
              selected = "yes",
              inline   = FALSE,
              status   = "primary"
            )
          )
        )
      ),

      # Right column - format badges + feedback
      shiny::div(
        bslib::card(
          class = "h-100",
          bslib::card_header(shiny::strong("Supported formats")),
          bslib::card_body(
            shiny::div(
              class = "d-flex gap-2 flex-wrap mb-3",
              shiny::span(class = "badge bg-primary", "CSV"),
              shiny::span(class = "badge bg-primary", "XLSX"),
              shiny::span(class = "badge bg-primary", "XLS")
            ),
            shiny::tags$ul(
              class = "small text-muted ps-3",
              shiny::tags$li("Max file size: 50 MB"),
              shiny::tags$li("Dates are auto-detected"),
              shiny::tags$li("First 2,000 rows shown in preview"),
              shiny::tags$li("Multi-sheet Excel files supported")
            )
          )
        )
      )
    ),

    shiny::uiOutput(ns("feedback")),

    gw_nav_buttons(ns, back = FALSE, next_label = "Load and Continue", next_icon = "arrow-right", next_id = "load_btn")
  )
}

#' @noRd
mod_import_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {

    # Flag to conditionally show sheet picker / delimiter picker
    is_excel <- shiny::reactiveVal(FALSE)
    output$is_excel_flag <- shiny::reactive({
      req <- input$file
      if (is.null(req)) return(FALSE)
      fmt <- input$format
      if (fmt == "auto") tolower(tools::file_ext(req$name)) %in% c("xlsx", "xls")
      else fmt == "excel"
    })
    shiny::outputOptions(output, "is_excel_flag", suspendWhenHidden = FALSE)

    # When a file is uploaded, detect and populate sheet selector
    shiny::observeEvent(input$file, {
      req(input$file)
      fname <- input$file$name
      fmt   <- input$format
      if (fmt == "auto") fmt <- if (tolower(tools::file_ext(fname)) %in% c("xlsx", "xls")) "excel" else "csv"
      if (fmt == "excel") {
        sheets <- tryCatch(readxl::excel_sheets(input$file$datapath), error = function(e) NULL)
        if (!is.null(sheets)) shiny::updateSelectInput(session, "sheet", choices = sheets, selected = sheets[1])
        is_excel(TRUE)
      } else {
        is_excel(FALSE)
      }
    })

    output$feedback <- shiny::renderUI(NULL)

    shiny::observeEvent(input$load_btn, {
      req(input$file)
      shinybusy::show_modal_spinner(
        spin  = "fading-circle",
        color = "#4F46E5",
        text  = "Loading your data..."
      )
      rv$logger <- new_run_logger()
      rv$logger$section("ggWizard Run Log")
      rv$logger$log("Session started")

      result <- tryCatch({
        read_data(
          path       = input$file$datapath,
          format     = input$format,
          sheet      = input$sheet %||% 1,
          delim      = input$delim,
          has_header = input$has_header == "yes"
        )
      }, error = function(e) {
        list(error = conditionMessage(e))
      })

      shinybusy::remove_modal_spinner()

      if (!is.null(result$error)) {
        output$feedback <- shiny::renderUI(
          gw_error_card(paste(
            "The file could not be read.",
            "Check that it is a valid CSV or Excel file and is not open in another program.",
            "Detail:", result$error
          ))
        )
        rv$logger$log(paste("File read failed:", result$error), "ERROR")
        return()
      }

      rv$raw_data  <- result$data
      rv$col_types <- result$col_types
      rv$file_info <- list(
        name         = input$file$name,
        datapath     = input$file$datapath,
        format       = result$format,
        sheets       = result$sheets,
        sheet_used   = input$sheet %||% 1,
        nrow         = nrow(result$data),
        ncol         = ncol(result$data)
      )
      rv$logger$log(sprintf("File loaded: %s", input$file$name))
      rv$logger$log(sprintf("Format: %s%s", result$format,
                             if (!is.null(input$sheet)) paste0(" | Sheet: ", input$sheet) else ""))
      rv$logger$log(sprintf("Dimensions: %d rows x %d columns", nrow(result$data), ncol(result$data)))

      output$feedback <- shiny::renderUI(
        gw_success_card(sprintf(
          "File loaded: %s - %d rows and %d columns.",
          input$file$name, nrow(result$data), ncol(result$data)
        ))
      )

      shiny::req(rv$raw_data)
      rv$step <- 2L
    })

    shiny::observeEvent(input$back, rv$step <- 1L)
  })
}
