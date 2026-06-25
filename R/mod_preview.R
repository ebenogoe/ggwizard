# Step 2 - Preview and Confirm ---------------------------------------------

#' @noRd
mod_preview_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    shiny::div(
      class = "gw-step-header",
      shiny::h2(shiny::tagList(bsicons::bs_icon("table"), " Preview and Confirm")),
      shiny::p(
        "Review your imported data before continuing. Verify the columns and data types look correct.",
        class = "text-muted"
      )
    ),

    # Metric tiles
    shiny::uiOutput(ns("metrics")),

    shiny::hr(),

    # Column type summary
    shiny::h5("Column Summary", class = "mt-3 mb-2 fw-semibold"),
    DT::dataTableOutput(ns("col_summary")),

    shiny::hr(),

    # Full data preview
    shiny::h5(
      gw_tooltip("Data Preview", "Showing the first 2,000 rows. Use the search box to filter."),
      class = "mt-3 mb-2 fw-semibold"
    ),
    DT::dataTableOutput(ns("data_preview")),

    gw_nav_buttons(ns, back = TRUE, next_label = "Continue", next_icon = "arrow-right")
  )
}

#' @noRd
mod_preview_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {

    output$metrics <- shiny::renderUI({
      req(rv$raw_data)
      s <- summarise_data(rv$raw_data, rv$col_types)

      pct_col <- if (s$pct_missing > 5) "#D97706" else if (s$pct_missing > 0) "#059669" else "#059669"

      shiny::div(
        class = "gw-metrics-row mb-3",
        gw_metric("Rows",         format(s$nrow, big.mark = ",")),
        gw_metric("Columns",      s$ncol),
        gw_metric("Numeric cols", s$n_numeric, "#059669"),
        gw_metric("Date cols",    s$n_date,    "#4F46E5"),
        gw_metric("Missing %",    paste0(s$pct_missing, "%"), pct_col)
      )
    })

    output$col_summary <- DT::renderDataTable({
      req(rv$raw_data, rv$col_types)
      df     <- rv$raw_data
      ctypes <- rv$col_types

      n_na  <- vapply(df, function(x) sum(is.na(x)), integer(1))
      n_uniq <- vapply(df, function(x) length(unique(x)), integer(1))
      sample_vals <- vapply(df, function(x) {
        v <- x[!is.na(x)]
        if (length(v) == 0) return(NA_character_)
        as.character(v[1])
      }, character(1))

      out <- data.frame(
        Column    = names(df),
        Type      = ctypes,
        `Non-missing` = paste0(round(100 * (1 - n_na / nrow(df)), 1), "%"),
        Unique    = format(n_uniq, big.mark = ","),
        Sample    = sample_vals,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      DT::datatable(
        out,
        rownames  = FALSE,
        selection = "none",
        options   = list(
          pageLength = 20,
          scrollX    = TRUE,
          dom        = "tp",
          columnDefs = list(list(className = "dt-left", targets = "_all"))
        )
      )
    })

    output$data_preview <- DT::renderDataTable({
      req(rv$raw_data)
      disp <- utils::head(rv$raw_data, 2000)
      DT::datatable(
        disp,
        rownames  = TRUE,
        filter    = "top",
        selection = "none",
        options   = list(
          pageLength  = 25,
          scrollX     = TRUE,
          scrollY     = "320px",
          scroller    = TRUE,
          dom         = "lfrtip"
        )
      )
    })

    shiny::observeEvent(input$next_btn, {
      req(rv$raw_data)
      rv$logger$log("Data preview confirmed", "OK")
      rv$step <- 3L
    })

    shiny::observeEvent(input$back, rv$step <- 1L)
  })
}
