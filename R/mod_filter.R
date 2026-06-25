# Step 3 - Filter & QC --------------------------------------------------------

#' @noRd
mod_filter_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(
    shiny::div(
      class = "gw-step-header",
      shiny::h2(shiny::tagList(bsicons::bs_icon("funnel-fill"), " Filter & QC")),
      shiny::p(
        "Optionally clean your data before plotting. All actions are reversible. Click Skip to continue with the original data.",
        class = "text-muted"
      )
    ),

    bslib::layout_columns(
      col_widths = c(5, 7),

      # -- Left: filter controls ---------------------------------------------
      bslib::accordion(
        id       = ns("filter_acc"),
        open     = "Column selection",
        multiple = TRUE,

        # Column selection ---------------------------------------------------
        bslib::accordion_panel(
          "Column selection",
          icon = bsicons::bs_icon("columns"),
          shiny::p("Uncheck any columns you want to exclude from your analysis.", class = "text-muted small"),
          shiny::div(
            class = "d-flex gap-2 mb-2",
            shiny::actionButton(ns("col_all"),  "Select all",   class = "btn-outline-secondary btn-sm"),
            shiny::actionButton(ns("col_none"), "Deselect all", class = "btn-outline-secondary btn-sm")
          ),
          shiny::uiOutput(ns("col_checkboxes"))
        ),

        # Outlier detection --------------------------------------------------
        bslib::accordion_panel(
          "Outlier detection (Z-score)",
          icon = bsicons::bs_icon("activity"),
          shiny::p(
            "Flag rows where any selected numeric column has |z-score| above the threshold. ",
            "Outliers are investigated rather than automatically removed -- review the count before deciding.",
            class = "text-muted small"
          ),
          shiny::uiOutput(ns("outlier_col_ui")),
          shiny::sliderInput(
            ns("z_threshold"), "SD threshold",
            min = 1.5, max = 5, value = 3, step = 0.5
          ),
          shiny::actionButton(
            ns("detect_outliers"), "Detect outliers",
            icon = shiny::icon("magnifying-glass"), class = "btn-outline-primary btn-sm mb-2"
          ),
          shiny::uiOutput(ns("outlier_results"))
        ),

        # Replicate CV QC ----------------------------------------------------
        bslib::accordion_panel(
          "Replicate variance (CV%)",
          icon = bsicons::bs_icon("bar-chart-line"),
          shiny::p(
            "For replicated measurements, the coefficient of variation (CV = SD / mean x 100%) ",
            "is computed within each group. High CV may indicate measurement error, equipment malfunction, or contamination.",
            class = "text-muted small"
          ),
          shiny::uiOutput(ns("qc_col_ui")),
          shiny::numericInput(
            ns("cv_threshold"),
            label = gw_tooltip("CV threshold (%)", "Groups with CV above this value will be flagged"),
            value = 30, min = 1, max = 500, step = 1
          ),
          shiny::actionButton(
            ns("detect_qc"), "Run QC check",
            icon = shiny::icon("magnifying-glass"), class = "btn-outline-primary btn-sm mb-2"
          ),
          shiny::uiOutput(ns("qc_summary_ui")),
          shiny::tableOutput(ns("qc_tbl"))
        )
      ),

      # -- Right: summary + preview ------------------------------------------
      shiny::div(
        bslib::card(
          bslib::card_header(
            shiny::div(
              class = "d-flex justify-content-between align-items-center",
              shiny::strong("Filter summary"),
              shiny::actionButton(
                ns("reset_filters"),
                shiny::tagList(bsicons::bs_icon("arrow-counterclockwise"), " Reset all"),
                class = "btn-outline-danger btn-sm"
              )
            )
          ),
          bslib::card_body(shiny::uiOutput(ns("filter_summary")))
        ),
        bslib::card(
          class = "mt-2",
          bslib::card_header(shiny::strong("Data preview (first 100 rows)")),
          bslib::card_body(DT::DTOutput(ns("preview_tbl")))
        )
      )
    ),

    shiny::hr(),

    shiny::div(
      class = "d-flex justify-content-between align-items-center mt-2 mb-3",
      shiny::div(
        class = "d-flex gap-2",
        shiny::actionButton(
          ns("back"),
          shiny::tagList(bsicons::bs_icon("arrow-left"), " Back"),
          class = "btn-outline-secondary"
        ),
        shiny::downloadButton(ns("dl_filtered"), "Download filtered data", class = "btn-outline-primary")
      ),
      shiny::div(
        class = "d-flex gap-2",
        shiny::actionButton(
          ns("skip"),
          shiny::tagList(bsicons::bs_icon("skip-forward-fill"), " Skip"),
          class = "btn-outline-secondary"
        ),
        shiny::actionButton(
          ns("next_btn"),
          shiny::tagList("Apply & Continue ", bsicons::bs_icon("arrow-right")),
          class = "btn-primary"
        )
      )
    )
  )
}


#' @noRd
mod_filter_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {

    # -- Internal filter state ------------------------------------------------
    filter_rv <- shiny::reactiveValues(
      excluded_cols  = character(0),
      outlier_rows   = integer(0),
      removed_groups = character(0),
      qc_group_col   = NULL
    )

    outlier_res <- shiny::reactiveVal(NULL)   # list(n, rows) or NULL
    qc_res      <- shiny::reactiveVal(NULL)   # list(tbl, threshold, g_col) or NULL


    # -- Column selection -----------------------------------------------------
    output$col_checkboxes <- shiny::renderUI({
      req(rv$raw_data_original)
      cols  <- names(rv$raw_data_original)
      types <- rv$col_types_original %||% rv$col_types %||% stats::setNames(rep("?", length(cols)), cols)

      badge_cls <- function(t) switch(t,
        "numeric"   = "bg-primary",
        "date"      = "bg-success",
        "factor"    = "bg-warning text-dark",
        "character" = "bg-secondary",
        "logical"   = "bg-info text-dark",
        "bg-light text-dark border"
      )

      labels <- lapply(cols, function(col) {
        tp <- if (col %in% names(types)) types[[col]] else "?"
        shiny::tagList(
          col, " ",
          shiny::tags$span(class = paste("badge small", badge_cls(tp)), tp)
        )
      })

      selected_cols <- setdiff(cols, filter_rv$excluded_cols)

      shiny::checkboxGroupInput(
        session$ns("col_include"),
        label        = NULL,
        choiceNames  = labels,
        choiceValues = cols,
        selected     = selected_cols
      )
    })

    shiny::observeEvent(input$col_include, {
      req(rv$raw_data_original)
      all_cols <- names(rv$raw_data_original)
      filter_rv$excluded_cols <- setdiff(all_cols, input$col_include %||% character(0))
    }, ignoreNULL = FALSE)

    shiny::observeEvent(input$col_all, {
      req(rv$raw_data_original)
      shiny::updateCheckboxGroupInput(session, "col_include",
                                      selected = names(rv$raw_data_original))
    })

    shiny::observeEvent(input$col_none, {
      shiny::updateCheckboxGroupInput(session, "col_include", selected = character(0))
    })


    # -- Outlier detection ----------------------------------------------------
    output$outlier_col_ui <- shiny::renderUI({
      req(rv$raw_data_original, rv$col_types_original %||% rv$col_types)
      types    <- rv$col_types_original %||% rv$col_types
      num_cols <- names(types)[types == "numeric"]
      if (!length(num_cols))
        return(shiny::p("No numeric columns detected.", class = "text-muted small"))
      shiny::selectInput(
        session$ns("outlier_cols"),
        label    = gw_tooltip("Columns to scan", "Z-scores are computed per column. A row is flagged if it exceeds the threshold in ANY selected column."),
        choices  = num_cols,
        selected = num_cols,
        multiple = TRUE
      )
    })

    shiny::observeEvent(input$detect_outliers, {
      req(rv$raw_data_original, input$outlier_cols, length(input$outlier_cols) > 0)
      data      <- rv$raw_data_original
      threshold <- input$z_threshold %||% 3

      flag_mask <- rep(FALSE, nrow(data))
      for (col in input$outlier_cols) {
        vals  <- data[[col]]
        mu    <- mean(vals, na.rm = TRUE)
        sigma <- stats::sd(vals,   na.rm = TRUE)
        if (is.na(sigma) || sigma == 0) next
        z          <- abs((vals - mu) / sigma)
        flag_mask  <- flag_mask | (!is.na(z) & z > threshold)
      }

      rows <- which(flag_mask)
      outlier_res(list(n = length(rows), rows = rows, threshold = threshold,
                       cols = input$outlier_cols))
    })

    output$outlier_results <- shiny::renderUI({
      res <- outlier_res()
      if (is.null(res)) return(NULL)

      if (res$n == 0)
        return(gw_success_card(sprintf(
          "No outliers found (|z| > %.1f) in: %s",
          res$threshold, paste(res$cols, collapse = ", ")
        )))

      shiny::tagList(
        shiny::div(
          class = "alert alert-warning p-2 small mb-2",
          bsicons::bs_icon("exclamation-triangle-fill"), " ",
          sprintf("%d row%s flagged as potential outlier%s (|z| > %.1f)",
                  res$n, if (res$n == 1) "" else "s",
                  if (res$n == 1) "" else "s", res$threshold)
        ),
        shiny::div(
          class = "d-flex gap-2",
          shiny::actionButton(
            session$ns("remove_outliers"),
            sprintf("Remove %d row%s", res$n, if (res$n == 1) "" else "s"),
            class = "btn-danger btn-sm"
          ),
          shiny::actionButton(session$ns("keep_outliers"), "Keep all rows",
                              class = "btn-outline-secondary btn-sm")
        )
      )
    })

    shiny::observeEvent(input$remove_outliers, {
      res <- outlier_res()
      if (!is.null(res)) filter_rv$outlier_rows <- res$rows
      outlier_res(NULL)
    })

    shiny::observeEvent(input$keep_outliers, outlier_res(NULL))


    # -- Replicate CV QC ------------------------------------------------------
    output$qc_col_ui <- shiny::renderUI({
      req(rv$raw_data_original)
      cols  <- names(rv$raw_data_original)
      types <- rv$col_types_original %||% rv$col_types %||% stats::setNames(rep("?", length(cols)), cols)

      num_cols <- names(types)[types == "numeric"]
      cat_cols <- setdiff(cols, num_cols)
      if (!length(cat_cols)) cat_cols <- cols

      shiny::tagList(
        shiny::selectInput(
          session$ns("qc_group_col"),
          label   = gw_tooltip("Grouping column", "Column identifying replicate groups (e.g. genotype, treatment, sample ID)"),
          choices = cat_cols, selected = cat_cols[1]
        ),
        shiny::selectInput(
          session$ns("qc_measure_col"),
          label   = gw_tooltip("Measurement column", "Numeric column to compute CV for within each group"),
          choices  = if (length(num_cols)) num_cols else cols,
          selected = if (length(num_cols)) num_cols[1] else cols[1]
        )
      )
    })

    shiny::observeEvent(input$detect_qc, {
      req(rv$raw_data_original, input$qc_group_col, input$qc_measure_col)
      data      <- rv$raw_data_original
      g_col     <- input$qc_group_col
      m_col     <- input$qc_measure_col
      threshold <- input$cv_threshold %||% 30

      if (!is.numeric(data[[m_col]])) {
        qc_res(list(error = sprintf("'%s' is not a numeric column.", m_col)))
        return()
      }

      groups <- unique(data[[g_col]])
      rows   <- lapply(groups, function(g) {
        vals <- data[[m_col]][data[[g_col]] == g]
        vals <- vals[!is.na(vals)]
        if (length(vals) < 2) return(NULL)
        mu  <- mean(vals)
        sig <- stats::sd(vals)
        cv  <- if (abs(mu) > 1e-10) sig / abs(mu) * 100 else NA_real_
        data.frame(
          Group    = as.character(g),
          N        = length(vals),
          Mean     = round(mu, 3),
          SD       = round(sig, 3),
          CV_pct   = round(cv, 1),
          Flagged  = !is.na(cv) && cv > threshold,
          stringsAsFactors = FALSE
        )
      })
      tbl <- do.call(rbind, Filter(Negate(is.null), rows))
      if (is.null(tbl) || nrow(tbl) == 0) {
        qc_res(list(error = "No groups with 2 or more non-missing measurements found."))
        return()
      }
      qc_res(list(tbl = tbl, threshold = threshold, g_col = g_col))
    })

    output$qc_summary_ui <- shiny::renderUI({
      res <- qc_res()
      if (is.null(res)) return(NULL)
      if (!is.null(res$error)) return(gw_error_card(res$error))

      tbl    <- res$tbl
      n_flag <- sum(tbl$Flagged, na.rm = TRUE)

      shiny::tagList(
        if (n_flag == 0) {
          gw_success_card(sprintf("All groups have CV <= %.0f%%", res$threshold))
        } else {
          shiny::div(
            class = "alert alert-warning p-2 small mb-2",
            bsicons::bs_icon("exclamation-triangle-fill"), " ",
            sprintf("%d of %d group%s exceed CV > %.0f%%: %s",
                    n_flag, nrow(tbl), if (nrow(tbl) == 1) "" else "s",
                    res$threshold,
                    paste(tbl$Group[tbl$Flagged], collapse = ", "))
          )
        },
        if (n_flag > 0) {
          shiny::div(
            class = "d-flex gap-2 mb-2",
            shiny::actionButton(
              session$ns("remove_qc"),
              sprintf("Remove %d group%s", n_flag, if (n_flag == 1) "" else "s"),
              class = "btn-danger btn-sm"
            ),
            shiny::actionButton(session$ns("keep_qc"), "Keep all groups",
                                class = "btn-outline-secondary btn-sm")
          )
        }
      )
    })

    output$qc_tbl <- shiny::renderTable({
      res <- qc_res()
      if (is.null(res) || !is.null(res$error)) return(NULL)
      tbl <- res$tbl
      tbl$CV_pct  <- sprintf("%.1f%%", tbl$CV_pct)
      tbl$Flagged <- ifelse(tbl$Flagged, "Yes", "")
      names(tbl) <- c("Group", "N", "Mean", "SD", "CV%", "Flagged")
      tbl
    }, striped = TRUE, hover = TRUE, bordered = TRUE, digits = 3)

    shiny::observeEvent(input$remove_qc, {
      res <- qc_res()
      if (is.null(res) || is.null(res$tbl)) return()
      filter_rv$removed_groups <- res$tbl$Group[res$tbl$Flagged]
      filter_rv$qc_group_col   <- res$g_col
      qc_res(NULL)
    })

    shiny::observeEvent(input$keep_qc, qc_res(NULL))


    # -- Filtered data --------------------------------------------------------
    filtered_data <- shiny::reactive({
      data <- rv$raw_data_original
      req(data)

      # 1. Column exclusion
      excl <- filter_rv$excluded_cols
      if (length(excl)) {
        keep <- setdiff(names(data), excl)
        if (length(keep) > 0) data <- data[, keep, drop = FALSE]
      }

      # 2. Outlier row removal
      if (length(filter_rv$outlier_rows)) {
        valid <- setdiff(seq_len(nrow(rv$raw_data_original)), filter_rv$outlier_rows)
        data  <- data[valid, , drop = FALSE]
      }

      # 3. QC group removal
      g_col <- filter_rv$qc_group_col
      if (length(filter_rv$removed_groups) && !is.null(g_col) && g_col %in% names(data)) {
        keep_rows <- !(as.character(data[[g_col]]) %in% filter_rv$removed_groups)
        data      <- data[keep_rows, , drop = FALSE]
      }

      data
    })


    # -- Filter summary card --------------------------------------------------
    output$filter_summary <- shiny::renderUI({
      orig <- rv$raw_data_original
      req(orig)
      curr       <- filtered_data()
      orig_r     <- nrow(orig); orig_c <- ncol(orig)
      curr_r     <- nrow(curr); curr_c <- ncol(curr)
      rows_rmvd  <- orig_r - curr_r
      cols_rmvd  <- orig_c - curr_c

      changes <- character(0)
      if (rows_rmvd > 0)
        changes <- c(changes, sprintf("%d row%s removed (%.1f%%)",
                                      rows_rmvd, if (rows_rmvd == 1) "" else "s",
                                      rows_rmvd / orig_r * 100))
      if (cols_rmvd > 0)
        changes <- c(changes, sprintf("%d column%s excluded",
                                      cols_rmvd, if (cols_rmvd == 1) "" else "s"))

      active <- list()
      if (length(filter_rv$excluded_cols))
        active[[length(active)+1]] <- shiny::tags$li(sprintf(
          "Columns excluded: %s", paste(filter_rv$excluded_cols, collapse = ", ")
        ))
      if (length(filter_rv$outlier_rows))
        active[[length(active)+1]] <- shiny::tags$li(sprintf(
          "Outlier rows removed: %d", length(filter_rv$outlier_rows)
        ))
      if (length(filter_rv$removed_groups))
        active[[length(active)+1]] <- shiny::tags$li(sprintf(
          "QC groups removed: %s", paste(filter_rv$removed_groups, collapse = ", ")
        ))

      shiny::tagList(
        shiny::div(
          class = "d-flex align-items-center gap-4 mb-3",
          shiny::div(
            shiny::div(class = "gw-metric-label", "Original"),
            shiny::div(class = "gw-metric-value",
                       style = "font-size:18px",
                       sprintf("%s x %d", format(orig_r, big.mark=","), orig_c))
          ),
          shiny::span(class = "text-muted fs-5", "->"),
          shiny::div(
            shiny::div(class = "gw-metric-label", "After filters"),
            shiny::div(class = "gw-metric-value",
                       style = paste0("font-size:18px;", if (rows_rmvd>0||cols_rmvd>0) "color:#D97706" else ""),
                       sprintf("%s x %d", format(curr_r, big.mark=","), curr_c))
          )
        ),
        if (length(changes) > 0) {
          shiny::div(class = "alert alert-warning p-2 small mb-2",
                     paste(changes, collapse = " | "))
        } else {
          shiny::p("No active filters. Original data will be used.", class = "text-muted small mb-2")
        },
        if (length(active)) {
          shiny::tagList(
            shiny::tags$strong("Active filters:", class = "small d-block mb-1"),
            shiny::tags$ul(class = "small mb-0 ps-3", active)
          )
        }
      )
    })


    # -- Data preview ---------------------------------------------------------
    output$preview_tbl <- DT::renderDT({
      data <- filtered_data()
      req(data)
      DT::datatable(
        utils::head(data, 100),
        rownames = FALSE,
        options  = list(dom = "tp", pageLength = 10, scrollX = TRUE)
      )
    }, server = FALSE)


    # -- Download filtered data ------------------------------------------------
    output$dl_filtered <- shiny::downloadHandler(
      filename = function() {
        paste0("ggwizard_filtered_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        utils::write.csv(filtered_data(), file, row.names = FALSE)
      }
    )


    # -- Reset ----------------------------------------------------------------
    shiny::observeEvent(input$reset_filters, {
      filter_rv$excluded_cols  <- character(0)
      filter_rv$outlier_rows   <- integer(0)
      filter_rv$removed_groups <- character(0)
      filter_rv$qc_group_col   <- NULL
      outlier_res(NULL)
      qc_res(NULL)
      req(rv$raw_data_original)
      shiny::updateCheckboxGroupInput(session, "col_include",
                                      selected = names(rv$raw_data_original))
    })


    # -- Navigation -----------------------------------------------------------
    shiny::observeEvent(input$back, rv$step <- 2L)

    push_data <- function(data) {
      rv$raw_data  <- data
      orig_types   <- rv$col_types_original %||% rv$col_types
      rv$col_types <- orig_types[names(orig_types) %in% names(data)]
    }

    shiny::observeEvent(input$skip, {
      push_data(rv$raw_data_original)
      rv$step <- 4L
    })

    shiny::observeEvent(input$next_btn, {
      push_data(filtered_data())
      rv$step <- 4L
    })
  })
}
