# Step 3 - Configure Plot --------------------------------------------------

PLOT_TYPES <- c("Bar", "Line", "Time Series", "Scatter",
                "Histogram", "Box", "Violin", "Area")

#' @noRd
mod_plotconfig_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    shiny::div(
      class = "gw-step-header",
      shiny::h2(shiny::tagList(bsicons::bs_icon("bar-chart-fill"), " Configure Plot")),
      shiny::p("Choose a plot type and map your columns to axes. The preview updates as you make changes.", class = "text-muted")
    ),

    bslib::layout_columns(
      col_widths = c(5, 7),

      # Left - controls
      bslib::card(
        bslib::card_body(
          shiny::selectInput(ns("plot_type"), "Plot type", choices = PLOT_TYPES, selected = "Scatter"),

          shiny::hr(class = "my-2"),

          shiny::uiOutput(ns("var_mapping")),

          shiny::hr(class = "my-2"),

          shinyWidgets::materialSwitch(
            inputId = ns("interactive"),
            label   = gw_tooltip(
              "Interactive plot (HTML)",
              "Produces a plotly interactive chart instead of a static image. Can also be downloaded as an HTML file."
            ),
            value  = FALSE,
            status = "primary"
          )
        )
      ),

      # Right - live preview
      bslib::card(
        bslib::card_header(
          shiny::div(
            class = "d-flex align-items-center justify-content-between",
            shiny::strong("Live preview"),
            shiny::span(class = "badge bg-secondary small", "Updates automatically")
          )
        ),
        bslib::card_body(
          shiny::uiOutput(ns("preview_panel"))
        )
      )
    ),

    gw_nav_buttons(ns, back = TRUE, next_label = "Continue", next_icon = "arrow-right")
  )
}

#' @noRd
mod_plotconfig_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {

    all_cols     <- shiny::reactive({ req(rv$raw_data); names(rv$raw_data) })
    numeric_cols <- shiny::reactive({ req(rv$col_types); names(rv$col_types)[rv$col_types == "numeric"] })
    date_cols    <- shiny::reactive({ req(rv$col_types); names(rv$col_types)[rv$col_types == "date"] })

    output$var_mapping <- shiny::renderUI({
      cols  <- all_cols()
      ncols <- numeric_cols()
      dcols <- date_cols()
      ptype <- input$plot_type %||% "Scatter"
      ns    <- session$ns

      x_choices <- if (ptype == "Time Series") {
        if (length(dcols)) dcols else cols
      } else cols

      y_needed   <- ptype != "Histogram"
      y_choices  <- if (ptype %in% c("Bar", "Histogram", "Box", "Violin")) cols else c("(count)" = "", ncols)

      shiny::tagList(
        shiny::selectInput(
          ns("x_var"),
          label   = gw_tooltip("X axis", "The column mapped to the horizontal axis"),
          choices = x_choices,
          selected = if (length(x_choices)) x_choices[1] else NULL
        ),
        if (y_needed) {
          shiny::selectInput(
            ns("y_var"),
            label   = gw_tooltip("Y axis", "The column mapped to the vertical axis"),
            choices = y_choices,
            selected = if (length(ncols)) ncols[1] else if (length(cols)) cols[1] else NULL
          )
        },
        if (ptype == "Bar") {
          shiny::tagList(
            shiny::selectInput(ns("bar_stat"), "Bar statistic",
                               c("Count rows" = "count", "Sum of Y column" = "sum")),
            shiny::selectInput(ns("bar_pos"),  "Bar position",
                               c("Grouped (dodge)" = "dodge", "Stacked" = "stack", "Filled (100%)" = "fill"))
          )
        },
        shiny::selectInput(
          ns("group_var"),
          label   = gw_tooltip("Colour / Group by", "Split the data by this column using colour"),
          choices = c("None" = "", cols),
          selected = ""
        ),
        shiny::selectInput(
          ns("facet_var"),
          label   = gw_tooltip("Facet by", "Create separate panels for each level of this column"),
          choices = c("None" = "", cols),
          selected = ""
        )
      )
    })

    # Reactive config
    plot_cfg <- shiny::reactive({
      list(
        plot_type = input$plot_type,
        x_var     = input$x_var,
        y_var     = input$y_var,
        group_var = if (nchar(input$group_var %||% "") > 0) input$group_var else NULL,
        facet_var = if (nchar(input$facet_var %||% "") > 0) input$facet_var else NULL,
        bar_stat  = input$bar_stat,
        bar_pos   = input$bar_pos
      )
    })

    # Live preview
    output$preview_panel <- shiny::renderUI({
      cfg <- plot_cfg()
      req(rv$raw_data, cfg$x_var)
      if (isTRUE(rv$interactive)) {
        plotly::renderPlotly({
          p <- tryCatch(
            build_plot(rv$raw_data, cfg, rv$custom_cfg %||% list(show_major_grid = TRUE, show_minor_grid = FALSE)),
            error = function(e) NULL
          )
          req(p)
          plotly::ggplotly(p)
        })
      } else {
        shiny::renderPlot({
          tryCatch(
            build_plot(rv$raw_data, cfg, rv$custom_cfg %||% list(show_major_grid = TRUE, show_minor_grid = FALSE)),
            error = function(e) {
              ggplot2::ggplot() +
                ggplot2::annotate("text", x = 0.5, y = 0.5,
                                  label = paste("Preview error:", conditionMessage(e)),
                                  colour = "#DC2626", size = 4) +
                ggplot2::theme_void()
            }
          )
        }, height = 340)
      }
    })

    shiny::observeEvent(input$next_btn, {
      req(input$x_var)
      cfg <- plot_cfg()
      rv$plot_cfg    <- cfg
      rv$interactive <- isTRUE(input$interactive)
      rv$logger$log(sprintf(
        "Plot type: %s | X: %s | Y: %s | Group: %s",
        cfg$plot_type, cfg$x_var %||% "-",
        cfg$y_var %||% "-", cfg$group_var %||% "none"
      ))
      rv$step <- 4L
    })

    shiny::observeEvent(input$back, rv$step <- 2L)
  })
}
