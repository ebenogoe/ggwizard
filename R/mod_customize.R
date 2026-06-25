# Step 4 - Customise Appearance -------------------------------------------

PALETTES    <- c("Plotter", "Viridis", "Set1", "Set2", "Pastel", "Spectral")
THEMES      <- c("Minimal", "Classic", "Dark", "Light", "Void")
LINE_TYPES  <- c("solid", "dashed", "dotted", "dotdash")
POINT_SHAPES <- c("Circle (16)" = "16", "Triangle (17)" = "17",
                   "Square (15)" = "15", "Cross (3)" = "3",
                   "Diamond (18)" = "18")

#' @noRd
mod_customize_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(
    shiny::div(
      class = "gw-step-header",
      shiny::h2(shiny::tagList(bsicons::bs_icon("palette-fill"), " Customise Appearance")),
      shiny::p("Fine-tune your chart. All changes are reflected in the preview instantly.", class = "text-muted")
    ),

    bslib::layout_columns(
      col_widths = c(5, 7),

      # Left - accordion controls
      bslib::accordion(
        id = ns("acc"),
        open = "Labels",
        multiple = TRUE,

        # Labels section
        bslib::accordion_panel(
          "Labels",
          icon = bsicons::bs_icon("type"),
          gw_toggle_reveal(ns, "title_enabled", "Plot title", shiny::textInput(ns("title"), NULL, placeholder = "e.g. Revenue Over Time"), value = TRUE),
          gw_toggle_reveal(ns, "subtitle_enabled", "Subtitle", shiny::textInput(ns("subtitle"), NULL, placeholder = "e.g. By region, 2024"), value = TRUE),
          gw_toggle_reveal(ns, "caption_enabled", "Caption", shiny::textInput(ns("caption"), NULL, placeholder = "e.g. Source: company data"), value = FALSE),
          gw_toggle_reveal(ns, "xlab_enabled", "X-axis title", shiny::textInput(ns("x_lab"), NULL), value = TRUE),
          gw_toggle_reveal(ns, "ylab_enabled", "Y-axis title", shiny::textInput(ns("y_lab"), NULL), value = TRUE)
        ),

        # Font section
        bslib::accordion_panel(
          "Font",
          icon = bsicons::bs_icon("fonts"),
          shinyWidgets::prettyRadioButtons(
            inputId  = ns("font_template"),
            label    = gw_tooltip("Size template", "Sets all text sizes proportionally - titles are always larger than axis labels"),
            choices  = names(FONT_SIZE_TEMPLATES),
            selected = "Medium",
            inline   = TRUE,
            status   = "primary"
          ),
          shiny::selectInput(
            ns("font_face"),
            label   = gw_tooltip("Font face", "Safe fonts that display consistently across Windows, Mac, and Linux"),
            choices = SAFE_FONTS,
            selected = "Arial"
          )
        ),

        # Theme section
        bslib::accordion_panel(
          "Theme",
          icon = bsicons::bs_icon("layout-three-columns"),
          shiny::selectInput(ns("theme_name"), "ggplot2 theme", choices = THEMES, selected = "Minimal"),
          shiny::selectInput(ns("palette"),    "Colour palette", choices = PALETTES, selected = "Plotter")
        ),

        # Legend section
        bslib::accordion_panel(
          "Legend",
          icon = bsicons::bs_icon("list-ul"),
          shiny::selectInput(
            ns("legend_pos"),
            label   = "Position",
            choices = c("Bottom" = "bottom", "Top" = "top", "Right" = "right",
                        "Left" = "left", "None (hidden)" = "none"),
            selected = "bottom"
          ),
          shiny::textInput(ns("legend_title"), "Legend title (leave blank to use column name)", placeholder = "")
        ),

        # Grid section
        bslib::accordion_panel(
          "Grid and axes",
          icon = bsicons::bs_icon("grid"),
          shinyWidgets::materialSwitch(ns("show_axis_lines"),  "Show axis lines",       value = TRUE,  status = "primary"),
          shinyWidgets::materialSwitch(ns("show_major_grid"),  "Show major grid lines", value = TRUE,  status = "primary"),
          shinyWidgets::materialSwitch(ns("show_minor_grid"),  "Show minor grid lines", value = FALSE, status = "primary")
        ),

        # Plot-type-specific section
        bslib::accordion_panel(
          "Geometry options",
          icon = bsicons::bs_icon("sliders"),
          shiny::uiOutput(ns("geom_options"))
        )
      ),

      # Right - live preview
      bslib::card(
        bslib::card_header(shiny::strong("Live preview")),
        bslib::card_body(
          shiny::uiOutput(ns("preview_panel"))
        )
      )
    ),

    gw_nav_buttons(ns, back = TRUE, next_label = "Continue to Export", next_icon = "arrow-right")
  )
}

#' @noRd
mod_customize_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {

    # Pre-fill axis label inputs from selected column names
    shiny::observe({
      req(rv$plot_cfg)
      shiny::updateTextInput(session, "x_lab",   value = rv$plot_cfg$x_var   %||% "")
      shiny::updateTextInput(session, "y_lab",   value = rv$plot_cfg$y_var   %||% "")
    })

    # Geometry options vary by plot type
    output$geom_options <- shiny::renderUI({
      req(rv$plot_cfg)
      ptype <- rv$plot_cfg$plot_type %||% "Scatter"
      ns    <- session$ns
      switch(
        ptype,
        "Scatter" = shiny::tagList(
          shiny::sliderInput(ns("point_size"), "Point size", min = 0.5, max = 8, value = 2, step = 0.5),
          shiny::selectInput(ns("point_shape"), "Point shape", choices = POINT_SHAPES, selected = "16")
        ),
        "Line"        = ,
        "Time Series" = shiny::tagList(
          shiny::sliderInput(ns("line_width"), "Line width", min = 0.2, max = 3, value = 0.8, step = 0.2),
          shiny::selectInput(ns("line_type"),  "Line type", choices = LINE_TYPES, selected = "solid")
        ),
        "Bar" = shiny::sliderInput(ns("bar_width"), "Bar width", min = 0.1, max = 1, value = 0.7, step = 0.05),
        "Histogram" = shiny::tagList(
          shiny::sliderInput(ns("hist_bins"), "Number of bins", min = 5, max = 100, value = 30, step = 5),
          shinyWidgets::materialSwitch(ns("hist_outline"), "Show bin outline", value = TRUE, status = "primary")
        ),
        "Box"    = shinyWidgets::materialSwitch(ns("box_outliers"), "Show outlier points", value = TRUE, status = "primary"),
        "Violin" = shinyWidgets::materialSwitch(ns("violin_trim"),  "Trim violin tails",   value = FALSE, status = "primary"),
        shiny::p("No additional options for this plot type.", class = "text-muted small")
      )
    })

    # Collect all customisation into a reactive list
    custom_cfg <- shiny::reactive({
      list(
        title_enabled    = isTRUE(input$title_enabled),
        subtitle_enabled = isTRUE(input$subtitle_enabled),
        caption_enabled  = isTRUE(input$caption_enabled),
        xlab_enabled     = isTRUE(input$xlab_enabled),
        ylab_enabled     = isTRUE(input$ylab_enabled),
        title            = input$title,
        subtitle         = input$subtitle,
        caption          = input$caption,
        x_lab            = input$x_lab,
        y_lab            = input$y_lab,
        font_template    = input$font_template    %||% "Medium",
        font_face        = input$font_face        %||% "Arial",
        theme_name       = input$theme_name       %||% "Minimal",
        palette          = input$palette          %||% "Plotter",
        legend_pos       = input$legend_pos       %||% "bottom",
        legend_title     = input$legend_title,
        show_axis_lines  = isTRUE(input$show_axis_lines),
        show_major_grid  = isTRUE(input$show_major_grid),
        show_minor_grid  = isTRUE(input$show_minor_grid),
        point_size       = input$point_size,
        point_shape      = input$point_shape,
        line_width       = input$line_width,
        line_type        = input$line_type,
        bar_width        = input$bar_width,
        hist_bins        = input$hist_bins,
        hist_outline     = isTRUE(input$hist_outline),
        box_outliers     = isTRUE(input$box_outliers)
      )
    })

    # Live preview
    output$preview_panel <- shiny::renderUI({
      req(rv$raw_data, rv$plot_cfg, rv$plot_cfg$x_var)
      cfg <- custom_cfg()
      if (isTRUE(rv$interactive)) {
        plotly::renderPlotly({
          p <- tryCatch(build_plot(rv$raw_data, rv$plot_cfg, cfg), error = function(e) NULL)
          req(p); plotly::ggplotly(p)
        })
      } else {
        shiny::renderPlot({
          tryCatch(
            build_plot(rv$raw_data, rv$plot_cfg, cfg),
            error = function(e) {
              ggplot2::ggplot() +
                ggplot2::annotate("text", x = 0.5, y = 0.5,
                                  label = paste("Preview error:", conditionMessage(e)),
                                  colour = "#DC2626", size = 4) + ggplot2::theme_void()
            }
          )
        }, height = 380)
      }
    })

    shiny::observeEvent(input$next_btn, {
      cfg        <- custom_cfg()
      rv$custom_cfg <- cfg
      rv$plot_obj   <- tryCatch(
        build_plot(rv$raw_data, rv$plot_cfg, cfg),
        error = function(e) {
          rv$logger$log(paste("Plot build failed:", conditionMessage(e)), "ERROR")
          NULL
        }
      )
      if (!is.null(rv$plot_obj)) rv$logger$log("Plot built successfully", "OK")

      rv$script_txt <- tryCatch(
        generate_script(rv$file_info$datapath, rv$file_info$sheet_used, rv$plot_cfg, cfg),
        error = function(e) NULL
      )
      rv$step <- 5L
    })

    shiny::observeEvent(input$back, rv$step <- 3L)
  })
}
