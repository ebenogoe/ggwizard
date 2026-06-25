# Help screen ---------------------------------------------------------------

#' @noRd
mod_help_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "gw-content-page",
    shiny::h2(shiny::tagList(fontawesome::fa("circle-question"), " Help and Documentation")),
    shiny::p("New to ggWizard? Here is everything you need to get started.", class = "lead text-muted"),

    shiny::hr(),

    shiny::h4("Getting started"),
    shiny::p("Click Get Started on the home screen to launch the wizard. The wizard walks you through six steps:"),
    shiny::tags$ol(
      class = "mb-3",
      shiny::tags$li(shiny::strong("Import Data"), " - Upload a CSV or Excel file."),
      shiny::tags$li(shiny::strong("Preview"), " - Verify your data loaded correctly before continuing."),
      shiny::tags$li(shiny::strong("Filter & QC"), " - Optionally exclude columns, remove outliers by Z-score, or flag groups with high replicate variance. You can skip this step if no cleaning is needed."),
      shiny::tags$li(shiny::strong("Configure Plot"), " - Choose a chart type and map your columns to axes."),
      shiny::tags$li(shiny::strong("Customise"), " - Adjust labels, fonts, colours, and themes."),
      shiny::tags$li(shiny::strong("Export"), " - Save your plot and optional R script to a folder of your choice.")
    ),

    shiny::h4("Supported file formats"),
    shiny::tags$ul(
      class = "mb-3",
      shiny::tags$li(shiny::strong("CSV"), " - Plain comma-separated values. Other delimiters (semicolon, tab) are also supported."),
      shiny::tags$li(shiny::strong("Excel (.xlsx, .xls)"), " - Single or multi-sheet workbooks. You will be prompted to choose a sheet if more than one is detected.")
    ),

    shiny::h4("Chart types"),
    DT::datatable(
      data.frame(
        Type        = c("Bar", "Line", "Time Series", "Scatter", "Histogram", "Box", "Violin", "Area"),
        Description = c(
          "Compare categories using bar height (count or sum).",
          "Show trends across an ordered X variable.",
          "Like Line but X must be a date column - the axis is formatted as dates automatically.",
          "Show the relationship between two numeric variables.",
          "Show the distribution of a single numeric variable.",
          "Compare distributions across groups using box-and-whisker plots.",
          "Like Box but shows the full distribution shape.",
          "Like Line but the area below the line is filled."
        ),
        stringsAsFactors = FALSE
      ),
      rownames  = FALSE,
      selection = "none",
      options   = list(dom = "t", pageLength = 10)
    ),

    shiny::h4(class = "mt-4", "Downloading the R script"),
    shiny::p("On the Export step, enable 'R script' to save a self-contained R file that reproduces your plot exactly.
             You can open it in RStudio and run it directly - no ggWizard needed."),

    shiny::h4("Font sizes"),
    shiny::p("Use the Size template setting (Small / Medium / Large / Extra Large) on the Customise step.
             The template sets all text sizes proportionally - the chart title is always larger than axis labels,
             which are larger than tick labels. You do not need to set each element individually."),

    shiny::h4("Exporting a high-resolution image"),
    shiny::p("On the Export step, select High (300 dpi) and increase the width and height to at least 8 x 6 inches.
             For most journals and presentations, PNG at 300 dpi is the recommended format."),

    shiny::hr(),

    shiny::actionButton(ns("home"), shiny::tagList(bsicons::bs_icon("house"), " Back to home"),
                        class = "btn-outline-secondary")
  )
}

#' @noRd
mod_help_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(input$home, rv$screen <- "welcome")
  })
}
