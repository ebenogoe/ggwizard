# ---------------------------------------------------------------------------
# Shared UI helpers used across modules
# ---------------------------------------------------------------------------

#' Font-size templates applied proportionally across all plot text elements
#'
#' Each entry gives (base_size, title, subtitle, axis_title, axis_text,
#' legend_text, caption) in points.
#' @noRd
FONT_SIZE_TEMPLATES <- list(
  Small      = list(base = 10, title = 12, subtitle =  9, axis_title =  9, axis_text =  8, legend =  8, caption =  7),
  Medium     = list(base = 12, title = 14, subtitle = 11, axis_title = 10, axis_text =  9, legend =  9, caption =  8),
  Large      = list(base = 14, title = 18, subtitle = 13, axis_title = 12, axis_text = 11, legend = 11, caption =  9),
  `Extra Large` = list(base = 16, title = 22, subtitle = 16, axis_title = 14, axis_text = 13, legend = 13, caption = 11)
)

#' Safe cross-platform font faces available for ggplot2
#' @noRd
SAFE_FONTS <- c("Arial", "Helvetica", "Calibri", "Times New Roman",
                "Georgia", "Courier New", "Palatino")

#' A label followed by a hover tooltip help icon
#' @noRd
gw_tooltip <- function(label, text) {
  shiny::tagList(
    label,
    bslib::tooltip(
      bsicons::bs_icon("info-circle-fill", class = "gw-help"),
      text,
      placement = "right"
    )
  )
}

#' A compact metric tile (label above, big value below)
#' @noRd
gw_metric <- function(label, value, colour = NULL) {
  val_style <- if (!is.null(colour)) paste0("color:", colour) else ""
  shiny::div(
    class = "gw-metric",
    shiny::div(class = "gw-metric-label", label),
    shiny::div(class = "gw-metric-value", style = val_style, value)
  )
}

#' Wizard step sidebar highlighting the current step
#' @noRd
gw_stepper <- function(current) {
  steps <- c("Import Data", "Preview", "Configure Plot", "Customise", "Export")
  items <- lapply(seq_along(steps), function(i) {
    cls  <- if (i == current) "gw-step active" else if (i < current) "gw-step done" else "gw-step"
    icon <- if (i < current) bsicons::bs_icon("check-lg") else as.character(i)
    shiny::tags$li(
      class = cls,
      shiny::span(class = "gw-step-num", icon),
      shiny::span(steps[i])
    )
  })
  shiny::tags$ul(class = "gw-stepper", items)
}

#' Standard Back / Continue button row for wizard steps
#' @noRd
gw_nav_buttons <- function(ns, back = TRUE, next_label = "Continue",
                            next_icon = "arrow-right", back_id = "back",
                            next_id = "next_btn") {
  shiny::div(
    class = "d-flex justify-content-between mt-4",
    if (back) {
      shiny::actionButton(
        ns(back_id),
        label = shiny::tagList(bsicons::bs_icon("arrow-left"), " Back"),
        class = "btn-outline-secondary"
      )
    } else {
      shiny::span()
    },
    shiny::actionButton(
      ns(next_id),
      label = shiny::tagList(next_label, " ", bsicons::bs_icon(next_icon)),
      class = "btn-primary"
    )
  )
}

#' Inline error card shown when something goes wrong
#' @noRd
gw_error_card <- function(message) {
  bslib::card(
    class = "border-danger mt-3",
    bslib::card_body(
      shiny::div(
        class = "d-flex gap-2 align-items-start",
        bsicons::bs_icon("exclamation-triangle-fill", class = "text-danger mt-1"),
        shiny::p(message, class = "mb-0 text-danger")
      )
    )
  )
}

#' Inline success card
#' @noRd
gw_success_card <- function(message) {
  bslib::card(
    class = "border-success mt-3",
    bslib::card_body(
      shiny::div(
        class = "d-flex gap-2 align-items-start",
        bsicons::bs_icon("check-circle-fill", class = "text-success mt-1"),
        shiny::p(message, class = "mb-0 text-success")
      )
    )
  )
}

#' A toggle + conditionally revealed content block
#'
#' Renders a checkbox switch; when checked, `content` is shown.
#' @noRd
gw_toggle_reveal <- function(ns, id, label, content, value = TRUE) {
  shiny::tagList(
    shinyWidgets::materialSwitch(
      inputId  = ns(id),
      label    = label,
      value    = value,
      status   = "primary",
      right    = FALSE
    ),
    shiny::conditionalPanel(
      condition = paste0("input['", ns(id), "'] == true"),
      content
    )
  )
}
