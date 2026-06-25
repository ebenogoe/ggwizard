#' Build a ggplot2 object from user configuration
#'
#' @param data       data.frame
#' @param plot_cfg   list from rv$plot_cfg
#' @param custom_cfg list from rv$custom_cfg
#'
#' @return A ggplot object.
#' @noRd
build_plot <- function(data, plot_cfg, custom_cfg) {
  ptype    <- plot_cfg$plot_type %||% "Scatter"
  x_var    <- plot_cfg$x_var
  y_var    <- plot_cfg$y_var
  grp_var  <- plot_cfg$group_var
  facet_var <- plot_cfg$facet_var
  bar_stat  <- plot_cfg$bar_stat   %||% "count"
  bar_pos   <- plot_cfg$bar_pos    %||% "dodge"

  # Base aesthetic mapping
  # Bar + stat="count" forbids a y aesthetic; only add y for identity/other types
  bar_uses_count <- ptype == "Bar" && !(bar_stat == "sum" && !is.null(y_var) && nchar(y_var) > 0)
  aes_args <- list(x = rlang::sym(x_var))
  if (!is.null(y_var) && nchar(y_var) > 0 && ptype != "Histogram" && !bar_uses_count) aes_args$y <- rlang::sym(y_var)
  if (!is.null(grp_var) && nchar(grp_var) > 0) {
    aes_args$colour <- rlang::sym(grp_var)
    aes_args$fill   <- rlang::sym(grp_var)
    aes_args$group  <- rlang::sym(grp_var)
  }

  p <- ggplot2::ggplot(data, do.call(ggplot2::aes, aes_args))

  # Geometry
  p <- switch(
    ptype,
    "Bar"         = p + ggplot2::geom_bar(
      stat     = if (bar_stat == "sum" && !is.null(y_var)) "identity" else "count",
      position = bar_pos,
      width    = custom_cfg$bar_width %||% 0.7
    ),
    "Line"        = p + ggplot2::geom_line(
      linewidth = custom_cfg$line_width %||% 0.8,
      linetype  = custom_cfg$line_type  %||% "solid"
    ),
    "Time Series" = p + ggplot2::geom_line(
      linewidth = custom_cfg$line_width %||% 0.8,
      linetype  = custom_cfg$line_type  %||% "solid"
    ),
    "Scatter"     = p + ggplot2::geom_point(
      size  = custom_cfg$point_size  %||% 2,
      shape = as.integer(custom_cfg$point_shape %||% 16)
    ),
    "Histogram"   = p + ggplot2::geom_histogram(
      bins  = custom_cfg$hist_bins   %||% 30,
      colour = if (isTRUE(custom_cfg$hist_outline)) "white" else NA
    ),
    "Box"         = p + ggplot2::geom_boxplot(
      outlier.shape = if (isTRUE(custom_cfg$box_outliers)) 19 else NA,
      width = 0.6
    ),
    "Violin"      = p + ggplot2::geom_violin(trim = FALSE),
    "Area"        = p + ggplot2::geom_area(alpha = 0.6),
    p + ggplot2::geom_point()
  )

  # Colour palette
  palette  <- custom_cfg$palette %||% "Plotter"
  p <- apply_palette(p, palette, ptype, grp_var)

  # Facet
  has_facet <- !is.null(facet_var) && nchar(facet_var) > 0
  if (has_facet) {
    p <- p + ggplot2::facet_wrap(ggplot2::vars(!!rlang::sym(facet_var)))
  }

  # Theme
  p <- apply_theme(p, custom_cfg)

  # Facet strip and panel border — applied after theme so they always win
  if (has_facet) {
    p <- p + ggplot2::theme(
      panel.border     = ggplot2::element_rect(colour = "grey40", fill = NA, linewidth = 0.5),
      strip.background = ggplot2::element_rect(fill = "grey88", colour = "grey40", linewidth = 0.5),
      strip.text       = ggplot2::element_text(face = "bold")
    )
  }

  # Labels
  p <- apply_labels(p, custom_cfg)

  p
}

#' Apply the selected ggplot2 theme and font/grid settings
#' @noRd
apply_theme <- function(p, cfg) {
  theme_fn <- switch(
    cfg$theme_name %||% "Minimal",
    "Minimal" = ggplot2::theme_minimal,
    "Classic" = ggplot2::theme_classic,
    "Dark"    = ggplot2::theme_dark,
    "Light"   = ggplot2::theme_light,
    "Void"    = ggplot2::theme_void,
    ggplot2::theme_minimal
  )

  sz_tpl   <- FONT_SIZE_TEMPLATES[[cfg$font_template %||% "Medium"]]
  font_fam <- cfg$font_face %||% "Arial"

  p <- p + theme_fn(
    base_size   = sz_tpl$base,
    base_family = font_fam
  )

  # Grid lines and axis lines
  # show_axis_lines defaults TRUE so the live preview on Step 3 already shows them
  show_axis <- !isFALSE(cfg$show_axis_lines)
  grid_overrides <- ggplot2::theme(
    axis.line = if (show_axis) ggplot2::element_line(colour = "grey30", linewidth = 0.4)
                else           ggplot2::element_blank()
  )
  if (!isTRUE(cfg$show_major_grid)) {
    grid_overrides <- grid_overrides + ggplot2::theme(
      panel.grid.major = ggplot2::element_blank()
    )
  }
  if (!isTRUE(cfg$show_minor_grid)) {
    grid_overrides <- grid_overrides + ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank()
    )
  }

  # X axis label angle
  x_angle <- cfg$x_axis_angle %||% 0
  if (!is.na(x_angle) && x_angle != 0) {
    vjust <- if (x_angle == 90) 0.5 else 1
    grid_overrides <- grid_overrides + ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = x_angle, hjust = 1, vjust = vjust)
    )
  }

  # Legend position
  legend_pos <- cfg$legend_pos %||% "bottom"
  grid_overrides <- grid_overrides + ggplot2::theme(
    legend.position = legend_pos,
    plot.title    = ggplot2::element_text(size = sz_tpl$title,      family = font_fam, face = "bold"),
    plot.subtitle = ggplot2::element_text(size = sz_tpl$subtitle,   family = font_fam),
    axis.title    = ggplot2::element_text(size = sz_tpl$axis_title, family = font_fam),
    axis.text     = ggplot2::element_text(size = sz_tpl$axis_text,  family = font_fam),
    legend.text   = ggplot2::element_text(size = sz_tpl$legend,     family = font_fam),
    plot.caption  = ggplot2::element_text(size = sz_tpl$caption,    family = font_fam, hjust = 1)
  )

  p + grid_overrides
}

#' Apply labels from custom_cfg, honouring enable toggles
#' @noRd
apply_labels <- function(p, cfg) {
  title    <- if (isTRUE(cfg$title_enabled)    && !is.null(cfg$title))    cfg$title    else NULL
  subtitle <- if (isTRUE(cfg$subtitle_enabled) && !is.null(cfg$subtitle)) cfg$subtitle else NULL
  caption  <- if (isTRUE(cfg$caption_enabled)  && !is.null(cfg$caption))  cfg$caption  else NULL
  x_lab    <- if (isTRUE(cfg$xlab_enabled)     && !is.null(cfg$x_lab))    cfg$x_lab    else ggplot2::waiver()
  y_lab    <- if (isTRUE(cfg$ylab_enabled)     && !is.null(cfg$y_lab))    cfg$y_lab    else ggplot2::waiver()
  leg_title <- if (!is.null(cfg$legend_title) && nchar(cfg$legend_title) > 0) cfg$legend_title else ggplot2::waiver()

  p + ggplot2::labs(
    title    = title,
    subtitle = subtitle,
    caption  = caption,
    x        = x_lab,
    y        = y_lab,
    colour   = leg_title,
    fill     = leg_title
  )
}

#' Apply a named colour palette
#' @noRd
apply_palette <- function(p, palette_name, ptype, grp_var) {
  if (is.null(grp_var) || nchar(grp_var) == 0) return(p)
  is_fill_geom <- ptype %in% c("Bar", "Histogram", "Area", "Box", "Violin")
  switch(
    palette_name,
    "Plotter"    = {
      cols <- c("#4F46E5", "#059669", "#D97706", "#DC2626", "#7C3AED", "#0891B2", "#65A30D")
      if (is_fill_geom) p + ggplot2::scale_fill_manual(values = cols) + ggplot2::scale_colour_manual(values = cols)
      else p + ggplot2::scale_colour_manual(values = cols) + ggplot2::scale_fill_manual(values = cols)
    },
    "Viridis"    = p + ggplot2::scale_colour_viridis_d() + ggplot2::scale_fill_viridis_d(),
    "Set1"       = p + ggplot2::scale_colour_brewer(palette = "Set1") + ggplot2::scale_fill_brewer(palette = "Set1"),
    "Set2"       = p + ggplot2::scale_colour_brewer(palette = "Set2") + ggplot2::scale_fill_brewer(palette = "Set2"),
    "Pastel"     = p + ggplot2::scale_colour_brewer(palette = "Pastel1") + ggplot2::scale_fill_brewer(palette = "Pastel1"),
    "Spectral"   = p + ggplot2::scale_colour_brewer(palette = "Spectral") + ggplot2::scale_fill_brewer(palette = "Spectral"),
    p
  )
}

#' NULL-coalescing operator
#' @noRd
`%||%` <- function(x, y) if (is.null(x) || (length(x) == 1 && is.na(x))) y else x
