#' Generate a self-contained, reproducible R script for the current plot
#'
#' @param file_path  Original data file path (character).
#' @param sheet      Sheet name or index (for Excel); NULL for CSV.
#' @param plot_cfg   list from rv$plot_cfg.
#' @param custom_cfg list from rv$custom_cfg.
#'
#' @return A single character string containing valid R code.
#' @noRd
generate_script <- function(file_path, sheet, plot_cfg, custom_cfg) {
  ptype   <- plot_cfg$plot_type %||% "Scatter"
  x_var   <- plot_cfg$x_var
  y_var   <- plot_cfg$y_var
  grp_var <- plot_cfg$group_var
  facet_var <- plot_cfg$facet_var

  is_excel <- tolower(tools::file_ext(file_path)) %in% c("xlsx", "xls")

  read_block <- if (is_excel) {
    sprintf(
      'library(readxl)\ndata <- as.data.frame(read_excel(%s, sheet = %s))',
      deparse(file_path),
      if (is.character(sheet)) deparse(sheet) else as.character(sheet)
    )
  } else {
    sprintf('library(readr)\ndata <- as.data.frame(read_csv(%s))', deparse(file_path))
  }

  bar_uses_count <- ptype == "Bar" && !(plot_cfg$bar_stat == "sum" && !is.null(y_var) && nchar(y_var) > 0)
  aes_parts <- sprintf('x = %s', backtick(x_var))
  if (!is.null(y_var) && nchar(y_var) > 0 && ptype != "Histogram" && !bar_uses_count)
    aes_parts <- paste0(aes_parts, sprintf(', y = %s', backtick(y_var)))
  if (!is.null(grp_var) && nchar(grp_var) > 0)
    aes_parts <- paste0(aes_parts, sprintf(', colour = %s, fill = %s', backtick(grp_var), backtick(grp_var)))

  geom_block <- switch(
    ptype,
    "Bar"         = sprintf('  geom_bar(stat = "%s", position = "%s", width = %.1f)',
                             if (!is.null(plot_cfg$bar_stat) && plot_cfg$bar_stat == "sum" && !is.null(y_var)) "identity" else "count",
                             plot_cfg$bar_pos %||% "dodge",
                             custom_cfg$bar_width %||% 0.7),
    "Line"        = ,
    "Time Series" = sprintf('  geom_line(linewidth = %.1f, linetype = "%s")',
                             custom_cfg$line_width %||% 0.8, custom_cfg$line_type %||% "solid"),
    "Scatter"     = sprintf('  geom_point(size = %.1f, shape = %d)',
                             custom_cfg$point_size %||% 2, as.integer(custom_cfg$point_shape %||% 16)),
    "Histogram"   = sprintf('  geom_histogram(bins = %d)', custom_cfg$hist_bins %||% 30),
    "Box" = {
      box_show_outliers <- isTRUE(custom_cfg$box_outliers)
      box_show_median   <- if (!is.null(custom_cfg$box_median)) isTRUE(custom_cfg$box_median) else TRUE
      box_extras <- paste0(
        if (!box_show_outliers) ", outlier.shape = NA" else "",
        if (!box_show_median)   ", fatten = 0"         else ""
      )
      sprintf("  geom_boxplot(width = 0.6%s)", box_extras)
    },
    "Violin"      = '  geom_violin(trim = FALSE)',
    "Area"        = '  geom_area(alpha = 0.6)',
    '  geom_point()'
  )

  sz_tpl    <- FONT_SIZE_TEMPLATES[[custom_cfg$font_template %||% "Medium"]]
  font_fam  <- custom_cfg$font_face %||% "Arial"
  theme_fn  <- switch(custom_cfg$theme_name %||% "Minimal",
                      "Minimal" = "theme_minimal", "Classic" = "theme_classic",
                      "Dark" = "theme_dark", "Light" = "theme_light",
                      "Void" = "theme_void", "theme_minimal")

  palette_block <- build_palette_script(custom_cfg$palette %||% "Plotter", ptype, grp_var)

  label_parts <- character(0)
  if (isTRUE(custom_cfg$title_enabled)    && !is.null(custom_cfg$title))    label_parts <- c(label_parts, sprintf('  title    = "%s"', custom_cfg$title))
  if (isTRUE(custom_cfg$subtitle_enabled) && !is.null(custom_cfg$subtitle)) label_parts <- c(label_parts, sprintf('  subtitle = "%s"', custom_cfg$subtitle))
  if (isTRUE(custom_cfg$caption_enabled)  && !is.null(custom_cfg$caption))  label_parts <- c(label_parts, sprintf('  caption  = "%s"', custom_cfg$caption))
  if (isTRUE(custom_cfg$xlab_enabled)     && !is.null(custom_cfg$x_lab))    label_parts <- c(label_parts, sprintf('  x        = "%s"', custom_cfg$x_lab))
  if (isTRUE(custom_cfg$ylab_enabled)     && !is.null(custom_cfg$y_lab))    label_parts <- c(label_parts, sprintf('  y        = "%s"', custom_cfg$y_lab))
  if (!is.null(custom_cfg$legend_title) && nchar(custom_cfg$legend_title) > 0) {
    label_parts <- c(label_parts, sprintf('  colour   = "%s"', custom_cfg$legend_title))
    label_parts <- c(label_parts, sprintf('  fill     = "%s"', custom_cfg$legend_title))
  }
  labs_block <- if (length(label_parts)) paste0("  labs(\n  ", paste(label_parts, collapse = ",\n  "), "\n  ) +") else ""

  has_facet   <- !is.null(facet_var) && nchar(facet_var) > 0
  facet_block <- if (has_facet) sprintf('  facet_wrap(vars(%s)) +', backtick(facet_var)) else ""
  facet_theme_block <- if (has_facet) paste0(
    '  theme(\n',
    '    panel.border     = element_rect(colour = "grey40", fill = NA, linewidth = 0.5),\n',
    '    strip.background = element_rect(fill = "grey88", colour = "grey40", linewidth = 0.5),\n',
    '    strip.text       = element_text(face = "bold")\n',
    '  ) +'
  ) else ""

  show_axis  <- !isFALSE(custom_cfg$show_axis_lines)
  x_angle    <- custom_cfg$x_axis_angle %||% 0
  grid_lines <- c()
  grid_lines <- c(grid_lines, if (show_axis) '    axis.line = element_line(colour = "grey30", linewidth = 0.4)'
                              else           "    axis.line = element_blank()")
  if (!isTRUE(custom_cfg$show_major_grid)) grid_lines <- c(grid_lines, "    panel.grid.major = element_blank()")
  if (!isTRUE(custom_cfg$show_minor_grid)) grid_lines <- c(grid_lines, "    panel.grid.minor = element_blank()")
  if (!is.na(x_angle) && x_angle != 0) {
    vjust <- if (x_angle == 90) 0.5 else 1
    grid_lines <- c(grid_lines, sprintf("    axis.text.x = element_text(angle = %g, hjust = 1, vjust = %g)", x_angle, vjust))
  }
  grid_theme <- if (length(grid_lines)) paste0("  theme(\n", paste(grid_lines, collapse = ",\n"), "\n  ) +") else ""

  # Mean diamond layer (Box plots only)
  box_mean_block <- if (ptype == "Box" && isTRUE(custom_cfg$box_mean)) {
    '  stat_summary(fun = mean, geom = "point", shape = 23, fill = "white", colour = "grey30", size = 3) +'
  } else ""

  # Axis scale overrides
  y_min_v  <- custom_cfg$y_min;  y_max_v  <- custom_cfg$y_max;  y_step_v <- custom_cfg$y_break_step
  x_min_v  <- custom_cfg$x_min;  x_max_v  <- custom_cfg$x_max
  scale_y_block <- if (!is.null(y_min_v) || !is.null(y_max_v) || !is.null(y_step_v)) {
    lim_str   <- sprintf("c(%s, %s)", if (is.null(y_min_v)) "NA" else y_min_v,
                                      if (is.null(y_max_v)) "NA" else y_max_v)
    break_str <- if (!is.null(y_step_v)) sprintf(
      ", breaks = function(x) seq(floor(x[1]/%g)*%g, ceiling(x[2]/%g)*%g, by = %g)",
      y_step_v, y_step_v, y_step_v, y_step_v, y_step_v
    ) else ""
    sprintf("  scale_y_continuous(limits = %s%s) +", lim_str, break_str)
  } else ""
  scale_x_block <- if (!is.null(x_min_v) || !is.null(x_max_v)) {
    lim_str <- sprintf("c(%s, %s)", if (is.null(x_min_v)) "NA" else x_min_v,
                                    if (is.null(x_max_v)) "NA" else x_max_v)
    sprintf("  scale_x_continuous(limits = %s) +", lim_str)
  } else ""

  paste0(
    "# Script generated by ggWizard\n",
    "# Run this file in R to reproduce your plot.\n\n",
    "library(ggplot2)\n",
    read_block, "\n\n",
    "p <- ggplot(data, aes(", aes_parts, ")) +\n",
    geom_block, " +\n",
    if (nchar(box_mean_block))  paste0(box_mean_block, "\n")  else "",
    if (nchar(palette_block))   paste0(palette_block, " +\n") else "",
    if (nchar(facet_block))     paste0(facet_block, "\n")     else "",
    sprintf('  %s(base_size = %d, base_family = "%s") +\n', theme_fn, sz_tpl$base, font_fam),
    sprintf('  theme(\n    legend.position = "%s",\n', custom_cfg$legend_pos %||% "bottom"),
    sprintf('    plot.title    = element_text(size = %d, face = "bold"),\n', sz_tpl$title),
    sprintf('    plot.subtitle = element_text(size = %d),\n', sz_tpl$subtitle),
    sprintf('    axis.title    = element_text(size = %d),\n', sz_tpl$axis_title),
    sprintf('    axis.text     = element_text(size = %d),\n', sz_tpl$axis_text),
    sprintf('    legend.text   = element_text(size = %d),\n', sz_tpl$legend),
    sprintf('    plot.caption  = element_text(size = %d, hjust = 1)\n  ) +\n', sz_tpl$caption),
    if (nchar(grid_theme))        paste0(grid_theme, "\n")        else "",
    if (nchar(facet_theme_block)) paste0(facet_theme_block, "\n") else "",
    if (nchar(scale_y_block))     paste0(scale_y_block, "\n")     else "",
    if (nchar(scale_x_block))     paste0(scale_x_block, "\n")     else "",
    if (nchar(labs_block))        paste0(labs_block, "\n")        else "",
    "  NULL\n\n",
    "print(p)\n\n",
    "# Save to file\n",
    "# ggsave('plot.png', plot = p, dpi = 300, width = 8, height = 6)\n"
  )
}

#' Wrap a column name in backticks if it contains spaces or special chars
#' @noRd
backtick <- function(x) {
  if (grepl("[^A-Za-z0-9_.]", x)) paste0("`", x, "`") else x
}

#' Build colour palette lines for the script
#' @noRd
build_palette_script <- function(palette_name, ptype, grp_var) {
  if (is.null(grp_var) || nchar(grp_var) == 0) return("")
  switch(
    palette_name,
    "Plotter"  = '  scale_colour_manual(values = c("#4F46E5","#059669","#D97706","#DC2626","#7C3AED","#0891B2","#65A30D")) +\n  scale_fill_manual(values = c("#4F46E5","#059669","#D97706","#DC2626","#7C3AED","#0891B2","#65A30D"))',
    "Viridis"  = '  scale_colour_viridis_d() +\n  scale_fill_viridis_d()',
    "Set1"     = '  scale_colour_brewer(palette = "Set1") +\n  scale_fill_brewer(palette = "Set1")',
    "Set2"     = '  scale_colour_brewer(palette = "Set2") +\n  scale_fill_brewer(palette = "Set2")',
    "Pastel"   = '  scale_colour_brewer(palette = "Pastel1") +\n  scale_fill_brewer(palette = "Pastel1")',
    "Spectral" = '  scale_colour_brewer(palette = "Spectral") +\n  scale_fill_brewer(palette = "Spectral")',
    ""
  )
}
