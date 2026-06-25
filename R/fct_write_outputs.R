#' Save all outputs to a timestamped subfolder
#'
#' Creates `<run_dir>/ggwizard_YYYYMMDD_HHMMSS/` and writes:
#'   - The plot in each requested format
#'   - plot_script.R
#'   - run_log.txt
#'
#' @param plot_obj   A ggplot object.
#' @param run_dir    Base output directory chosen by the user.
#' @param formats    Character vector of formats, e.g. c("png", "pdf").
#' @param dpi        Integer DPI for raster formats.
#' @param width      Plot width in inches.
#' @param height     Plot height in inches.
#' @param script_txt Character string; the R script content.
#' @param logger     A logger object from new_run_logger().
#'
#' @return The path to the timestamped subfolder (invisibly).
#' @noRd
write_outputs <- function(plot_obj, run_dir, formats = "png",
                          dpi = 300, width = 8, height = 6,
                          script_txt = NULL, logger = NULL) {
  logger <- as_logger(logger)

  ts       <- format(Sys.time(), "%Y%m%d_%H%M%S")
  out_dir  <- fs::path(run_dir, paste0("ggwizard_", ts))
  fs::dir_create(out_dir, recurse = TRUE)
  logger$log(paste("Output folder:", out_dir))

  for (fmt in formats) {
    fname <- fs::path(out_dir, paste0("plot.", fmt))
    tryCatch({
      ggplot2::ggsave(
        filename = fname,
        plot     = plot_obj,
        dpi      = dpi,
        width    = width,
        height   = height,
        units    = "in"
      )
      logger$log(sprintf("Saved: plot.%s (%d dpi, %.0fx%.0f in)", fmt, dpi, width, height), "OK")
    }, error = function(e) {
      logger$log(paste("Failed to save", fmt, "-", conditionMessage(e)), "ERROR")
    })
  }

  if (!is.null(script_txt)) {
    script_path <- fs::path(out_dir, "plot_script.R")
    writeLines(script_txt, script_path)
    logger$log("Saved: plot_script.R", "OK")
  }

  log_path <- fs::path(out_dir, "run_log.txt")
  logger$section("End of run")
  logger$log("Saved: run_log.txt", "OK")
  writeLines(logger$lines(), log_path)

  invisible(out_dir)
}
