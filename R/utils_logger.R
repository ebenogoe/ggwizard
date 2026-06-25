#' Create a run logger
#'
#' Returns a small object that collects timestamped log lines during a run.
#' Engine functions take a `logger` argument and call `logger$log()`;
#' passing `NULL` is also accepted (a no-op logger is created).
#'
#' @return A list with:
#'   \describe{
#'     \item{log}{function(msg, level) - append one timestamped line.}
#'     \item{section}{function(title) - append a visual section header.}
#'     \item{lines}{function() - return the full character vector of log lines.}
#'     \item{tail}{function(n) - return the last n lines.}
#'   }
#' @noRd
new_run_logger <- function() {
  store <- new.env(parent = emptyenv())
  store$lines <- character(0)

  add <- function(line) {
    store$lines <- c(store$lines, line)
    invisible(line)
  }

  log <- function(msg, level = c("INFO", "WARN", "ERROR", "OK")) {
    level <- match.arg(level)
    ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    add(sprintf("[%s] %-5s %s", ts, level, msg))
  }

  section <- function(title) {
    add("")
    add(strrep("=", 70))
    add(paste0("  ", title))
    add(strrep("=", 70))
  }

  list(
    log     = log,
    section = section,
    lines   = function() store$lines,
    tail    = function(n = 1) utils::tail(store$lines, n)
  )
}

#' Coerce a possibly-NULL logger to a usable one
#' @noRd
as_logger <- function(logger = NULL) {
  if (is.null(logger)) new_run_logger() else logger
}
