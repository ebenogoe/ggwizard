#' Read a data file (CSV or Excel) and return parsed content
#'
#' @param path  Character. Path to the file.
#' @param format  One of "auto", "csv", "excel".
#' @param sheet   Sheet name or index for Excel files (default 1).
#' @param delim   Delimiter for CSV files; NULL lets readr guess.
#' @param has_header  Logical; whether the first row contains column names.
#'
#' @return A list with:
#'   \describe{
#'     \item{data}{data.frame}
#'     \item{col_types}{named character vector of detected column types}
#'     \item{sheets}{character vector of sheet names (Excel only, else NULL)}
#'     \item{format}{resolved format string}
#'   }
#' @noRd
read_data <- function(path, format = c("auto", "csv", "excel"),
                      sheet = 1, delim = NULL, has_header = TRUE) {
  format <- match.arg(format)
  if (format == "auto") {
    ext <- tolower(tools::file_ext(path))
    format <- if (ext %in% c("xlsx", "xls")) "excel" else "csv"
  }

  sheets <- NULL

  if (format == "excel") {
    sheets <- readxl::excel_sheets(path)
    df <- readxl::read_excel(
      path,
      sheet     = sheet,
      col_names = has_header,
      .name_repair = "unique"
    )
    df <- as.data.frame(df)
  } else {
    if (!is.null(delim) && nchar(delim) > 0) {
      df <- readr::read_delim(path, delim = delim, col_names = has_header,
                              show_col_types = FALSE, name_repair = "unique")
    } else {
      df <- readr::read_csv(path, col_names = has_header,
                            show_col_types = FALSE, name_repair = "unique")
    }
    df <- as.data.frame(df)
  }

  col_types <- vapply(df, detect_col_type, character(1))
  list(data = df, col_types = col_types, sheets = sheets, format = format)
}

#' Detect a human-readable column type label
#' @noRd
detect_col_type <- function(x) {
  if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) return("date")
  if (is.numeric(x))   return("numeric")
  if (is.logical(x))   return("logical")
  if (is.factor(x))    return("factor")
  "character"
}

#' Summarise a data.frame for the preview step
#'
#' @return A list suitable for populating metric tiles.
#' @noRd
summarise_data <- function(df, col_types) {
  n_missing_cells <- sum(vapply(df, function(x) sum(is.na(x)), integer(1)))
  total_cells     <- nrow(df) * ncol(df)
  pct_missing     <- if (total_cells > 0) round(100 * n_missing_cells / total_cells, 1) else 0

  list(
    nrow       = nrow(df),
    ncol       = ncol(df),
    n_numeric  = sum(col_types == "numeric"),
    n_date     = sum(col_types == "date"),
    pct_missing = pct_missing
  )
}
