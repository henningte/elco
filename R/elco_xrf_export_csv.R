#' Reformats raw csv X-ray fluorescence data as exported by the vendor software.
#'
#' `elco_xrf_export_csv` takes an object of class xrf and exports two csv files:
#' one with the measured data and one with metadata indicating data types and units of measurement.
#'
#' @param x An object of class [`xrf`][elco::elco_new_xrf].
#' @param file A character value representing a path and file name (csv file) where to export
#' the reformatted data to. The two exported csv files are named using the value of `file`:
#' \enumerate{
#'   \item A file named with the value of `file`.
#'   \item A file named with the value of `file` and with additional `"_metadata"`, containing
#'   the metadata (variable type and unit).
#' }
#' @return nothing.
#'
#' @export
elco_xrf_export_csv <- function(x,
                           file) {

  # checks
  elco_check_xrf(x)
  if(!is.character(file)) {
    rlang::abort(paste0("`file` must be a character value, not ", class(file)[[1]], "."))
  }
  cond <- length(file)
  if(cond != 1) {
    rlang::abort(paste0("`file` must be of length 1, but it is of length ", cond, "."))
  }

  # collate metadata
  x_metadata <-
    tibble::tibble(
      variable_name = colnames(x),
      variable_type = purrr::map_chr(x, function(y) class(y)[[1]]),
      variable_unit = purrr::map_chr(x, function(y) {
        if(inherits(y, "units")) {
          as.character(units(y))
        } else {
          NA_character_
        }
      })
    )
  x_metadata$variable_type[x_metadata$variable_type == "units"] <- "numeric"

  # export
  write.csv(x, file, row.names = FALSE)
  write.csv(x_metadata, paste0(stringr::str_remove(file, "\\.csv$"), "_metadata", stringr::str_extract(file, "\\.csv$")), row.names = FALSE)

}
