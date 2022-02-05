#' Creates an object of class \code{irms}
#'
#' \code{elco_new_irms} is the internal constructor function for objects of class
#' \code{irms}.
#' An object of class \code{irms} is a \code{\link[tibble]{tibble}} with a
#' sample in each row and a pre-scribed set of columns for metadata and
#' element contents. Objects of class \code{irms} are intended to be created
#' only internally.
#'
#' @param x A \code{data.frame} with a row for each measured sample or standard and the
#' following columns:
#' \describe{
#'   \item{file_id}{An integer increasing by 1 and starting from 1, representing a unique ID for each
#'   csv file that has been imported.}
#'   \item{measurement_id}{An integer increasing by 1 and starting from 1 for each imported file,
#'   representing a unique ID for each measurement per file.}
#'   \item{measurement_type}{A character vector representing the measurement type. One of "Sam" (the
#'   measurement is a measurement of a sample) or "Ref" (the measurement is a measurement of a
#'   standard/reference).}
#'   \item{sample_label}{A character vector with names for the samples. If \code{measurmeent_type == "Sam"},
#'   this is a label for the sample. If \code{measurement_type == "Ref"}, this is the name of the standard
#'   as given in \code{\link[elco:irms_standards]{irms_standards}}}.
#'   \item{sample_mass}{A quantities vector representing the mass of the sample [g] with measurmeent errors (these are set to 0 by default).}
#'   \item{time}{A POSIXct vector with the date and time when the sample was measured.}
#'   \item{file_name}{A character vector with the full path to the .RUN file containing the raw
#'   data for the corresponding measurement.}
#'   \item{15N_area}{A numeric vector with the area of the chromatogram peak for the N content and
#'   \eqn{\delta^{15}\text{N\textperthousand}} value.}
#'   \item{15N}{A numeric vector with the \eqn{\delta^{15}\text{N\textperthousand}} value.}
#'   \item{13C_area}{A numeric vector with the area of the chromatogram peak for the C content and
#'   \eqn{\delta^{13}\text{C\textperthousand}} value.}
#'   \item{13C}{A numeric vector with the \eqn{\delta^{13}\text{C\textperthousand}} value.}
#'   \item{18O}{A numeric vector with the \eqn{\delta^{18}\text{O\textperthousand}} value.}
#'   \item{C}{A numeric vector with the relative mass fraction of C [g/g].}
#'   \item{N}{A numeric vector with the relative mass fraction of N [g/g].}
#' }
#' @return An object of class \code{irms}. This is identical to \code{x}, but has
#' an additional class attribute.
elco_new_irms <- function(x) {

  # checks
  target_variables <- c("file_id", "measurement_id", "measurement_type", "sample_label",
                        "sample_mass", "time", "file_name", "15N_area", "15N", "13C_area",
                        "13C", "18O", "N", "C")

  cond <- !purrr::map_lgl(target_variables, function(y) y %in% colnames(x))
  if(any(cond)) {
    if(sum(cond) == 1) {
      rlang::abort(paste0("`x` must contain defined columns. Column ", target_variables[[cond]], " should exist, but is missing."))
    } else {
      rlang::abort(paste0("`x` must contain defined columns. Columns ", paste(target_variables[cond], collapse = ", "), " should exist, but are missing."))
    }
  }
  x_leftover <- x[, colnames(x)[!colnames(x) %in% target_variables]]
  x <- x[, target_variables] # sort columns
  target_variable_types <- c("integer", "integer", "character", "character", "quantities", "POSIXct",
                             "character", "numeric", "numeric", "numeric", "numeric", "numeric",
                             "elco", "elco")
  x_variable_types <- purrr::map_chr(x, function(x) class(x)[[1]])
  cond <- !purrr::map2_lgl(x_variable_types, target_variable_types, identical)
  if(any(cond)) {
    if(sum(cond) == 1) {
      rlang::abort(paste0("Column ", colnames(x)[[cond]]," should be of class ", target_variable_types[[cond]], ", but is of class ", x_variable_types[[cond]],"."))
    } else {
      rlang::abort(paste0("Columns ", paste(colnames(x)[cond], collapse = ", ")," should be of class ", paste(target_variable_types[cond], collapse = ", "), ", but are of class ", paste(x_variable_types[cond], collapse = ", "),"."))
    }
  }

  structure(cbind(x, x_leftover), class = c("irms", class(x)))

}

#' Checks if an object is of class \code{irms}.
#'
#' \code{elco_check_irms} checks if an object is of class \code{\link[elco:elco_new_irms]{irms}}.
#'
#' @param x An object.
#' @return An object of class \code{\link[elco:elco_new_irms]{irms}}.
#' @keywords internal
elco_check_irms <- function(x) {
  inherits(x, "irms")
}
