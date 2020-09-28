#' Creates an object of class \code{irms_std}
#'
#' \code{elco_new_irms_std} is the internal constructor function for objects of class
#' \code{irms_std}.
#' An object of class \code{irms} is an object of class \code{\link[elco_new_irms]{irms}}, but
#' contains only measurements on standards. Objects of class \code{irms} are intended to be created
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
#'   this is a label for the sample. If \code{measurement_type == "Ref"}, this is the name of a standard.
#'   Only values existing in \code{\link[elco:irms_standards]{irms_standards}} are allowed}.
#'   \item{sample_mass}{A units vector representing the mass of the sample [g].}
#'   \item{time}{A POSIXct vector with the date and time when the sample was measured.}
#'   \item{file_name}{A character vector with the full path to the .RUN file containing the raw
#'   data for the corresponding measurement.}
#'   \item{d15N_area}{A numeric vector with the area of the chromatogram peak for the N content and
#'   \eqn{\delta^{15}\text{N\textperthousand}} value.}
#'   \item{d15N}{A numeric vector with the \eqn{\delta^{15}\text{N\textperthousand}} value.}
#'   \item{d13C_area}{A numeric vector with the area of the chromatogram peak for the C content and
#'   \eqn{\delta^{13}\text{C\textperthousand}} value.}
#'   \item{d13C}{A numeric vector with the \eqn{\delta^{13}\text{C\textperthousand}} value.}
#'   \item{d18O}{A numeric vector with the \eqn{\delta^{18}\text{O\textperthousand}} value.}
#'   \item{C_m}{A numeric vector with the relative mass fraction of C.}
#'   \item{N_m}{A numeric vector with the relative mass fraction of N.}
#' }
#' @return An object of class \code{irms_std}. This is identical to \code{x}, but has
#' an additional class attribute.
elco_new_irms_std <- function(x) {

  # checks
  elco_check_irms(x)
  elco_irms_check_standards(x)

  structure(x, class = c("irms_std", class(x)))

}

#' Checks if an object is of class \code{irms_std}.
#'
#' \code{elco_check_irms_std} checks if an object is of class \code{\link[elco:elco_new_irms_std]{irms_std}}.
#'
#' @param x An object.
#' @return An object of class \code{\link[elco:elco_new_irms_std]{irms_std}}.
#' @keywords internal
elco_check_irms_std <- function(x) {
  x_sym <- as.character(rlang::get_expr(rlang::enquo(x)))
  if(!inherits(x, "irms_std"))
    rlang::abort(paste0("`", x_sym, "` must be of class `irms_std`, not ", class(x)[[1]], "."))
  x
}
