#' Creates an object of class xrf.
#'
#' \code{elco_new_xrf} is the internal constructor function for objects of class
#' \code{xrf}.
#' An object of class \code{xrf} is a \code{\link[tibble]{tibble}} with a
#' sample in each row and a pre-scribed set of columns for metadata and
#' element contents. Objects of class \code{xrf} are intended to be created
#' only internally.
#'
#' @param x A \code{data.frame} with a row for each measured sample and the
#' following columns:
#' \describe{
#'   \item{sample_id}{A character vector with the sample labels.}
#'   \item{method_name}{A character vector with the names of the measurement methods used.}
#'   \item{time}{A POSIXct vector with the date and time when the sample was measured.}
#'   \item{Na}{A units vector with the measured Na content [ppm].}
#'   \item{Na_flag}{A logical vector indicating if the calibration range was exceeded for \code{Na}
#'   (\code{TRUE}) or not (\code{FALSE}).}
#'   \item{Mg}{A units vector with the measured Mg content [ppm].}
#'   \item{Mg_flag}{A logical vector indicating if the calibration range was exceeded for \code{Mg}
#'   (\code{TRUE}) or not (\code{FALSE}).}
#'   \item{Al}{A units vector with the measured Al content [ppm].}
#'   \item{Al_flag}{A logical vector indicating if the calibration range was exceeded for \code{Al}
#'   (\code{TRUE}) or not (\code{FALSE}).}
#'   \item{Si}{A units vector with the measured Si content [ppm].}
#'   \item{Si_flag}{A logical vector indicating if the calibration range was exceeded for \code{Si}
#'   (\code{TRUE}) or not (\code{FALSE}).}
#'   \item{P}{A units vector with the measured P content [ppm].}
#'   \item{P_flag}{A logical vector indicating if the calibration range was exceeded for \code{P}
#'   (\code{TRUE}) or not (\code{FALSE}).}
#'   \item{S}{A units vector with the measured S content [ppm].}
#'   \item{S_flag}{A logical vector indicating if the calibration range was exceeded for \code{S}
#'   (\code{TRUE}) or not (\code{FALSE}).}
#'   \item{Cl}{A units vector with the measured Cl content [ppm].}
#'   \item{Cl_flag}{A logical vector indicating if the calibration range was exceeded for \code{Cl}
#'   (\code{TRUE}) or not (\code{FALSE}).}
#'   \item{K}{A units vector with the measured K content [ppm].}
#'   \item{K_flag}{A logical vector indicating if the calibration range was exceeded for \code{K}
#'   (\code{TRUE}) or not (\code{FALSE}).}
#'   \item{Ca}{A units vector with the measured Ca content [ppm].}
#'   \item{Ca_flag}{A logical vector indicating if the calibration range was exceeded for \code{Ca}
#'   (\code{TRUE}) or not (\code{FALSE}).}
#'   \item{Ti}{A units vector with the measured Ti content [ppm].}
#'   \item{Ti_flag}{A logical vector indicating if the calibration range was exceeded for \code{Ti}
#'   (\code{TRUE}) or not (\code{FALSE}).}
#'   \item{Cr}{A units vector with the measured Cr content [ppm].}
#'   \item{Cr_flag}{A logical vector indicating if the calibration range was exceeded for \code{Cr}
#'   (\code{TRUE}) or not (\code{FALSE}).}
#'   \item{Mn}{A units vector with the measured Mn content [ppm].}
#'   \item{Mn_flag}{A logical vector indicating if the calibration range was exceeded for \code{Mn}
#'   (\code{TRUE}) or not (\code{FALSE}).}
#'   \item{Fe}{A units vector with the measured Fe content [ppm].}
#'   \item{Fe_flag}{A logical vector indicating if the calibration range was exceeded for \code{Fe}
#'   (\code{TRUE}) or not (\code{FALSE}).}
#'   \item{Cu}{A units vector with the measured Cu content [ppm].}
#'   \item{Cu_flag}{A logical vector indicating if the calibration range was exceeded for \code{Cu}
#'   (\code{TRUE}) or not (\code{FALSE}).}
#'   \item{Zn}{A units vector with the measured Zn content [ppm].}
#'   \item{Zn_flag}{A logical vector indicating if the calibration range was exceeded for \code{Zn}
#'   (\code{TRUE}) or not (\code{FALSE}).}
#'   \item{As}{A units vector with the measured As content [ppm].}
#'   \item{As_flag}{A logical vector indicating if the calibration range was exceeded for \code{As}
#'   (\code{TRUE}) or not (\code{FALSE}).}
#'   \item{Br}{A units vector with the measured Br content [ppm].}
#'   \item{Br_flag}{A logical vector indicating if the calibration range was exceeded for \code{Br}
#'   (\code{TRUE}) or not (\code{FALSE}).}
#'   \item{Rb}{A units vector with the measured Rb content [ppm].}
#'   \item{Rb_flag}{A logical vector indicating if the calibration range was exceeded for \code{Rb}
#'   (\code{TRUE}) or not (\code{FALSE}).}
#'   \item{Sr}{A units vector with the measured Sr content [ppm].}
#'   \item{Sr_flag}{A logical vector indicating if the calibration range was exceeded for \code{Sr}
#'   (\code{TRUE}) or not (\code{FALSE}).}
#'   \item{Ba}{A units vector with the measured Ba content [ppm].}
#'   \item{Ba_flag}{A logical vector indicating if the calibration range was exceeded for \code{Ba}
#'   (\code{TRUE}) or not (\code{FALSE}).}
#'   \item{Pb}{A units vector with the measured Pb content [ppm].}
#'   \item{Pb_flag}{A logical vector indicating if the calibration range was exceeded for \code{Pb}
#'   (\code{TRUE}) or not (\code{FALSE}).}
#'   \item{C6H10O5N}{A units vector with the measured C6H10O5N content [ppm].}
#'   \item{C6H10O5N_flag}{A logical vector indicating if the calibration range was exceeded for \code{C6H10O5N}
#'   (\code{TRUE}) or not (\code{FALSE}).}
#' }
#' @return An object of class \code{xrf}. This is identical to \code{x}, but has
#' an additional class attribute.
elco_new_xrf <- function(x) {

  # checks
  target_variables <- c("sample_id", "method_name", "time", "Na", "Na_flag",
                        "Mg", "Mg_flag", "Al", "Al_flag", "Si", "Si_flag", "P",
                        "P_flag", "S", "S_flag", "Cl", "Cl_flag", "K", "K_flag",
                        "Ca", "Ca_flag", "Ti", "Ti_flag", "Cr", "Cr_flag", "Mn",
                        "Mn_flag", "Fe", "Fe_flag", "Cu", "Cu_flag", "Zn", "Zn_flag",
                        "As", "As_flag", "Br", "Br_flag", "Rb", "Rb_flag", "Sr",
                        "Sr_flag", "Ba", "Ba_flag", "Pb", "Pb_flag", "C6H10O5N",
                        "C6H10O5N_flag")

  cond <- !purrr::map_lgl(colnames(x), function(y) y %in% target_variables)
  if(any(cond)) {
    if(sum(cond) == 1) {
      rlang::abort(paste0("`x` must contain defined columns. Column ", target_variables[[cond]], " should exist, but is missing."))
    } else {
      rlang::abort(paste0("`x` must contain defined columns. Columns ", paste(target_variables[cond], collapse = ", "), " should exist, but are missing."))
    }
  }
  x <- x[, target_variables] # sort columns
  target_variable_types <- c("character", "character", "POSIXct", rep(c("elco", "logical"), 21), c("quantities", "logical"))
  x_variable_types <- purrr::map_chr(x, function(x) class(x)[[1]])
  cond <- !purrr::map2_lgl(x_variable_types, target_variable_types, identical)
  if(any(cond)) {
    if(sum(cond) == 1) {
      rlang::abort(paste0("Column ", colnames(x)[[cond]]," should be of class ", target_variable_types[[cond]], ", but is of class ", x_variable_types[[cond]],"."))
    } else {
      rlang::abort(paste0("Columns ", colnames(x)[cond]," should be of class ", paste(target_variable_types[cond], collapse = ", "), ", but are of class ", paste(x_variable_types[cond], collapse = ", "),"."))
    }
  }

  structure(x, class = c("xrf", class(x)))

}

#' Checks if an object is of class \code{xrf}.
#'
#' \code{elco_check_xrf} checks if an object is of class \code{\link[elco:elco_new_xrf]{xrf}}.
#'
#' @param x An object.
#' @return An object of class \code{\link[elco:elco_new_xrf]{xrf}}.
#' @keywords internal
elco_check_xrf <- function(x) {
  x_sym <- as.character(rlang::get_expr(rlang::enquo(x)))
  if(!inherits(x, "xrf"))
    rlang::abort(paste0("`", x_sym, "` must be of class `xrf`, not ", class(x)[[1]], "."))
  x
}
