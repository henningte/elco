#' Creates an object of class xrf.
#'
#' `elco_new_xrf` is the internal constructor function for objects of class
#' `xrf`.
#' An object of class `xrf` is a [tibble::tibble()] with a
#' sample in each row and a prescribed set of columns for metadata and
#' element contents. Objects of class `xrf` are intended to be created
#' only internally.
#'
#' @param x A `data.frame` with a row for each measured sample and the
#' following columns:
#' \describe{
#'   \item{sample_id}{A character vector with the sample labels.}
#'   \item{method_name}{A character vector with the names of the measurement methods used.}
#'   \item{time}{A POSIXct vector with the date and time when the sample was measured.}
#'   \item{Na}{A units vector with the measured Na content \[\eqn{\mu}g/g\].}
#'   \item{Na_flag}{A logical vector indicating if the calibration range was exceeded for `Na`
#'   (`TRUE`) or not (`FALSE`).}
#'   \item{Mg}{A units vector with the measured Mg content \[\eqn{\mu}g/g\].}
#'   \item{Mg_flag}{A logical vector indicating if the calibration range was exceeded for `Mg`
#'   (`TRUE`) or not (`FALSE`).}
#'   \item{Al}{A units vector with the measured Al content \[\eqn{\mu}g/g\].}
#'   \item{Al_flag}{A logical vector indicating if the calibration range was exceeded for `Al`
#'   (`TRUE`) or not (`FALSE`).}
#'   \item{Si}{A units vector with the measured Si content \[\eqn{\mu}g/g\].}
#'   \item{Si_flag}{A logical vector indicating if the calibration range was exceeded for `Si`
#'   (`TRUE`) or not (`FALSE`).}
#'   \item{P}{A units vector with the measured P content \[\eqn{\mu}g/g\].}
#'   \item{P_flag}{A logical vector indicating if the calibration range was exceeded for `P`
#'   (`TRUE`) or not (`FALSE`).}
#'   \item{S}{A units vector with the measured S content \[\eqn{\mu}g/g\].}
#'   \item{S_flag}{A logical vector indicating if the calibration range was exceeded for `S`
#'   (`TRUE`) or not (`FALSE`).}
#'   \item{Cl}{A units vector with the measured Cl content \[\eqn{\mu}g/g\].}
#'   \item{Cl_flag}{A logical vector indicating if the calibration range was exceeded for `Cl`
#'   (`TRUE`) or not (`FALSE`).}
#'   \item{K}{A units vector with the measured K content \[\eqn{\mu}g/g\].}
#'   \item{K_flag}{A logical vector indicating if the calibration range was exceeded for `K`
#'   (`TRUE`) or not (`FALSE`).}
#'   \item{Ca}{A units vector with the measured Ca content \[\eqn{\mu}g/g\].}
#'   \item{Ca_flag}{A logical vector indicating if the calibration range was exceeded for `Ca`
#'   (`TRUE`) or not (`FALSE`).}
#'   \item{Ti}{A units vector with the measured Ti content \[\eqn{\mu}g/g\].}
#'   \item{Ti_flag}{A logical vector indicating if the calibration range was exceeded for `Ti`
#'   (`TRUE`) or not (`FALSE`).}
#'   \item{Cr}{A units vector with the measured Cr content \[\eqn{\mu}g/g\].}
#'   \item{Cr_flag}{A logical vector indicating if the calibration range was exceeded for `Cr`
#'   (`TRUE`) or not (`FALSE`).}
#'   \item{Mn}{A units vector with the measured Mn content \[\eqn{\mu}g/g\].}
#'   \item{Mn_flag}{A logical vector indicating if the calibration range was exceeded for `Mn`
#'   (`TRUE`) or not (`FALSE`).}
#'   \item{Fe}{A units vector with the measured Fe content \[\eqn{\mu}g/g\].}
#'   \item{Fe_flag}{A logical vector indicating if the calibration range was exceeded for `Fe`
#'   (`TRUE`) or not (`FALSE`).}
#'   \item{Cu}{A units vector with the measured Cu content \[\eqn{\mu}g/g\].}
#'   \item{Cu_flag}{A logical vector indicating if the calibration range was exceeded for `Cu`
#'   (`TRUE`) or not (`FALSE`).}
#'   \item{Zn}{A units vector with the measured Zn content \[\eqn{\mu}g/g\].}
#'   \item{Zn_flag}{A logical vector indicating if the calibration range was exceeded for `Zn`
#'   (`TRUE`) or not (`FALSE`).}
#'   \item{As}{A units vector with the measured As content \[\eqn{\mu}g/g\].}
#'   \item{As_flag}{A logical vector indicating if the calibration range was exceeded for `As`
#'   (`TRUE`) or not (`FALSE`).}
#'   \item{Br}{A units vector with the measured Br content \[\eqn{\mu}g/g\].}
#'   \item{Br_flag}{A logical vector indicating if the calibration range was exceeded for `Br`
#'   (`TRUE`) or not (`FALSE`).}
#'   \item{Rb}{A units vector with the measured Rb content \[\eqn{\mu}g/g\].}
#'   \item{Rb_flag}{A logical vector indicating if the calibration range was exceeded for `Rb`
#'   (`TRUE`) or not (`FALSE`).}
#'   \item{Sr}{A units vector with the measured Sr content \[\eqn{\mu}g/g\].}
#'   \item{Sr_flag}{A logical vector indicating if the calibration range was exceeded for `Sr`
#'   (`TRUE`) or not (`FALSE`).}
#'   \item{Ba}{A units vector with the measured Ba content \[\eqn{\mu}g/g\].}
#'   \item{Ba_flag}{A logical vector indicating if the calibration range was exceeded for `Ba`
#'   (`TRUE`) or not (`FALSE`).}
#'   \item{Pb}{A units vector with the measured Pb content \[\eqn{\mu}g/g\].}
#'   \item{Pb_flag}{A logical vector indicating if the calibration range was exceeded for `Pb`
#'   (`TRUE`) or not (`FALSE`).}
#'   \item{C6H10O5N}{A units vector with the measured C6H10O5N content \[\eqn{\mu}g/g\].}
#'   \item{C6H10O5N_flag}{A logical vector indicating if the calibration range was exceeded for `C6H10O5N`
#'   (`TRUE`) or not (`FALSE`).}
#' }
#' @return An object of class `xrf`. This is identical to `x`, but has
#' an additional class attribute.
#'
#' @export
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

#' Checks if an object is of class `xrf`.
#'
#' `elco_check_xrf` checks if an object is of class [`xrf()`][elco::elco_new_xrf].
#'
#' @param x An object.
#' @return An object of class [`xrf()`][elco::elco_new_xrf].
#' @keywords internal
elco_check_xrf <- function(x) {
  x_sym <- as.character(rlang::get_expr(rlang::enquo(x)))
  if(!inherits(x, "xrf"))
    rlang::abort(paste0("`", x_sym, "` must be of class `xrf`, not ", class(x)[[1]], "."))
  x
}
