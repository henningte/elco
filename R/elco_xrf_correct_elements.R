#' Heuristic function to correct XRF element contents of samples measured with sample masses smaller than used for the instrument calibration.
#'
#' \code{elco_xrf_correct_elements} is a heuristic function to correct measured element contents of samples
#' during XRF analysis. Elements for which to perform a correction can be selected. Only batch processing of
#' files is possible.
#' The correction is based on a reference calibration data set where five different samples have been measured
#' with four different masses (500 mg, the current reference with which the instrument is calibrated, 300, 250, and 200 mg).
#' For each measured element and measurement, the ratio of the element content measured with mass x and the element content
#' measured with reference mass (500 mg) are divided. Deviations from 1 are due to measurement noise and effects of
#' mass differences on the recorded signal. The correction targets the latter, but also propagates uncertainty about the
#' former. The computed ratios are modeled with a linear mixed model, where there is a random intercept for each sample and
#' the sample mass is treated as factor. Sample mass is treated as factor because the variance is not homogeneous between
#' different masses (a future version might include a procedure where the variance is also modeled in dependence of the sample mass).
#' A consequence is that the correction procedure currently can only be applied to values measured with sample masses
#' as used in the calibration data set.
#' The regression models assume a Gamma distribution since all computed ratios are \eqn{\ge0}. The models are fit with
#' \code{\link[rstanarm:rstanarm-package]{rstanarm}} (\code{\link[rstanarm:stan_glmer]{stan_glmer}}) using default
#' priors. From the models' predictive distributions, the predicted correction factors are sampled and its mean and
#' standard deviation stored.
#' During correction, the measured value to correct and a sample mass are given. \code{elco_xrf_correct_elements}
#' searches the respective correction factor and its standard deviation and multiplies it with the
#' measured value. The \code{\link[quantities]{quantities}} package is used to propagate the uncertainty in the
#' correction factor into the corrected value.
#'
#' @param x An object of class \code{data.frame}.
#' @param element A character vector representing the chemical elements for which to correct the mass fraction
#' values. This must be one in \code{\link[elco:xrf_calibration]{xrf_calibration$el_symbol}}. Can alternatively be set to
#' \code{NULL} to correct all chemical elements possible.
#' @param sample_mass A vector of class \code{\link[units:units]{units}} with an element for each row in \code{x}
#' and \code{"mg"} as measurement unit.
#' @param xrf_calibration A data frame which stores correction factors for combinations of chemical elements and sample
#' masses, with a row for each element and sample mass and three columns:
#' \describe{
#'   \item{el_symbol}{A character vector with the chemical element symbol which the
#'   correction factor in \code{correction_factor} applies.}
#'   \item{sample_mass}{A character vector with the sample mass [mg] for which the
#'   correction factor in \code{correction_factor} applies.}
#'   \item{correction_factor}{A \code{\link[quantities:quantities]{quantities}} vector
#'   with the correction factors.}
#' }
#' Alternatively, \code{xrf_calibration} can be set to \code{NULL}. In this case,
#' the built-in \code{\link{xrf_calibration}} is used. Check beforehand if this
#' set of calibration factors is appropriate!
#' @note Currently, the calibration procedure is rather crude and not based on a large amounf of samples. This
#' causes relative large uncertainties.
#' @return \code{x} with corrected element content values.
#' @export
elco_xrf_correct_elements <- function(x,
                                      element = NULL,
                                      sample_mass,
                                      xrf_calibration = NULL) {

  # checks
  if(!is.data.frame(x)) {
    rlang::abort("`x` must be a data.frame, but is of class ", class(x)[[1]],".")
  }
  if(!(is.character(element) || is.null(element))) {
    rlang::abort(paste0("`element` must be a character vector or NULL, but is of class ", class(element)[[1]], "."))
  }
  if(!(is.data.frame(xrf_calibration) || is.null(xrf_calibration))) {
    rlang::abort(paste0("`xrf_calibration` must be a data frame or NULL, but is of class ", class(xrf_calibration)[[1]], "."))
  }
  index_el <- elco::elco_identify_elco_df(x)
  x_element <- purrr::map_chr(x[, index_el, drop = FALSE], function(x) {
    attr(x, "el_symbol")
  })
  if(is.null(xrf_calibration)) {
    xrf_calibration <- elco::xrf_calibration
  }
  stopifnot(colnames(xrf_calibration) %in% c("el_symbol", "sample_mass", "correction_factor"))
  stopifnot(inherits(xrf_calibration$correction_factor, "quantities") | is.character(xrf_calibration$el_symbol) | is.character(sample_mass))
  if(is.null(element)) {
    element <- x_element
    element <- element[element %in% xrf_calibration$el_symbol]
    print(paste0("Correcting element contents for ", paste(element, collapse = ", "), "."))
  }
  cond <- !purrr::map_lgl(element, function(x) x %in% xrf_calibration$el_symbol)
  if(any(cond)) {
    rlang::abort(paste0("All elements in `element` must be in `elco::xrf_calibration$el_symbol`, but " , paste(element[cond], collapse = ", "), " is/are not."))
  }
  if(!is.numeric(sample_mass)) {
    rlang::abort(paste0("`sample_mass` must be a numeric vector, but is of class ", class(sample_mass)[[1]], "."))
  }
  if(is.null(attr(sample_mass, "units"))) {
    rlang::abort('`sample_mass` must be a units object, but has no attribute "units".')
  }
  cond <- as.character(units(sample_mass))
  if(cond != "mg") {
    rlang::abort(paste0("`sample_mass` must be given in mg, but has units ", cond,"."))
  }
  cond <- !sample_mass %in% unique(xrf_calibration$sample_mass)
  if(any(cond)) {
    rlang::warn(paste0("In the current version of the function, all values of `sample_mass` must be in ", paste(unique(elco::xrf_calibration$sample_mass), collapse = ", "), ". This is not the case for elements ", paste(which(cond), collapse = ", "), ". These elements are skipped"))
  }

  index_sample_mass <- !cond
  index_el <- which(index_el)[x_element %in% element]

  x[index_sample_mass, index_el] <- purrr::map2_df(index_el, names(index_el), function(i, z) {

    y <- x[index_sample_mass, i]

    factors <-
      data.frame(
      sample_mass = as.character(sample_mass[index_sample_mass]),
      el_symbol = z,
      stringsAsFactors = FALSE,
      row.names = NULL
    )
    factors <- dplyr::left_join(factors, xrf_calibration, by = c("el_symbol", "sample_mass"))
    y_unit <- as.character(units(y))
    y <- elco::drop_elco(y)/factors$correction_factor
    y <- units::set_units(y, "g/g")
    units(y) <- with(units::ud_units, y_unit)
    y

  })

  x

}
