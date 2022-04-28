#' C and N contents and \eqn{^{13}}C and \eqn{^{15}}N \eqn{\delta\text{\textperthousand}} of IRMS standards
#'
#' A `data.frame` with names, C and N contents and \eqn{^{13}}C and
#' \eqn{^{15}}N \eqn{\delta\text{\textperthousand}} of IRMS standards.
#'
#' @source
#' \describe{
#'   \item{IAEA 600}{\insertRef{Coplen.2006}{elco}}
#'   \item{BBOT_CN., WFS}{\insertRef{IVAAnalysentechnikGmbHuCoKGt.2016}{elco}}
#' }
#'
#' @format A data frame with 4 rows and 5 columns:
#' \describe{
#'   \item{standard_name}{A character vector with the names of the standards as used in the
#'   vendor device of the IRMS.}
#'   \item{C}{A numeric vector with the relative mass fraction of C of the
#'     standards \[g/g\].}
#'   \item{N}{A numeric vector with the relative mass fraction of N of the
#'     standards \[g/g\].}
#'   \item{S}{A numeric vector with the relative mass fraction of S of the
#'     standards \[g/g\].}
#'   \item{H}{A numeric vector with the relative mass fraction of H of the
#'     standards \[g/g\].}
#'   \item{O}{A numeric vector with the relative mass fraction of O of the
#'     standards \[g/g\].}
#'   \item{13C}{A numeric vector with the
#'     \eqn{\delta\text{\textperthousand}^{13}}C value of the standards.}
#'   \item{15N}{A numeric vector with the
#'     \eqn{\delta\text{\textperthousand}^{15}}N value of the standards.}
#'   \item{threshold_13C}{A numeric vector with a threshold for the
#'     \eqn{\delta\text{\textperthousand}^{13}}C value of the standards. The thresholds indicate the (by expert knowledge) estimated 95-% confidence interval half-width for standards measured at our working group's laboratory. If after isotope signature correction with [`elco::elco_irms_correct_isotopes()`], values for a standard exceed this limit, a warning is printed. Note that not for all standards such values are defined.}
#'   \item{threshold_15N}{A numeric vector with a threshold for the
#'     \eqn{\delta\text{\textperthousand}^{15}}N value of the standards. The thresholds indicate the (by expert knowledge) estimated 95-% confidence interval half-width for standards measured at our working group's laboratory. If after isotope signature correction with [`elco::elco_irms_correct_isotopes()`], values for a standard exceed this limit, a warning is printed. Note that not for all standards such values are defined.}
#'   \item{source}{A character vector with the bibtex keys to the references
#'     where the data was derived from. If `NA`, values are own, uncertified,
#'     measurements.}
#' }
#'
"irms_standards"

#' Simulated C, H, N, O contents, and sample masses for five samples
#'
#' A `data.frame` with simulated C, H, N, O contents, and sample masses for five
#' samples.
#'
#' @format A data frame with 5 rows and 5 columns:
#' \describe{
#'   \item{C}{A numeric vector with the relative mass fraction of C \[g/g\].}
#'   \item{H}{A numeric vector with the relative mass fraction of H \[g/g\].}
#'   \item{N}{A numeric vector with the relative mass fraction of N \[g/g\].}
#'   \item{O}{A numeric vector with the relative mass fraction of O \[g/g\].}
#'   \item{sample_mass}{A numeric vector with the mass of the samples \[mg\].}
#' }
#'
"chno"

#' Correction factors for XRF element contents for samples with differing masses
#'
#' A `data.frame` with chemical element-specific heuristic correction factors
#' for XRF element contents (relative to a reference mass of 500 mg) measured
#' with masses smaller than the reference mass.
#'
#' @note This is only a sample data frame which may not be suitable to correct
#' a specific measured data set.
#'
#' @format A data frame with 60 rows and 3 columns:
#' \describe{
#'   \item{el_symbol}{A character vector with the symbols of chemical elements
#'     for which correction factors were computed.}
#'   \item{sample_mass}{A character vector with discrete sample mass values
#'     \[mg\] for which the correction factors were computed.}
#'   \item{correction_factor}{A `quantities` vector with the mean, standard
#'     deviation, and unit for each correction factor. See
#'     [elco::elco_xrf_correct_elements()]} for how these values are used.
#' }
#'
"xrf_calibration"

#' Allowed units for `elco` objects
#'
#' A data frame listing allowed units for `elco` objects.
#'
#' @format A data frame with 96 rows and 2 columns:
#' \describe{
#'   \item{type}{A character value classifying measurement units by their
#'     nominator and denominator.}
#'   \item{unit_symbol}{A character value representing a measurement unit.}
#' }
#'
"el_units_allowed"
