#' C and N contents and \eqn{^{13}}C and \eqn{^{15}}N \eqn{\delta\text{\textperthousand}} of IRMS standards.
#'
#' A \code{data.frame} with names, C and N contents and \eqn{^{13}}C and
#' \eqn{^{15}}N \eqn{\delta\text{\textperthousand}} of IRMS standards.
#'
#' @format A data frame with 4 rows and 5 columns:
#' \describe{
#'   \item{standard_name}{A character vector with the names of the standards as used in the
#'   vendor device of the IRMS.}
#'   \item{C_m}{A numeric vector with the relative mass fraction of C of the standards.}
#'   \item{N_m}{A numeric vector with the relative mass fraction of N of the standards.}
#'   \item{d13C}{A numeric vector with the \eqn{\delta\text{\textperthousand}^{13}}C value of the standards.}
#'   \item{d15N}{A numeric vector with the \eqn{\delta\text{\textperthousand}^{15}}N value of the standards.}
#' }
"irms_standards"
