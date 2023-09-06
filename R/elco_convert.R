#' Helper function to allow batch conversion between units defined in the 'elco' package
#'
#' @param x A quantities vector with a custom unit defined in the 'elco' package
#' (either grams of an element, e.g. `"g_C"`, or mols of an element, e.g.
#' `"mol_C"`) to convert (to mols or grams, respectively).
#'
#' @param to A character value defining the new unit to which to convert `x`.
#' Currently, the following values are allowed (including the same version with
#' prefixes, e.g. `"mmol"` or `"kg"`):
#' \describe{
#'   \item{`"mol"`}{Converts the numerator of `x` to mol. The numerator of `x`
#'   must be in grams of the element.}
#'   \item{`"g"`}{Converts the numerator of `x` to g. The numerator of `x`
#'   must be in mols of the element.}
#' }
#'
#' @return `x` with converted unit. An error is returned when unit conversion is
#' not sensible.
#'
#' @examples
#' # convert from mass to amount
#' C <- quantities::set_quantities(1, errors = 0.0, unit = "g_C")
#' elco_convert(C, to = "mol")
#' C <- quantities::set_quantities(1, errors = 0.0, unit = "kg_C")
#' elco_convert(C, to = "mmol")
#'
#' # convert from amount to mass
#' C <- quantities::set_quantities(1, errors = 0.0, unit = "mol_C")
#' elco_convert(C, to = "g")
#' C <- quantities::set_quantities(1, errors = 0.0, unit = "kmol_C")
#' elco_convert(C, to = "Pg")
#'
#' # conversion works also when the unit denominator is not empty
#' C <- quantities::set_quantities(1, errors = 0.0, unit = "kg_C/mg_sample")
#' elco_convert(C, to = "mmol")
#'
#' # the function is useful for batch conversion, e.g.:
#' set.seed(324)
#' tibble::tibble(
#'   C = quantities::set_quantities(rnorm(5), errors = 0, unit = "g_C/g_sample"),
#'   N = quantities::set_quantities(rnorm(5), errors = 0, unit = "g_N/g_sample")
#' ) %>%
#'   dplyr::mutate(
#'     dplyr::across(dplyr::everything(), \(.x) elco_convert(.x, to = "mmol"))
#'   )
#'
#' @export
elco_convert <- function(x, to) {

  x_element <-
    units(x)$numerator %>%
    stringr::str_extract(pattern = "_.+$")

  new_unit <- units(x)
  new_unit$numerator <- paste0(to, x_element)

  units(x) <- new_unit

  x

}
