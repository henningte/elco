#' Changes units defined by the 'elco' to the respective generic units
#'
#' For example, `g_C` is changed to `g` and `mol_C` to `mol`. The function
#' does not test whether the result is sensible. E.g. one could have unit
#' `(g_C*g_H)/(g_sample*L)` (see the example) which `elco_make_units_generic()`
#' turns into `g/L`.
#'
#' @param x A units object in which to change units defined by the 'elco'
#' to the respective generic units.
#'
#' @return `x` with generic units.
#'
#' @examples
#' # changing g_C into g
#' units::as_units("g_C", mode = "standard") |>
#'   elco_make_units_generic()
#'
#' # changing mmol_C/kg_sample into mol/g
#' units::as_units("mmol_C/kg_sample", mode = "standard") |>
#'   elco_make_units_generic()
#'
#' # make sure to check whether the result makes sense
#' units::as_units("(g_C * g_H) / (g_sample * L)", mode = "standard") |>
#'   elco_make_units_generic()
#'
#'
#' @export
elco_make_units_generic <- function(x) {

  stopifnot(inherits(x, "units"))
  utils::data("elco_units", package = "elco")
  allowed_prefixes <- c(units::valid_udunits_prefixes()$symbol, "")
  allowed_prefixes_pattern <- paste0("^", allowed_prefixes)

  elco_convert_unit_to_generic <-
    function(.x, is_numerator) {
      is_selected_g <- stringr::str_remove(.x, pattern = allowed_prefixes_pattern) %in% elco_units$g
      if(all(is_selected_g)) {
        is_selected_g <- rep(FALSE, length(is_selected_g))
        is_selected_g[allowed_prefixes == ""] <- TRUE
      }
      is_selected_mol <- stringr::str_remove(.x, pattern = allowed_prefixes_pattern) %in% elco_units$mol
      if(all(is_selected_mol)) {
        is_selected_mol <- rep(FALSE, length(is_selected_mol))
        is_selected_mol[allowed_prefixes == ""] <- TRUE
      }
      if(any(is_selected_g)) {
        if(is_numerator) {
          paste0(allowed_prefixes[is_selected_g], "g/", .x)
        } else {
          paste0(.x, "/", allowed_prefixes[is_selected_g], "g")
        }
      } else if(any(is_selected_mol)) {
        if(is_numerator) {
          paste0(allowed_prefixes[is_selected_mol], "mol/", .x)
        } else {
          paste0(.x, "/", allowed_prefixes[is_selected_mol], "mol")
        }
      } else {
        character()
      }
    }

  x_units <- units(x)
  x_conversion_units <-
    c(
      lapply(x_units$numerator, elco_convert_unit_to_generic, is_numerator = TRUE) |>
        unlist(),
      lapply(x_units$denominator, elco_convert_unit_to_generic, is_numerator = FALSE) |>
        unlist()
    ) |>
    paste(collapse = " * ")

   x * units::set_units(1, value = x_conversion_units, mode = "standard")

}
