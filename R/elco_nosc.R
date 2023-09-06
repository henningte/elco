#' Carbon oxidation state, carbon oxidative ratio, and degree of unsaturation
#'
#' Functions to compute the carbon oxidation state, carbon oxidative ratio, and
#' degree of unsaturation with [`quantities`][quantities::quantities] objects.
#'
#' @describeIn elco_nosc Computes the carbon oxidation
#' state using the elemental composition of C, H, N, O.
#'
#' @param C A quantities object \[mol_C\].
#'
#' @param H A quantities object \[mol_H\].
#'
#' @param N A quantities object \[mol_N\].
#'
#' @param O A quantities object \[mol_O\].
#'
#' @return
#' - `elco_nosc`: A [`quantities`][quantities::quantities] object with the carbon
#' oxidation state.
#'
#' @examples
#' # sample data with elemental contents in mol
#' d <-
#'   elco::chno %>%
#'   dplyr::mutate(
#'     dplyr::across(
#'       ! dplyr::all_of("sample_mass"),
#'       function(.x) elco_convert(.x * sample_mass, to = "mol")
#'     )
#'   )
#'
#' ## NOSC
#' d %>%
#'   dplyr::mutate(nosc = elco_nosc(C = C, H = H, N = N, O = O))
#'
#' @export
elco_nosc <- function(C, H, N, O) {

  elements <-
    list(C = C, H = H, N = N, O = O) %>%
    purrr::map(elco_convert, to = "mol") %>%
    purrr::map(units::drop_units) %>%
    purrr::map(units::set_units, value = "mol", mode = "standard")

  (quantities::set_quantities(2, unit = "1", errors = 0) * elements$O - elements$H + quantities::set_quantities(3, unit = "1", errors = 0) * elements$N)/elements$C

}


#' Computes the carbon oxidative ratio.
#'
#' @describeIn elco_nosc Computes the carbon oxidation
#' state using the elemental composition of C, H, N, O.
#'
#' @return
#' - `elco_or`: A [`quantities`][quantities::quantities] object with the
#' oxidative ratio.
#'
#' @examples
#' ## oxidative ratio
#' d %>%
#'   dplyr::mutate(or = elco_or(C = C, H = H, N = N, O = O))
#'
#' @export
elco_or <- function(C, H, N, O) {

  nosc <- elco_nosc(C = C, H = H, N = N, O = O)

  elements <-
    list(C = C, H = H, N = N, O = O) %>%
    purrr::map(elco_convert, to = "mol") %>%
    purrr::map(units::drop_units) %>%
    purrr::map(units::set_units, value = "mol", mode = "standard")

  nosc/quantities::set_quantities(4, unit = "1", errors = 0) + (quantities::set_quantities(3, unit = "1", errors = 0) * elements$N)/(quantities::set_quantities(4, unit = "1", errors = 0) * elements$C)

}

#' Computes the degree of unsaturation.
#'
#' @describeIn elco_nosc Computes the carbon degree
#' of unsaturation using the elemental composition of C, H, N.
#'
#' @return
#' - `elco_du`: A [`quantities`][quantities::quantities] object with the degree
#' of unsaturation.
#'
#' @examples
#' ## degree of unsaturation
#' d %>%
#'   dplyr::mutate(du = elco_du(C = C, H = H, N = N))
#'
#' @export
elco_du <- function(C, H, N) {

  elements <-
    list(C = C, H = H, N = N) %>%
    purrr::map(elco_convert, to = "mol") %>%
    purrr::map(units::drop_units) %>%
    purrr::map(units::set_units, value = "mol", mode = "standard")

  elements$C - elements$H/quantities::set_quantities(2, unit = "1", errors = 0) - elements$N/quantities::set_quantities(2, unit = "1", errors = 0) + quantities::set_quantities(1, unit = "mol", errors = 0)

}
