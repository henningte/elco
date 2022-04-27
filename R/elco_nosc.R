#' Carbon oxidation state, carbon oxidative ratio, and degree of unsaturation.
#'
#' Functions to compute the carbon oxidation state, carbon oxidative ratio, and
#' degree of unsaturation with objects of class [`elco()`][elco::elco_new_elco].
#'
#' @describeIn elco_nosc Computes the carbon oxidation
#' state using the elemental composition of C, H, N, O.
#'
#' @param c An object of class `elco` with the amount of C \[mol\].
#' @param h An object of class `elco` with the amount of H \[mol\].
#' @param n An object of class `elco` with the amount of N \[mol\].
#' @param o An object of class `elco` with the amount of O \[mol\].
#' @return An object of class [quantities::quantities()] with the carbon oxidation state.
#' @export
elco_nosc <- function(c,
                      h,
                      n,
                      o) {

  elco_chno_check(c = c, h = h, n = n, o = o)
  (quantities::set_quantities(2, unit = "1", errors = 0) * elco_drop_elco(o) - elco_drop_elco(h) + quantities::set_quantities(3, unit = "1", errors = 0) * elco_drop_elco(n))/elco_drop_elco(c)

}


#' Computes the carbon oxidative ratio.
#'
#' @describeIn elco_nosc Computes the carbon oxidation
#' state using the elemental composition of C, H, N, O.
#' @return A numeric vector with the oxidative ratio.
#' @export
elco_or <- function(c,
                    h,
                    n,
                    o) {

  nosc <- elco_nosc(c = c, h = h, n = n, o = o)
  nosc/quantities::set_quantities(4, unit = "1", errors = 0) + (quantities::set_quantities(3, unit = "1", errors = 0) * elco_drop_elco(n))/(quantities::set_quantities(4, unit = "1", errors = 0) * elco_drop_elco(c))

}

#' Computes the degree of unsaturation.
#'
#' @describeIn elco_nosc Computes the carbon degree
#' of unsaturation using the elemental composition of C, H, N.
#' @return A numeric vector with the oxidative ratio.
#' @export
elco_du <- function(c,
                    h,
                    n) {

  elco_chno_check(c = c, h = h, n = n, o = NULL)
  elco_drop_elco(c) - elco_drop_elco(h)/quantities::set_quantities(2, unit = "1", errors = 0) - elco_drop_elco(n)/quantities::set_quantities(2, unit = "1", errors = 0) + quantities::set_quantities(1, unit = "mol", errors = 0)

}

#' helper function to check inputs
#'
elco_chno_check <- function(c = NULL, h = NULL, n = NULL, o = NULL) {
  l <- list(c = c, h = h, n = n, o = o)
  l <- l[!purrr::map_lgl(l, is.null)]
  l_lengths <- purrr::map_dbl(l, length)
  cond <- length(unique(l_lengths))
  if(cond > 1) {
    rlang::abort("All, c, h, n, o, must have the same length.")
  }
  cond <- purrr::map_lgl(l, elco_check_elco)
  if(!all(cond)) {
    rlang::abort("All, c, h, n, o, must be of class elco.")
  }
  l_element_symbols <- purrr::map_chr(l, attr, "el_symbol")
  cond <- !purrr::map2_lgl(l_element_symbols, names(l), function(x, y) {
    x == toupper(y)
  })
  if(any(cond)) {
    if(sum(cond) == 1) {
      rlang::abort(paste0("`", names(l)[cond], "` does not match the element symbol given in the elco object which is ", l_element_symbols[cond], "."))
    } else {
      rlang::abort(paste0(paste(paste0("`", names(l)[cond], "`"), collapse = ", "), " do not match the element symbols given in the elco object which are ", paste(l_element_symbols[cond], collapse = ", "), "."))
    }
  }
  cond <- !purrr::map_lgl(l, function(x) stringr::str_detect(units::deparse_unit(x), pattern = "mol"))
  if(any(cond)) {
    if(sum(cond) == 1) {
      rlang::abort(paste0("All, c, h, n, o, must have unit 'mol' or any derivative of it. ", names(l)[cond], " does not have unit 'mol'."))
    } else {
      rlang::abort(paste0("All, c, h, n, o, must have unit 'mol' or any derivative of it. ", paste(names(l)[cond], collapse = ", "), " do not have unit 'mol'."))
    }

  }

  TRUE
}
