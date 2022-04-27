#### Manipulating chemical element symbol information ####

#' Getting or setting chemical element information
#'
#' @param x An object.
#'
#' @param value A character value representing a chemical element symbol with
#' which to set or replace an existing attribute `"el_symbol"` in `x`.
#'
#' @return `x` converted to an [`elco`][elco::elco_new_elco] object.
#'
#' @export
elco_el_symbol <- function(x) UseMethod("elco_el_symbol")

#' @rdname elco_el_symbol
#'
#' @examples
#' # getting element information from an `elco` object
#' elco_el_symbol(elco::chno$C)
#'
#' @export
elco_el_symbol.elco <- function(x) {
  attr(x, "el_symbol")
}

#' @rdname elco_el_symbol
#'
#' @param value
#'
#' @return `x` with attribute value for `el_symbol` replaced by `value`.
#'
#' @export
`elco_el_symbol<-` <- function(x, value) UseMethod("elco_el_symbol<-")

#' @rdname elco_el_symbol
#'
#' @examples
#' ## setting element information in an `elco` object
#' x <- elco::chno$C
#' elco_el_symbol(x) <- "C"
#'
#' # pipe-friendly
#' elco::chno$C %>% elco_set_el_symbol("C")
#'
#' @export
`elco_el_symbol<-.elco` <- function(x, value) {
  stopifnot(length(value) == 1L && PeriodicTable::isSymb(value))
  attr(x, "el_symbol") <- value
  x
}

#' @rdname elco_el_symbol
#'
#' @examples
#' ## setting element information in a `quantities` object
#' x <- elco_drop_elco(elco::chno$C)
#'
#' elco_el_symbol(x) <- "C"
#'
#' # pipe-friendly
#' x <-
#'   elco_drop_elco(elco::chno$C) %>%
#'   elco_set_el_symbol("C")
#'
#' @export
`elco_el_symbol<-.quantities` <- function(x, value) {
  elco_new_elco(x, el_symbol = value)
}

#' @rdname elco_el_symbol
#'
#' @export
elco_set_el_symbol <- function(x, value) UseMethod("elco_set_el_symbol")

#' @rdname elco_el_symbol
#'
#' @export
elco_set_el_symbol.elco <- function(x, value) {
  elco_el_symbol(x) <- value
  x
}

#' @rdname elco_el_symbol
#'
#' @export
elco_set_el_symbol.quantities <- function(x, value) {
  elco_el_symbol(x) <- value
  x
}
