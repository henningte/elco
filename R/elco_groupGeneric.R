#' S3 Group Generic Functions for objects of class `elco`
#'
#' `Math`, `Ops` and `Summary` group generic methods for `elco` objects (see
#' [base::groupGeneric()] for a comprehensive list of available methods).
#'
#' @name elco-groupGeneric
#'
#' @inheritParams base::groupGeneric
#'
#' @param x An object of class `elco`.
#'
#' @examples
#' # Math
#' abs(elco::chno$C)
#'
#' @export
Math.elco <- function(x, ...) {
  .reclass(NextMethod(), el_symbol = attr(x, "el_symbol"))
}

#' @rdname elco-groupGeneric
#'
#' @param e1 An object of class `elco`.
#'
#' @param e2 An object of class `elco` which is
#' added to/subtracted from/multiplied with/used for division of `e1`.
#'
#' @examples
#' x <- elco::chno
#'
#' x$C + x$C
#'
#' # drops elco class because these are two different element symbols:
#' x$C + x$N
#'
#' # drops elco class because the operation is not meaningful for chemical
#' # elements:
#' x$C * x$C
#'
#' @export
Ops.elco <- function(e1, e2) {
  .reclass(NextMethod(), el_symbol = unique(purrr::map_chr(list(e1, e2), elco_el_symbol)))
}
