#' S3 Group Generic Functions
#'
#' `Math`, `Ops` and `Summary` group generic methods for
#' `elco` objects (see [base::groupGeneric()] for a
#' comprehensive list of available methods).
#'
#' @inheritParams base::groupGeneric
#' @name groupGeneric.elco
#'
#' @export
Math.elco <- function(x, ...) {
  elco_new_elco(NextMethod(), el_symbol = attr(x, "el_symbol"))
}
