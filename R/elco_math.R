#' S3 Group Generic Functions
#'
#' \code{Math}, \code{Ops} and \code{Summary} group generic methods for
#' \code{elco} objects (see \code{\link[base]{groupGeneric}} for a
#' comprehensive list of available methods).
#'
#' @inheritParams base::groupGeneric
#' @name groupGeneric.elco
#'
#' @export
Math.elco <- function(x, ...) {
  elco_new_elco(NextMethod(), el_symbol = attr(x, "el_symbol"))
}
