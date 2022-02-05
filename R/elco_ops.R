#' @rdname groupGeneric.elco
#'
#' @export
Ops.elco <- function(e1, e2) {

  element_e1 <- attr(e1, "el_symbol")
  element_e2 <- attr(e2, "el_symbol")

  e1 <- drop_elco(e1)
  e2 <- drop_elco(e2)
  NextMethod()
}
