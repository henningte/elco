#' @rdname groupGeneric.elco
#'
#' @export
Ops.elco <- function(e1, e2) {

  element_e1 <- attr(e1, "el_symbol")
  element_e2 <- attr(e2, "el_symbol")

  if(identical(element_e1, element_e2)) {
    elco_new_elco(NextMethod(), el_symbol = element_e1)
  } else {
    e1 <- drop_elco(e1)
    e2 <- drop_elco(e2)
    NextMethod()
  }
}
