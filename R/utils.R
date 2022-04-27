#' Restores the class attribute of an object with the attributes of an object of class `elco`
#'
#' `elco_reclass_elco` restores the class attribute of an object with the attributes
#' (`units`, `errors`, `el_symbol`) of an object of class `elco`.
#'
#' @param x A numeric vector with the attributes of an object of class
#' [`elco()`][elco::elco_new_elco].
#' @return An object of class `elco`.
#' @source The function is modified from `quantities:::reclass`
#' (<https://github.com/r-quantities/quantities/blob/master/R/utils.R>).
#' @export
elco_reclass_elco <- function(x) {
  if (!is.null(attr(x, "units")) && !is.null(attr(x, "errors")) && !is.null(attr(x, "el_symbol")))
    class(x) <- c("elco", "quantities", "units", "errors")
  x
}
