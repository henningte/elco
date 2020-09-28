#' Restores the class attribute of an object with the attributes of an object of class \code{elco}
#'
#' \code{elco_reclass_elco} restores the class attribute of an object with the attributes
#' (\code{units}, \code{errors}, \code{el_symbol}) of an object of class \code{elco}.
#'
#' @param x A numeric vector with the attributes of an object of class
#' \code{\link[elco:elco_new_elco]{elco}}.
#' @return An object of class \code{elco}.
#' @source The function is modified from \code{quantities:::reclass}
#' (\url{https://github.com/r-quantities/quantities/blob/master/R/utils.R}).
#' @export
elco_reclass_elco <- function(x) {
  if (!is.null(attr(x, "units")) && !is.null(attr(x, "errors")) && !is.null(attr(x, "el_symbol")))
    class(x) <- c("elco", "quantities", "units", "errors")
  x
}
