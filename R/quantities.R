#### quantities ####

#' Handling quantities in `elco` objects
#'
#' @name elco-quantities
#'
#' @inheritParams quantities::set_quantities
#'
#' @examples
#' ## quantities
#'
#' # quantities.elco
#' quantities(elco::chno$C)
#'
#' @export
quantities.elco <- function(x) {
  NextMethod()
}

#' @rdname elco-quantities
#'
#' @examples
#' # `quantities.elco<-`
#' x <- elco::chno$C
#' quantities(x) <- list("g/g", 0.1)
#'
#' @export
"quantities<-.elco" <- function(x, value) {
  .reclass(NextMethod(), el_symbol = attr(x, "el_symbol"))
}


#' @rdname elco-quantities
#'
#' @examples
#' # set_quantities
#' set_quantities(elco::chno$C, unit = "g/g", errors = 0.1, mode = "standard")
#'
#' @export
set_quantities.elco <- function(x, unit, errors = 0, ..., mode = units_options("set_units_mode")) {
  .reclass(NextMethod(), el_symbol = attr(x, "el_symbol"))
}

#### errors ####

#' @rdname elco-quantities
#'
#' @examples
#' ## errors
#'
#' # `errors.elco`
#' errors::errors(elco::chno$C)
#'
#' @export
errors.elco <- function(x) {
  NextMethod()
}

#' @rdname elco-quantities
#'
#' @examples
#' # `errors.elco<-`
#' x <- elco::chno$C
#' errors::errors(x) <- 0.1
#'
#' @export
"errors<-.elco" <- function(x, value) {
  .reclass(NextMethod(), el_symbol = attr(x, "el_symbol"))
}


#' @rdname elco-quantities
#'
#' @examples
#' # set_errors
#' set_errors(elco::chno$C, value = 0.1)
#'
#' @export
set_errors.elco <- function(x, value = 0) {
  .reclass(NextMethod(), el_symbol = attr(x, "el_symbol"))
}


#' @rdname elco-quantities
#'
#' @examples
#' # drop_errors.elco
#' errors::drop_errors(elco::chno$C)
#'
#'@export
drop_errors.elco <- function(x) {
   .reclass(NextMethod(), el_symbol = attr(x, "el_symbol"))
}


#### units ####

#' @rdname elco-quantities
#'
#' @examples
#' ## units
#'
#' # units.elco
#' units(elco::chno$C)
#'
#' @export
units.elco <- function(x) {
  NextMethod()
}


#' @rdname elco-quantities
#'
#' @examples
#' # `units.elco<-`
#' x <- elco::chno$C
#' units(x) <- "g/g"
#'
#' @export
"units<-.elco" <- function(x, value) {
  .reclass(NextMethod(), el_symbol = attr(x, "el_symbol"))
}


#' @rdname elco-quantities
#'
#' @examples
#' # set_units
#' set_units(elco::chno$C, value = "g/g", mode = "standard")
#'
#' @export
set_units.elco <- function(x, value, ..., mode = units_options("set_units_mode")) {
  .reclass(NextMethod(), el_symbol = attr(x, "el_symbol"))
}
