#' Creates an object of class `elco`
#'
#' `elco_new_elco` is the constructor function for objects of class
#' `elco`. An object of class `elco` is a numeric vector with each element
#' representing a chemical element content value. At the same time, the numeric vector
#' is an object of class [quantities::quantities()] with an
#' additional attribute `"el_symbol"` with the respective element symbol.
#'
#' @param x A numeric vector that is an object of class
#' [quantities::quantities()] with each element representing an
#' element content.
#' @param el_symbol A character value representing the symbol for a chemical element.
#' @return An object of class `elco`. This is identical to `x`, but has
#' an additional attribute `"el_symbol"` and class attribute.
#' @export
elco_new_elco <- function(x,
                          el_symbol) {

  # checks
  if(!inherits(x, "quantities")) {
    rlang::abort(paste0("`x` must be of class quantities, but is of class ", class(x)[[1]], "."))
  }
  x_units <- units(x)
  x_units_numerator <- x_units$numerator
  if(length(x_units_numerator) == 0) {
    rlang::abort("The numerator of the unit of `x` is empty, but must have a value")
  }

  x_units_numerator <- units::as_units(x_units_numerator)
  cond <- !(elco_check_unit_conversion(x_units_numerator, to = "g", mode = "standard") | elco_check_unit_conversion(x_units_numerator, to = "mol", mode = "standard"))
  if(cond) {
    rlang::abort(paste0("The units attribute of `x` must either identify a mass or a molar amount, but is ", as.character(x_units), "."))
  }

  x_units_denominator <- x_units$denominator
  if(length(x_units_denominator) == 1) {
    x_units_denominator <- units::as_units(x_units_denominator)
    cond <- !(elco_check_unit_conversion(x_units_denominator, to = "g", mode = "standard") | elco_check_unit_conversion(x_units_denominator, to = "mol", mode = "standard"))
    if(cond) {
      rlang::abort(paste0("The units attribute of `x` must either identify a mass or a molar amount, but is ", as.character(x_units), "."))
    }
  }

  if(length(el_symbol) != 1) {
    rlang::abort("`el_symbol` must be a character value, but is of length ", length(el_symbol), ".")
  }
  if(!is.character(el_symbol)) {
    rlang::abort(paste0("`el_symbol` must be a character value, but is of class ", class(el_symbol)[[1]], "."))
  }

  cond <- !PeriodicTable::isSymb(el_symbol)
  if(cond) {
    rlang::abort(paste0("`el_symbol` must be a symbol for a chemical element, but is ", el_symbol, "."))
  }

  structure(x,
            class = c("elco", class(x)),
            units = attr(x, "units"),
            errors = attr(x, "errors"),
            el_symbol = el_symbol)

}

#' Prints an object of class `elco`.
#'
#' @param x An object of class [`elco()`][elco::elco_new_elco].
#' @param ... Additional arguments, will be ignored.
#' @return `x`.
#' @export
print.elco <- function(x, ...) {
  cat(paste0("Element: ", attr(x, "el_symbol"), "\n"))
  x <- drop_elco(x)
  NextMethod()
}

#' Checks if a unit conversion is valid.
#'
#' `elco_check_unit_conversion` takes an object of class [units::units()]
#' and a character value representing a unit it should be converted into and checks if the
#' conversion is valid.
#'
#' @param x An object of class [units::units()].
#' @param to A character value representing the symbol for a unit into which
#' `x` should be converted.
#' @param ... Additional arguments passed to [units::set_units()].
#' @return A logical value indicating if the conversion is valid (`TRUE`)
#' or not (`FALSE`).
elco_check_unit_conversion <- function(x, to, silent = TRUE, ...) {
  tryCatch({
    units::set_units(x, to, ...)
    TRUE
  },
  error = function(cnd) FALSE,
  silent = silent)
}

#' Checks if an object is of class `elco`.
#'
#' `elco_check_elco` checks if an object is of class [`elco()`][elco::elco_new_elco].
#'
#' @param x An object.
#' @return An object of class `elco`.
#' @keywords internal
elco_check_elco <- function(x) {
  inherits(x, "elco")
}

#' Combine Values into a Vector or List
#'
#' S3 method for `quantities` objects (see [c()]).
#'
#' @inheritParams base::c
#'
#' @examples
#' c(set_quantities(1, m/s, 0.2), set_quantities(30, km/h, 0.1))
#'
#' @export
c.elco <- function(...){
  x <- list(...)
  x_el_symbols <- unique(purrr::map_chr(x, attr, "el_symb"))
  stopifnot(length(x_el_symbols) == 1)
  structure(NextMethod(), class = class(x[[1]]), el_symbol = x_el_symbols)
}

#'
#' @export
set_quantities.elco <- function(x, unit, errors = 0, ..., mode = units_options("set_units_mode")) {
  x_el_symbol <- attr(x, "el_symbol")
  x <- as.numeric(x)
  elco_new_elco(quantities::set_quantities(x, unit = unit, errors = errors, ..., mode = mode), el_symbol = x_el_symbol)
}

#'
#' @export
set_errors.elco <- function(x, value = 0) {
  x_or <- x
  class(x) <- setdiff(class(x), "vctrs_vctr")
  elco_reclass_elco(NextMethod())
}

#'
#'@export
drop_elco <- function(x) {
  attr(x, "el_symbol") <- NULL
  class(x) <- setdiff(class(x), "elco")
  x
}

#'
#'@export
drop_errors.elco <- function(x) {
  class(x) <- setdiff(class(x), "elco")
  NextMethod()
}

#'
#' @export
errors.elco <- function(x) {
  x <- drop_elco(x)
  NextMethod()
}

#'
#' @export
`errors<-.elco` <- function(x, value) structure(NextMethod(), class = class(x))

#'
#' @export
`[.elco` <- function(x, ...) {
  elco::elco_new_elco(NextMethod(), el_symbol = attr(x, "el_symbol"))
}

#'
#' @export
vec_restore.elco <- function(x, to, ...) {
  print(class(x))
  x_el_symbol <- attr(x, "el_symbol")

  x <- drop_elco(x)
  print(class(x))

  elco_new_elco(NextMethod(), el_symbol = x_el_symbol)
}

#'
#' @export
cbind.elco  <- function(..., deparse.level = 1) {
  dots <- list(...)
  stopifnot(all(sapply(dots, inherits, "units")))
  u <- units(dots[[1]])
  dots <- getS3method("set_units", "mixed_units")(dots, as.character(u))

  nm <- names(as.list(match.call()))
  nm <- nm[nm != "" & nm != "deparse.level"]
  if (is.null(nm))
    names(dots) <- sapply(substitute(list(...))[-1], deparse)
  else names(dots) <- nm

  call <- as.character(match.call()[[1]])
  assign(call, getS3method(call, "errors"))
  value <- do.call(call, c(dots, deparse.level=deparse.level))
  attr(value, "units") <- u
  structure(value, class = c("elco", "quantities", "units", "errors"))
}

#'
#' @export
rbind.elco <- cbind.elco



# tidyverse

#' @export
#' @source Modified from <https://github.com/r-quantities/quantities/blob/master/R/tidyverse.R>.
vec_ptype2.elco.elco <- function(x, y, ..., x_arg = "", y_arg = "") {
  x_or <- x
  y_or <- y
  if (!identical(attr(x_or, "el_symbol"), attr(y_or, "el_symbol"))) {
    vctrs::stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
  }

  x_quantities <- drop_elco(x)
  y_quantities <- drop_elco(y)
  common <- vctrs::vec_ptype2(x_quantities, y_quantities, ...)

  elco_new_elco(common, el_symbol = attr(x_or, "el_symbol"))
}

#' @export
vec_proxy.elco <- function(x, ...) {
  vec_proxy.quantities <- getS3method("vec_proxy", "quantities", envir = asNamespace("vctrs"))
  vec_proxy.quantities(drop_elco(x))
}

#' @export
vec_restore.elco <- function(x, to, ...) {
  vec_restore.quantities <- getS3method("vec_restore", "quantities", envir = asNamespace("vctrs"))

  out <- vec_restore.quantities(x, drop_elco(to))
  elco_new_elco(out, attr(to, "el_symbol"))
}

#' @export
vec_cast.elco.elco <- function(x, to, ...) {
  to_units <- units(to)

  # First set units and errors. Must happen first in case this causes
  # `x` to become fractional (which should cause an error if `to` is
  # integer).
  out <- units::set_units(x, to_units, mode = "standard")
  out_errors <- errors(out)

  # Now cast base type
  out_bare <- quantities::drop_quantities(out)
  to_bare <- quantities::drop_quantities(to)
  out <- vctrs::vec_cast(out_bare, to_bare, ...)

  # Set quantities back
  out <- set_quantities(out, to_units, out_errors, mode = "standard")
  elco_new_elco(out, attr(to, "el_symbol"))
}

#' @export
mean.elco <- function(x, trim = 0, na.rm = FALSE, ...) {
  elco_new_elco(NextMethod(), el_symbol = attr(x, "el_symbol"))
}

#' Identifies columns of class `elco` in a `data.frame`.
#'
#' `elco_identify_elco_df` identifies columns in a `data.frame` that
#' are of class [`elco`][elco_new_elco].
#'
#' @param x A `data.frame`.
#' @return A logical vector with an element for each column in `x` indicating if
#' the column is of class `elco` or not.
#' @export
elco_identify_elco_df <- function(x) {

  # checks
  if(!is.data.frame(x)) {
    rlang::abort(paste0("`x` must e a data.frame, but is of class ",  class(x)[[1]], "."))
  }
  f <- elco_check_elco
  purrr::map_lgl(x, f)

}
