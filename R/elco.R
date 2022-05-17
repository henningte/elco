#' Creates an object of class `elco`
#'
#' `elco_new_elco` is the constructor function for objects of class `elco`. An
#' object of class `elco` is a numeric vector with each element representing a
#' chemical element content value. It is a subclass of the
#' [quantities::quantities()] class with an additional attribute `"el_symbol"`
#' where the respective element symbol is stored.
#'
#' @param x A numeric vector that is an object of class
#' [`quantities`][quantities::quantities()] with each element representing an
#' element content.
#'
#' @param el_symbol A character value representing the symbol for a chemical
#' element. To check this, [`PeriodicTable::isSymb()`] is used.
#'
#' @return An object of class `elco`. This is identical to `x`, but has
#' an additional attribute `"el_symbol"` and an additional class attribute
#' `"elco"`.
#'
#' @examples
#' library(quantities)
#'
#' # store C content value as elco object
#' elco_new_elco(
#'   x = quantities::set_quantities(0.42, unit = "mol/g", errors = 0.002),
#'   el_symbol = "C"
#' )
#'
#' @export
elco_new_elco <- function(x, el_symbol) {

  ## checks
  stopifnot(inherits(x, "quantities"))

  # units
  x_units <- units(x)
  x_units_numerator <- x_units$numerator
  x_units_denominator <- x_units$denominator
  if(length(x_units_numerator) == 0) {
    rlang::abort("The numerator of the unit of `x` is empty, but must have a value")
  }

  x_units_numerator <- units::as_units(x_units_numerator)
  cond_numerator <- !(elco_check_unit_conversion(x_units_numerator, to = "g", mode = "standard") | elco_check_unit_conversion(x_units_numerator, to = "mol", mode = "standard"))
  cond_denominator <- FALSE
  if(length(x_units_denominator) != 0) {
    x_units_denominator <- units::as_units(x_units_denominator)
    cond_denominator <- !(elco_check_unit_conversion(x_units_denominator, to = "g", mode = "standard") | elco_check_unit_conversion(x_units_denominator, to = "mol", mode = "standard"))
  }

  if(cond_numerator || cond_denominator) {
    rlang::abort(paste0("The units attribute of `x` must either identify a mass or a molar amount, but is ", as.character(x_units), "."))
  }

  # element symbol
  if(length(el_symbol) != 1 || !is.character(el_symbol)) {
    rlang::abort("`el_symbol` must be a character value.")
  }
  if(!PeriodicTable::isSymb(el_symbol)) {
    rlang::abort(paste0("`el_symbol` must be a symbol for a chemical element, but is ", el_symbol, "."))
  }

  structure(
    x,
    class = c("elco", class(x)),
    el_symbol = el_symbol
  )

}


#' @rdname elco_new_elco
#'
#'@export
elco_drop_elco <- function(x) {
  attr(x, "el_symbol") <- NULL
  class(x) <- setdiff(class(x), "elco")
  x
}



#### Printing ####


#' @rdname elco_new_elco
#'
#' @param x An object of class [`elco`][elco::elco_new_elco].
#'
#' @param ... Additional arguments, will be ignored.
#'
#' @return Returns the input object `x`. Prints its content as side effect.
#'
#' @examples
#' print(elco::chno$C)
#'
#' @export
print.elco <- function(x, ...) {
  cat(paste0("Element: ", attr(x, "el_symbol"), "\n"))
  x <- elco_drop_elco(x)
  NextMethod()
}



#### Combining ####


#' Combine `elco` objects into a vector or list
#'
#' S3 method for `elco` objects (see [`c()`]).
#'
#' @inheritParams base::c
#'
#' @return `...` concatenated into one vector.
#'
#' @examples
#' c(elco::chno$C[[1]], elco::chno$C[-1])
#' c(elco::chno$C[[1]], elco::chno$N[-1])
#' c(elco::chno$C[[1]], units::set_units(NA_real_, "g/g"))
#'
#' @export
c.elco <- function(...){
  dots <- list(...)
  dots_el_symbols <- unique(purrr::map_chr(dots, function(.x) {
    res <- attr(.x, "el_symbol")
    if(is.null(res)) {
      NA_character_
    } else {
      res
    }
  }))
  if(length(stats::na.omit(dots_el_symbols)) != 1) {
    message("`...` contains `elco` objects with different chemical elements. Dropping `elco` class.")
  }
  if(any(is.na(dots_el_symbols))) {
    message("`...` contains non-`elco` object. Dropping `elco` class.")
    dots_el_symbols <- NULL
  }
  .reclass(do.call("c", lapply(dots, elco_drop_elco)), el_symbol = dots_el_symbols)
}



#### Subsetting ####


#' Extract or replace parts of an `elco` object
#'
#' @name elco-extract
#'
#' @param x An `elco` object to subset values from or where to replace values.
#'
#' @param i,j,... indices specifying elements to extract or replace. See
#' [`base::Extract`].
#'
#' @param value An object of class `elco` which replaces elements in `x`.
#'
#' @return An object of class `elco` with subsetted or replaced values.
#'
NULL

#' @rdname elco-extract
#'
#' @examples
#' elco::chno$C[1]
#'
#' @export
`[.elco` <- function(x, i) {
  elco::elco_new_elco(NextMethod(), el_symbol = attr(x, "el_symbol"))
}


#' @rdname elco-extract
#'
#' @examples
#' elco::chno$C[[1]]
#'
#' @export
`[[.elco` <- function(x, i) {
  elco::elco_new_elco(NextMethod(), el_symbol = attr(x, "el_symbol"))
}


#' @rdname elco-extract
#'
#' @examples
#' x <- elco::chno$C
#' y <- elco::chno$N
#' z <- NA_real_
#'
#' # returns `elco` object because both have the same el_symbol attribute
#' x[1] <- x[2]
#' class(x)
#'
#' # strips off the elco class because the el_symbol attribute differs
#' y[1] <- x[2]
#' class(y)
#'
#' # strips off the elco class because z is no elco object
#' x[1] <- z
#' class(x)
#'
#' @export
"[<-.elco" <- function(x, i, j, ..., value) {
  dots <- list(x, value)
  dots_el_symbols <- unique(purrr::map_chr(dots, function(.x) {
    res <- attr(.x, "el_symbol")
    if(is.null(res)) {
      NA_character_
    } else {
      res
    }
  }))
  if(length(stats::na.omit(dots_el_symbols)) != 1) {
    message("`value` is an `elco` object with different chemical element than `x`. Dropping `elco` class.")
  }
  if(is.null(attr(value, "el_symbol"))) {
   message("`value` is no `elco` object. Dropping `elco` class.")
   dots_el_symbols <- NULL
  }
  .reclass(NextMethod(), el_symbol = dots_el_symbols)
}

#' @rdname elco-extract
#'
#' @examples
#' x <- elco::chno$C
#' y <- elco::chno$N
#'
#' # returns `elco` object because both have the same el_symbol attribute
#' x[[1]] <- x[2]
#' class(x)
#'
#' # strips off the elco class because the el_symbol attribute differs
#' y[[1]] <- x[2]
#' class(y)
#'
#' @export
"[[<-.elco" <- `[<-.elco`



#### Casting ####

#' Cast an object to an `elco` object
#'
#' @name elco-cast
#'
#' @param x An object to be converted to class [`elco`][elco::elco_new_elco] or
#' and `elco` object to be converted to a different class.
#'
#' @param ... Additional arguments passed on to methods.
#'
#' @param el_symbol A character vector representing a chemical element symbol to
#' set for the `elco` object `x` gets converted to.
#'
#' @return
#' - `elco_as_elco` returns `x` converted to an object of class `elco`.
#' - `as.data.frame.elco` returns `x` converted to a data frame.
#'
#' @export
elco_as_elco <- function(x, ...) UseMethod("elco_as_elco")

#' @rdname elco-cast
#'
#' @examples
#' # elco_as_elco.quantities
#' elco::chno$C %>%
#'   elco_drop_elco() %>%
#'   elco_as_elco(el_symbol = "C")
#'
#' @export
elco_as_elco.quantities <- function(x, el_symbol, ...) {
  elco_new_elco(x, el_symbol = el_symbol)
}

#' @rdname elco-cast
#'
#' @examples
#' # elco_as_elco.elco
#' elco::chno$C %>%
#'   elco_as_elco()
#'
#' @export
elco_as_elco.elco <- function(x, ...) {
  x
}


#' @rdname elco-cast
#'
#' @inheritParams base::as.data.frame
#'
#' @examples
#' # elco_as_elco.elco
#' elco::chno$C %>%
#'   as.data.frame() %>%
#'   setNames("C")
#'
#' @export
as.data.frame.elco <- function(x, row.names = NULL, optional = FALSE, ...) {
  res <- NextMethod()
  res[[1]] <- elco_new_elco(res[[1]], el_symbol = attr(x, "el_symbol"))
  res
}

####









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


#### Helpers ####

#' Checks if a unit conversion is valid
#'
#' `elco_check_unit_conversion` takes a `units` object and a
#' character value representing a unit it should be converted into and checks if
#' the conversion is valid.
#'
#' @param x An object of class [`units`][units::units].
#'
#' @param to A character value representing the symbol for a unit into which `x`
#' should be converted.
#'
#' @param ... Additional arguments passed to [units::set_units()].
#'
#' @return A logical value indicating if the conversion is valid (`TRUE`)
#' or not (`FALSE`).
#'
#' @keywords internal
#' @noRd
#'
elco_check_unit_conversion <- function(x, to, silent = TRUE, ...) {
  tryCatch({
    units::set_units(x, to, ...)
    TRUE
  },
  error = function(cnd) FALSE,
  silent = silent)
}

#' Checks if an object is of class `elco`
#'
#' `elco_check_elco` checks if an object is of class [`elco`][elco::elco_new_elco].
#'
#' @param x An object.
#'
#' @return An object of class `elco`.
#'
#' @keywords internal
#' @noRd
#'
elco_check_elco <- function(x) {
  inherits(x, "elco")
}

#' Helper functions to reclass objects to `elco`
#'
#' Internal helper function to reclass objects of class `elco` modified by a
#' function. Check whether the modified objects meets the requirements to be of
#' class `elco` and if not, drops the `elco` class.
#'
#' @keywords internal
#' @noRd
#'
.reclass <- function(x, el_symbol) {

  x_units <- as.character(units(x))

  if(length(el_symbol) != 1L || !inherits(x, "quantities") || ! x_units %in% elco::el_units_allowed$unit_symbol) { # more than one element OR x is no quantities object any more
    attr(x, "el_symbol") <- NULL
    class(x) <- setdiff(class(x), "elco")
    res <- x
  } else {
    res <- elco_new_elco(x, el_symbol = el_symbol)
  }

  res

}
