#### vctrs methods for elco objects ####

#' 'vctrs' methods for `elco` objects
#'
#' @name elco-vctrs
#'
#' @source https://github.com/r-quantities/quantities/blob/master/R/tidyverse.R
#'
#' @keywords internal
#' @noRd
NULL

#' @rdname elco-vctrs
#'
#' @examples
#' # vec_restore.elco
#' x <- elco::chno$C
#' vctrs::vec_restore(vctrs::vec_data(x), to = x)
#'
vec_restore.elco <- function(x, to, ...) {
  vec_restore.quantities <- getS3method("vec_restore", "quantities", envir = asNamespace("vctrs"))

  out <- vec_restore.quantities(x, elco_drop_elco(to))
  elco_new_elco(out, el_symbol = elco_el_symbol(to))
}


#' @rdname elco-vctrs
#'
#' @examples
#' # vec_proxy.elco
#' vctrs::vec_proxy(elco::chno$C)
#'
vec_proxy.elco <- function(x, ...) {
  vec_proxy.quantities <- getS3method("vec_proxy", "quantities", envir = asNamespace("vctrs"))
  vec_proxy.quantities(elco_drop_elco(x))
}

#' @rdname elco-vctrs
#'
#' @examples
#' # vec_ptype2.elco.elco
#' vctrs::vec_ptype2(x = elco::chno$C, y = elco::chno$C)
#'
vec_ptype2.elco.elco <- function(x, y, ...) {
  x_units <- errors::drop_errors(x)
  y_units <- errors::drop_errors(y)
  common <- vctrs::vec_ptype2(x_units, y_units, ...)

  res_el_symbols <- unique(c(elco_el_symbol(x), elco_el_symbol(y)))

  elco_new_elco(quantities::set_quantities(common, unit = units(common), mode = "standard"), el_symbol = res_el_symbols)
}

#' @rdname elco-vctrs
#'
#' @examples
#' # vec_cast.elco.elco
#' vctrs::vec_cast(x = elco::chno$C, to = elco::chno$C)
#' vctrs::vec_cast(x = elco::chno$C, to = elco::chno$N)
#'
vec_cast.elco.elco <- function(x, to, ...) {
  to_units <- units(to)
  to_el_symbol = elco_el_symbol(to)

  # First set units and errors. Must happen first in case this causes
  # `x` to become fractional (which should cause an error if `to` is
  # integer).
  out <- units::set_units(x, to_units, mode = "standard")
  out_errors <- errors::errors(out)

  # Now cast base type
  out_bare <- quantities::drop_quantities(elco_drop_elco(out))
  to_bare <- quantities::drop_quantities(elco_drop_elco(to))
  out <- vctrs::vec_cast(out_bare, to_bare, ...)

  # Set quantities back
  out <- quantities::set_quantities(out, to_units, out_errors, mode = "standard")

  # Set elco back
  elco_new_elco(out, el_symbol = to_el_symbol)

}




#nocov start
# source: https://github.com/r-quantities/quantities/blob/master/R/tidyverse.R
register_all_s3_methods <- function() {

  register_s3_method("vctrs::vec_proxy", "elco")
  register_s3_method("vctrs::vec_restore", "elco")
  register_s3_method("vctrs::vec_ptype2", "elco.elco")
  register_s3_method("vctrs::vec_cast", "elco.elco")
}

register_s3_method <- function(generic, class, fun=NULL) {
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)

  pieces <- strsplit(generic, "::")[[1]]
  stopifnot(length(pieces) == 2)
  package <- pieces[[1]]
  generic <- pieces[[2]]

  if (is.null(fun))
    fun <- get(paste0(generic, ".", class), envir=parent.frame())
  stopifnot(is.function(fun))

  if (package %in% loadedNamespaces())
    registerS3method(generic, class, fun, envir=asNamespace(package))

  # Always register hook in case package is later unloaded & reloaded
  setHook(packageEvent(package, "onLoad"), function(...)
    registerS3method(generic, class, fun, envir=asNamespace(package)))
}
# nocov end
