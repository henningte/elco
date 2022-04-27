#' Converts the unit of element contents.
#'
#' `elco_el_transform` takes a `data.frame`, that contains chemical element contents, and
#' specified chemical elements as input, and converts their unit of measurement.
#'
#' @param x A `data.frame` that contains chemical element contents. Columns to be transformed
#' have to be named as symbols of the respective chemical elements. Element contents must be of class
#' [units::units()] and are allowed to have as units mass ratios (e.g. `"g/g"`,
#' `"mg/mg"`) or molar contents (e.g. `"mol/g"`).
#' @param ... Chemical elements for which to transform the element content. This can be character
#' values or symbols that represent the symbols of the chemical elements (e.g. `C`).
#' @param to A character value representing the unit of measurement to which the element contents
#' should be transformed. This should be one of `c("g/g", "mol/g")`.
#' @param col_sample_mass A character value or an integer value indicating the column in `x` that
#' contains the mass of the sample. `col_sample_mass` can be set to `NULL`, if the unit
#' transformation does not involve multiplication with the sample mass. Otherwise, the values in this
#' column are used to transform the units (for example to compute the amount of C in a sample \[mol\] based
#' on its mass fraction [g/g] and the sample mass \[g\]).
#' @return `x` with converted element contents.
#' @export
elco_el_transform <- function(x, ..., to, col_sample_mass = NULL) {

  # checks
  if(length(to) != 1) {
    rlang::abort(paste0("`to` must be a value, but is of length ", length(to), "."))
  }
  if(!is.character(to)) {
    rlang::abort("`to` must be a character value, but is of class ", class(to)[[1]], ".")
  }
  if(!to %in% elco::el_units_allowed$unit_symbol) {
    rlang::abort(paste0("`to` must be one of `elco::el_units_allowed$unit_symbol`, but is ", to, "."))
  }
  if(!(is.null(col_sample_mass) || is.character(col_sample_mass)) || is.integer(col_sample_mass)) {
    rlang::abort("`col_sample_mass` must be `NULL` or a character value or integer value.")
  }
  if(!is.null(col_sample_mass)) {
    if(length(col_sample_mass) != 1) {
      rlang::abort(paste0("`col_sample_mass` must be a value, but is of length ", length(col_sample_mass), "."))
    }
    sample_mass <- dplyr::select(x, dplyr::all_of(col_sample_mass))
    sample_mass_unit <- as.character(units(sample_mass[, 1, drop = TRUE]))
  }

  x_els <- elco_get_el(x, ...)


}

#' @export
elco_get_el <- function(x, ...) {

  els <- as.character(rlang::enexprs(...))

  # checks
  if(!is.data.frame(x)) {
    rlang::abort(paste0("`x` must be a data.frame, but is of class ", class(x)[[x]], "."))
  }
  cond <- !PeriodicTable::areSymb(els)
  if(any(cond)) {
    if(sum(cond) == 1) {
      rlang::abort(paste0("All arguments passed in ... must be symbols for chemical elements, but argument ", which(cond), " in ... is ", els[cond], "."))
    } else {
      rlang::abort(paste0("All arguments passed in ... must be symbols for chemical elements, but arguments ", paste(which(cond), collapse = ", "), " in ... are ", paste(els[cond], collapse = ", "), "."))
    }
  }
  x_vars <- colnames(x)
  cond <- !purrr::map_lgl(els, function(y) y %in% x_vars)
  if(any(cond)) {
    if(sum(cond) == 1) {
      rlang::abort(paste0("All arguments passed in ... must correspond to a column in `x`, but argument ", which(cond), " in ... has no matching column in `x`."))
    } else {
      rlang::abort(paste0("All arguments passed in ... must correspond to a column in `x`, but arguments ", paste(which(cond), collapse = ", "), " in ... have no matching column in `x`."))
    }
  }
  x_els <- dplyr::select(x, dplyr::all_of(els))
  cond <- !purrr::map_lgl(x_els, function(y) inherits(y, "units"))
  if(any(cond)) {
    if(sum(cond) == 1) {
      rlang::abort(paste0("All columns with selected element contents in `x` must be of class units, but the column for element ", els[cond], " is not of class units."))
    } else {
      rlang::abort(paste0("All columns with selected element contents in `x` must be of class units, but the columns for elements ", paste(els[cond], collapse = ", "), " are not of class units."))
    }
  }
  x_els_units <- purrr::map_chr(x_els, function(y) {
    as.character(units(y))
  })
  # el_units_allowed <- data("el_units_allowed", package = "elco")
  cond <- !purrr::map_lgl(x_els_units, function(y) y %in% elco::el_units_allowed$unit_symbol)
  if(any(cond)) {
    if(sum(cond) == 1) {
      rlang::abort(paste0("All columns with selected element contents in `x` must have units as defined in `elco::el_units_allowed$unit_symbol`, but the column for element ", els[cond], " has units ", x_els_units[cond], "."))
    } else {
      rlang::abort(paste0("All columns with selected element contents in `x` must have units as defined in `elco::el_units_allowed$unit_symbol`, but the columns for elements ", paste(els[cond], collapse = ", "), " have units ", paste(x_els_units[cond], collapse = ", "), ", respectively."))
    }
  }
  x_els

}

