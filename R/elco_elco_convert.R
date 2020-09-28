#' Converts the unit of objects of class \code{elco}.
#'
#' \code{elco_elco_convert} takes an object of class \code{elco} and converts it measurement unit to
#' a specified unit.
#'
#' @param x An object of class \code{\link[elco:elco_new_elco]{elco}}.
#' @param to A character value representing the unit of measurement to which the element contents
#' should be transformed. This can be any combinations of masses and molar amounts (e.g. "g",
#' "mol", "g/g", "mol/g").
#' @param sample_mass Optionally. A numeric vector of class \code{\link[quantities:quantities]{quantities}}
#' with the same length as \code{x} that contains the mass of the corresponding samples. This is used for all
#' unit conversions that require the sample mass, for example to convert from a mass fraction [g/g] to molar
#' amount [mol].
#' @return \code{x} with converted element contents.
#' @export
elco_elco_convert <- function(x,
                              to,
                              sample_mass = NULL) {

  # checks
  stopifnot(elco_check_elco(x))
  if(!is.character(to)) {
    rlang::abort("`to` must be a character value, but is of class ", class(to)[[1]], ".")
  }
  if(!to %in% elco::el_units_allowed$unit_symbol) {
    rlang::abort(paste0("`to` must be one of `elco::el_units_allowed$unit_symbol`, but is ", to, "."))
  }
  if(!inherits(sample_mass, "quantities") && !is.null(sample_mass)) {
    rlang::abort("`sample_mass` must be NULL or a quantities object.")
  }
  if(!is.null(sample_mass)) {
    cond <- !units:::ud_are_convertible(sample_mass, units::set_units(1, "g"))
    if(cond) rlang::abort(paste0("`sample_mass` must have a unit of measurement representing a mass, but is given in ", as.character(units(sample_mass)), "."))
  }

  # get allowed units
  data("el_units_allowed", package = "elco")
  type_conversion_from <- el_units_allowed$type[el_units_allowed$unit_symbol == as.character(units(x))]
  type_conversion_to <- el_units_allowed$type[el_units_allowed$unit_symbol == to]

  element <- attr(x, "el_symbol")

  # convert unit
  x <- drop_elco(x)
  res <-
    switch(paste0(type_conversion_from, "-", type_conversion_to),
           # conversion between equivalent units
           "mass-mass" =,
           "mol-mol" =,
           "mass_mass-mass_mass" = ,
           "mol_mass-mol_mass" = {
             units(x) <- suppressWarnings(with(units::ud_units, to)) # needed to suppress akward encoding warnings
             x
           },
           # convert from mass to mass_mass and back
           "mass_mass-mass" = {
             stopifnot(!is.null(sample_mass))
             x * sample_mass
           },
           "mass-mass_mass" = {
             stopifnot(!is.null(sample_mass))
             x / sample_mass
           },
           # convert from mass to mol
           "mass-mol" = {
             molar_mass <- quantities::set_quantities(PeriodicTable::mass(element), unit = "g/mol", errors = 0)
             x / molar_mass
           },
           "mass_mass-mol" = {
             stopifnot(!is.null(sample_mass))
             molar_mass <- quantities::set_quantities(PeriodicTable::mass(element), unit = "g/mol", errors = 0)
             x / molar_mass * sample_mass
           },
           "mol_mass-mol" = {
             stopifnot(!is.null(sample_mass))
             x * sample_mass
           },
           "mass_mass-mol_mass" = {
             molar_mass <- quantities::set_quantities(PeriodicTable::mass(element), unit = "g/mol", errors = 0)
             x / molar_mass
           },
           # convert from mol to mass
           "mol-mass" = {
             molar_mass <- quantities::set_quantities(PeriodicTable::mass(element), unit = "g/mol", errors = 0)
             x * molar_mass
           },
           "mol-mass_mass" = {
             stopifnot(!is.null(sample_mass))
             molar_mass <- quantities::set_quantities(PeriodicTable::mass(element), unit = "g/mol", errors = 0)
             x * molar_mass / sample_mass
           },
           "mol-mol_mass" = {
             stopifnot(!is.null(sample_mass))
             x / sample_mass
           },
           "mol_mass-mass_mass" = {
             molar_mass <- quantities::set_quantities(PeriodicTable::mass(element), unit = "g/mol", errors = 0)
             x * molar_mass
           },
           rlang::abort("Not implemented yet.")
    )

  units(res) <- suppressWarnings(with(units::ud_units, to, mode = "standard"))

  elco_new_elco(res, element)

}

#' Converts the unit of objects of class \code{elco} in a \code{data.frame}.
#'
#' \code{elco_elco_convert_df} applies \code{\link{elco_elco_convert}} to all columns of
#' class \code{elco} in a specified \code{data.frame}
#'
#' @param x A \code{data.frame}.
#' @param to A character value representing the unit of measurement to which the element contents
#' should be transformed. This can be any combinations of masses and molar amounts (e.g. "g",
#' "mol", "g/g", "mol/g").
#' @param sample_mass Optionally. A numeric vector of class \code{\link[quantities:quantities]{quantities}}
#' with t\code{nrow(x)} elements that contains the mass of the corresponding samples. This is used for all
#' unit conversions that require the sample mass, for example to convert from a mass fraction [g/g] to molar
#' amount [mol].
#' @return \code{x} with converted element contents.
#' @export
elco_elco_convert_df <- function(x,
                                 to,
                                 sample_mass = NULL) {

  x_elco <- elco_identify_elco_df(x)
  x[, x_elco] <- purrr::map_df(x[, x_elco, drop = FALSE], elco_elco_convert, to = to, sample_mass = sample_mass)
  x

}
