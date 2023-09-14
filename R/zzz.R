.onLoad <- function(libname, pkgname) {

  ## install custom units for chemical elements and their conversion constants
  elco_units <- NULL
  utils::data("elco_units", package = "elco", envir = environment())

  # install units
  for(i in seq_len(nrow(elco_units))) {
    units::install_unit(
      symbol = elco_units$mol[[i]],
      name = elco_units$mol_names[[i]]
    )
    if(is.na(elco_units$g_to_mol[[i]])) {
      units::install_unit(
        symbol = elco_units$g[[i]],
        name = elco_units$g_names[[i]]
      )
    } else {
      units::install_unit(
        symbol = elco_units$g[[i]],
        name = elco_units$g_names[[i]],
        def = elco_units$g_to_mol[[i]]
      )
    }

  }

  invisible()
}


.onUnload <- function(libname, pkgname) {

  # remove custom units
  elco_units <- NULL
  utils::data("elco_units", package = "elco", envir = environment())
  for(i in seq_len(nrow(elco_units))) {
    units::remove_unit(symbol = elco_units$mol[[i]])
    units::remove_unit(symbol = elco_units$g[[i]])
  }

  invisible()

}
