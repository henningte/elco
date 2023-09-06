## code to prepare `elco_units` dataset goes here

# define units
chemical_elements <-
  PeriodicTable:::periodicTable |>
  dplyr::filter(symb != "Xx")
elco_units <-
  tibble::tibble(
    thing = chemical_elements$symb,
    mol = paste0("mol_", chemical_elements$symb),
    mol_names =
      purrr::map(chemical_elements$symb, function(.x) {
        paste0(c("mols_of_", "MolsOf"), .x)
      }),
    g = paste0("g_", chemical_elements$symb),
    g_names =
      purrr::map(chemical_elements$symb, function(.x) {
        paste0(c("grams_of_", "GramsOf"), .x)
      }),
    g_to_mol = paste(PeriodicTable::mass(chemical_elements$symb), mol)
  ) |>
  dplyr::bind_rows(
    tibble::tibble(
      thing = "sample",
      mol = "mol_sample",
      mol_names = list(c("mols_of_sample", "MolsOfSample")),
      g = "g_sample",
      g_names = list(c("grams_of_sample", "GramsOfSample")),
      g_to_mol = NA_character_
    )
  )

usethis::use_data(elco_units, overwrite = TRUE)
