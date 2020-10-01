## code to prepare `el_units_allowed` dataset goes here

mass = c("kg", "g", "mg", "µg", "ng", "pg")
amount = c("mol", "µmol", "nmol", "pmol")

mass_mass_combn <- expand.grid(mass1 = mass, mass2 = mass)
mol_mass_combn <- expand.grid(amount = amount, mass = mass)

el_units_allowed <-
  data.frame(
    type = c(
      rep("mass", length(mass)),
      rep("mol", length(amount)),
      rep("mass_mass", nrow(mass_mass_combn)),
      rep("mol_mass", nrow(mol_mass_combn))
    ),
    unit_symbol =
      c(
        mass,
        amount,
        paste0(mass_mass_combn$mass1, "/", mass_mass_combn$mass2),
        paste0(mol_mass_combn$amount, "/", mol_mass_combn$mass)
      ),
    stringsAsFactors = FALSE
  )

usethis::use_data(el_units_allowed, overwrite = TRUE)
