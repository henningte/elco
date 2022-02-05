## code to prepare `xrf_calibration` dataset goes here

# import and format data
d <- elco::elco_xrf_import_csv(list.files("./inst/extdata/xrf_calibration/", full.names = TRUE))
d$sample_mass <- units::set_units(as.numeric(stringr::str_extract(d$sample_id, pattern = "[:digit:]{3}$")), "mg")
d$sample_id <- stringr::str_remove(d$sample_id, pattern = "_[:digit:]{3}$")

index_ref <- d$sample_mass == units::set_units(500, "mg")
index_ref <- purrr::map_int(d$sample_id, function(x) {
  which(d$sample_id == x & index_ref)
})

index_el <- which(elco::elco_identify_elco_df(d))

d_ratios <- d
d_ratios[, index_el] <- d_ratios[, index_el]/d_ratios[index_ref, index_el]

m <- purrr::map(index_el, function(x) {
  dx <- d_ratios[d_ratios$sample_mass != units::set_units(500, "mg"), match(c("sample_id", "sample_mass", colnames(d_ratios)[[x]]), colnames(d_ratios))]
  dx$sample_mass <- as.character(dx$sample_mass)
  if(colnames(dx)[[3]] %in% "Cl"){ # no values >0 measured currently
    return(NULL)
  }
  colnames(dx)[[3]] <- "y"
  dx$y <- as.numeric(dx$y)
  rstanarm::stan_glmer(y ~ sample_mass + (1|sample_id), data = dx, family = Gamma(link = "log"), adapt_delta = 0.99, seed = 1)
})

# For each model: get the factors to divide by
xrf_calibration <-
  purrr::map(m, function(x) {

    if(is.null(x)) {
      return(NULL)
    }
    newx <- data.frame(sample_mass = as.character(sort(unique(d_ratios$sample_mass[d_ratios$sample_mass != units::set_units(500, "mg")]))))
    yhat <- rstanarm::posterior_predict(x,
                                        re.form = ~0,
                                        newdata = newx)
    yhat <- as.data.frame(yhat)
    colnames(yhat) <- newx$sample_mass
    yhat[, order(colnames(yhat))]

  })

# compute the mean and standard error and store all as quantities object
xrf_calibration <-
  purrr::map_df(seq_along(index_el), function(i) {

    x <- xrf_calibration[[i]]
    if(is.null(x)) {
      return(NULL)
    }
    res <- purrr::map2_df(x, colnames(x), function(y, z) {
      data.frame(el_symbol = attr(d[, index_el[[i]]], "el_symbol"),
                 sample_mass = z,
                 correction_factor = quantities::set_quantities(mean(y), unit = "1", errors = sd(y)),
                 stringsAsFactors = FALSE
                 )
    })
    res

  })

# a <- rstanarm::stan_glm(as.numeric(Ca) ~ sample_mass, data = d_ratios, family = Gamma(link = "log"), adapt_delta = 0.99, seed = 1)

# i = 15
# ppc_intervals(
#   y = as.numeric(d_ratios[d_ratios$sample_mass != units::set_units(500, "mg"), index_el[[i]]]),
#   yrep = posterior_predict(m[[i]], re.form = ~0),
#   x = as.numeric(d_ratios[d_ratios$sample_mass != units::set_units(500, "mg"), "sample_mass"]),
#   prob = 0.5
# )

usethis::use_data(xrf_calibration, overwrite = TRUE)
