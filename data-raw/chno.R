## code to prepare `chno` dataset goes here

# create toy data
set.seed(123)
no <- 5
chno <-
  data.frame(
    C = rbeta(no, shape1 = 1000, shape2 = 1000),
    H = rbeta(no, shape1 = 1000, shape2 = 5000),
    N = rbeta(no, shape1 = 1000, shape2 = 10000),
    O = rbeta(no, shape1 = 1000, shape2 = 5000),
    stringsAsFactors = FALSE
  )

# convert to class elco
chno <- purrr::map_df(chno, quantities::set_quantities, unit = "g/g", errors = 0)
chno <- purrr::map2_df(chno, colnames(chno), function(x, y) elco::elco_new_elco(x, el_symbol = y))

# add sample mass
chno <-
  dplyr::mutate(chno,
                sample_mass = quantities::set_quantities(
                  rnorm(no, mean = 4, sd = 0.3),
                  unit = "mg",
                  errors = rnorm(no, mean = 0, sd = 0.01)
                  )
                )

usethis::use_data(chno, overwrite = TRUE)
