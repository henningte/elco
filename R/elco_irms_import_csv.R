#' Reformats raw csv element analyzer isotope ratio mass spectrometry (EA-IRMS) data as exported by the vendor software.
#'
#' `elco_irms_import_csv` reads one or more csv files that contain raw element analyzer isotope ratio mass spectrometry
#' (EA-IRMS) data as exported by the vendor software of the IRMS spectrometer and tidies the
#' data. This includes reformatting columns and rows, deleting unneeded columns and rows,
#' adding units, and renaming columns.
#'
#' @param files A vector of character values representing paths to csv files with EA-IRMS raw data as exported
#' by the vendor software of the IRMS.
#' @return An object of class [`irms`][elco::elco_new_irms].
#' @export
elco_irms_import_csv <- function(files) {

  # checks
  if(!is.character(files)) {
    rlang::abort(paste0("`files` must be a character vector, not ", class(files)[[1]], "."))
  }
  cond <- !file.exists(files)
  if(any(cond)) {
    if(sum(cond) == 1) {
      rlang::abort(paste0("File ", files[[cond]], " does not exist."))
    } else {
      rlang::abort(paste0("Files ", paste(files[cond], collapse = ", "), " do not exist."))
    }
  }
  cond <- !stringr::str_detect(files, "\\.csv$")
  if(any(cond)) {
    if(sum(cond) == 1) {
      rlang::abort(paste0("File ", files[[cond]], " is not a csv file."))
    } else {
      rlang::abort(paste0("Files ", paste(files[cond], collapse = ", "), " are not csv files."))
    }
  }

  # import individual data
  d <- purrr::map(files, utils::read.csv, header = TRUE, as.is = TRUE, row.names = NULL)

  # extract analysis metadata
  d_metadata_analysis <- purrr::map2_df(d, seq_along(d), function(x, i) {
    tibble::tibble(
      file_id = i,
      file_name = x[1, 2, drop = TRUE],
      measurement_time = lubridate::parse_date_time(paste0(x[1, 4, drop = TRUE], " ", x[1, 6, drop = TRUE]), orders = "dmy %h:%M"),
      instrument_name = x[1, 8, drop = TRUE],
      measurement_software_version = x[1, 10, drop = TRUE],
      reanalysis_time = lubridate::parse_date_time(paste0(x[1, 12, drop = TRUE], " ", x[1, 14, drop = TRUE]), orders = "dmy %h:%M"),
      reanalysis_software_version = x[1, 10, drop = TRUE]
      )
  })
  d <- purrr::map(d, function(x) {
    x[-c(1:2), , drop = FALSE]
  })

  # rename columns
  d <-
    purrr::map(d, function(x) {
      colnames(x) <- c("measurement_id", "measurement_type", "sample_label", "sample_mass", "time", "file_name", "15N_area", "unknown1", "unknown2", "15N", "13C_area", "unknown3", "13C", "18O", "N", "C", "unknown4", "unknown5", "unknown6", "unknown7", "unknown8")
      x
    })

  # remove unneeded columns
  d <- purrr::map(d, function(x) {
    x[, !stringr::str_detect(colnames(x), "unknown")]
  })

  # remove unneeded rows
  d <- purrr::map(d, function(x) {
    x[-c(1:4), , drop = FALSE]
  })

  # reformat time
  d <- purrr::map(d, function(x) {
    x$time <- lubridate::parse_date_time(x$time, orders = "dmy %h:%M")
    x
  })

  # reformat units and numeric variables (unfortunately, there's no permil in units::udunits)
  d <- purrr::map(d, function(x) {
    dplyr::mutate(x,
                  "measurement_id" = as.integer(.data$measurement_id),
                  "sample_mass" = quantities::set_quantities(as.numeric(.data$sample_mass), unit = "mg", errors = 0),
                  "15N_area" = as.numeric(.data$`15N_area`),
                  "15N" = as.numeric(.data$`15N`),
                  "13C_area" = as.numeric(.data$`13C_area`),
                  "13C" = as.numeric(.data$`13C`),
                  "18O" = as.numeric(.data$`18O`),
                  "N" = elco_new_elco(quantities::set_quantities(as.numeric(.data$N)/100, unit = "g/g", errors = 0), el_symbol = "N"),
                  "C" = elco_new_elco(quantities::set_quantities(as.numeric(.data$C)/100, unit = "g/g", errors = 0), el_symbol = "C")
    )
  })

  # add file number
  d <- purrr::map2(d, seq_along(d), function(x, i) {
    dplyr::bind_cols(
      tibble::tibble(
        file_id = rep(i, nrow(x))
      ),
      x
    )
  })

  # remove awkward space at the beginning of sample labels
  d <- purrr::map(d, function(x) {
    dplyr::mutate(x, "sample_label" = stringr::str_remove(.data$sample_label, pattern = "^ "))
  })

  # merge
  d <- dplyr::bind_rows(d)
  elco_new_irms(d)

}
