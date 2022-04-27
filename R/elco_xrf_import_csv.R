#' Reformats raw csv X-ray fluorescence data as exported by the vendor software.
#'
#' `elco_xrf_import_csv` reads one or more csv files that contain raw X-ray fluorescence (XRF) data as
#' exported by the vendor software of the XRF spectrometer (ZSX Primus II, Rigaku) and tidies the data. This includes
#' reformatting columns and rows, deleting unneeded columns and rows (summary statistics),
#' adding units, and renaming columns.
#'
#' @param files A vector of character values representing paths to csv files with XRF raw data as exported
#' by the vendor software of the XRF spectrometer (ZSX Primus II, Rigaku).
#' @return An object of class [`xrf`][elco::elco_new_xrf].
#' @export
elco_xrf_import_csv <- function(files) {

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

  # rename columns
  d <- purrr::map(d, function(x) {
    target_vars <- colnames(x)[6:ncol(x)]
    index_flags <- stringr::str_detect(target_vars, "X\\.")
    target_vars[index_flags] <- NA_character_
    target_vars <- ifelse(is.na(target_vars), target_vars[-1], target_vars)
    target_vars[!index_flags] <- paste0(target_vars[!index_flags], "_flag")
    colnames(x) <- c("sample_id", "method_name", "time", "unknown1", "unknown2", target_vars)
    x
  })

  # extract units
  index_target_vars <- purrr::map(d, function(x) {
    index <- seq_len(ncol(x))
    index > 5 & !stringr::str_detect(colnames(x), "_flag")
  })
  d_units <- purrr::map2(d, index_target_vars, function(x, i) {
    x_units <-
      tibble::tibble(
        el_symbol = colnames(x)[i],
        index_variable = which(i),
        unit = unlist(x[1, i])
      )
    index_unit <- x_units$unit == "ppm"
    x_units$division_factor <- ifelse(index_unit, 1, 100)
    x_units$unit[!index_unit] <- "g/g"
    x_units$unit[index_unit] <- "ug/g"
    x_units
  })
  d <- purrr::map(d, function(x) {
    x[-1, ]
  })

  # discard summary statistics
  d <- purrr::map(d, function(x) {
    index_summary <- (nrow(x) - 6):nrow(x)
    x[-index_summary, ]
  })

  # reformat units
  d <- purrr::map2(d, d_units, function(x, y) {
    x[, y$index_variable] <- purrr::map2_df(x[, y$index_variable], seq_len(nrow(y)), function(z, i) {
      res <- quantities::set_quantities(as.numeric(z)/y$division_factor[[i]], unit = y$unit[[i]], errors = 0, mode = "standard")
      if(y$el_symbol[[i]] != "C6H10O5N") {
        res <- elco_new_elco(res, el_symbol = y$el_symbol[[i]])
      }
      res
    })
    x
  })

  # reformat flags
  d <- purrr::map2(d, d_units, function(x, y) {
    x[, y$index_variable + 1] <- purrr::map(x[, y$index_variable + 1], function(z){
      ifelse(is.na(z) | z == "", FALSE, TRUE)
    })
    x
  })

  # reformat time
  d <- purrr::map(d, function(x) {
    x$time <- as.POSIXct(x$time)
    x
  })

  # remove unknown columns
  d <- purrr::map(d, function(x) {
    x[, !stringr::str_detect(colnames(x), "unknown")]
  })

  # merge
  d <- dplyr::bind_rows(d)
  elco_new_xrf(d)

}
