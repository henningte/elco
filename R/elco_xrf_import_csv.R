#' Reformats raw csv X-ray fluorescence data as exported by the vendor software.
#'
#' `elco_xrf_import_csv` reads one or more csv files that contain raw X-ray
#' fluorescence (XRF) data as exported by the vendor software of the XRF
#' spectrometer (ZSX Primus II, Rigaku) and tidies the data. This includes
#' reformatting columns and rows, deleting unneeded columns and rows (summary
#' statistics), adding units, and renaming columns.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @keywords internal
#'
#' @param files A vector of character values representing paths to csv files
#' with XRF raw data as exported by the vendor software of the XRF spectrometer
#' (ZSX Primus II, Rigaku).
#'
#' @param use_intensities Logical value indicating whether instead of elemental
#' contents intensities of peaks should be extracted (`TRUE`) or not (`FALSE`).
#'
#' @return A data frame with the extracted elemental contents (if
#' `use_intensities = FALSE`) or intensities (if `use_intensities = TRUE`).
#'
#' @export
elco_xrf_import_csv <- function(files, use_intensities = FALSE) {

  lifecycle::deprecate_warn("0.1.0", "elco_xrf_import_csv()", "iloekxrf::xrf_import_csv()")

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
  if(!(is.logical(use_intensities) && length(use_intensities) == 1)) {
    rlang::abort("`use_intensities` must be a logical value.")
  }

  # import individual data
  d <- purrr::map(files, utils::read.csv, header = TRUE, as.is = TRUE, row.names = NULL)

  if(! use_intensities) {
    # rename columns
    d <-
      purrr::map(d, function(x) {
        target_vars <- colnames(x)[6:ncol(x)]
        index_flags <- stringr::str_detect(target_vars, "X\\.")
        target_vars[index_flags] <- NA_character_
        target_vars <- ifelse(is.na(target_vars), target_vars[-1], target_vars)
        target_vars[!index_flags] <- paste0(target_vars[!index_flags], "_flag")
        colnames(x) <- c("sample_id", "method_name", "time", "unknown1", "unknown2", target_vars)
        x
    })

    # extract units
    index_target_vars <-
      purrr::map(d, function(x) {
        index <- seq_len(ncol(x))
        index > 5 & ! stringr::str_detect(colnames(x), "_flag")
      })
    d_units <-
      purrr::map2(d, index_target_vars, function(x, i) {
        x_units <-
          tibble::tibble(
            el_symbol = colnames(x)[i],
            index_variable = which(i),
            unit = unlist(x[1, i])
          )
        index_unit <- x_units$unit == "ppm"
        x_units$division_factor <- ifelse(index_unit, 1, 100)
        x_units$unit[! index_unit] <- paste0("g_", x_units$el_symbol[! index_unit], "/g_sample")
        x_units$unit[index_unit] <- paste0("ug_", x_units$el_symbol[index_unit], "/g_sample")
        x_units$unit[x_units$el_symbol == "C6H10O5N" & index_unit] <- "ug/g_sample"
        x_units
      })

    d <-
      purrr::map(d, function(x) {
        x[-1, ]
      })

    # discard summary statistics
    d <-
      purrr::map(d, function(x) {
        index_summary <- which(x[, 1, drop = TRUE] == "Number")[[1]]:nrow(x)
        x[-index_summary, ]
      })

    # reformat units
    d <-
      purrr::map2(d, d_units, function(x, y) {
        x[, y$index_variable] <-
          purrr::map2_df(x[, y$index_variable], seq_len(nrow(y)), function(z, i) {
            quantities::set_quantities(as.numeric(z)/y$division_factor[[i]], unit = y$unit[[i]], errors = 0, mode = "standard")
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

  } else {

    d <-
      purrr::map(d, function(.x) {

        # extract rows
        index_boundary_lines <-
          tibble::tibble(
            first =
              which(purrr::map_lgl(seq_len(nrow(.x)), function(i) {
                any(stringr::str_detect(as.character(unlist(.x[i, ])), pattern = "^Na-KA$"))
              })),
            last = which(.x[, 1, drop = TRUE] == "Number" & !duplicated(.x[, 1, drop = TRUE], fromLast = TRUE)) - 1L
          )

        .x <- .x[index_boundary_lines$first:index_boundary_lines$last, ]

        # set column names
        column_is_not_all_na <-
          purrr::map_lgl(.x, function(.x2) !(all(is.na(.x2)) || all(.x2 == "")))

        .x <- .x[, column_is_not_all_na]
        index_target_vars <- 6:ncol(.x)
        colnames(.x) <- c("sample_id", "method_name", "time", "unknown1", "unknown2", stringr::str_replace(unlist(.x[1, index_target_vars]), pattern = " ?-", replacement = "_"))

        # set units
        units_target_vars <- unlist(.x[2, index_target_vars])
        .x <- .x[-c(1:2), ]
        .x[, index_target_vars] <-
          purrr::map2_dfc(.x[, index_target_vars], units_target_vars, function(.x2, .x3) {
            units::set_units(as.numeric(.x2), value = .x3, mode = "standard")
          })

        .x

      })

  }

  # reformat time and remove unknown columns
  d <-
    purrr::map(d, function(.x) {
      .x$time <- as.POSIXct(.x$time)
      .x[, !stringr::str_detect(colnames(.x), "unknown")]
    })

  # merge
  d <- dplyr::bind_rows(d)

  d

}
