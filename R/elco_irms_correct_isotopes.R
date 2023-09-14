#' Heuristic function to correct IRMS isotope signals by subtracting differences measured values for standards and their known isotope signature.
#'
#' `elco_irms_correct_isotopes` is a heuristic function to correct measured isotope signatures of samples
#' during IRMS analysis. Correction can be performed for either \eqn{^{13}}C or \eqn{^{13}}N and, if multiple
#' files are batch-processed, using either standards from all files or only from the file where the respective
#' sample is located in.
#' The procedure is as follows:
#' \enumerate{
#'   \item For a standard defined by the user, compute the median of the individually measured isotope signals
#'   for the selected isotope. The defined standard should be a trusted standard.
#'   \item For the same standard, count the number of samples that have been measured. If this number is smaller
#'   than a threshold `t`, print a warning. This is done to avoid that correction is done without enough
#'   samples to reliably compute a median value.
#'   \item Look up the known reference isotope signature for the selected standard. Compute the difference between
#'   this value and the median of the isotope signature of the standard. Take this difference and subtract it
#'   from each measured value.
#'   \item (Optionally): Extract the values of other standards after the correction, compute their medians, and
#'   check if the absolute deviation from the reference value is larger than some threshold. Print a warning
#'   if this is the case.
#'   \item (Optionally): create a plot showing uncorrected and corrected isotope signatures for all measured
#'   standards along their known reference values. This plot may be used as a visual check of the heuristic
#'   correction procedure.
#' }
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' @keywords internal
#'
#' @param x An object of class [`irms()`][elco::elco_new_irms].
#'
#' @param isotope A character value representing the isotope for which to
#' correct isotope signatures. This must be one of "13C" or "15N".
#'
#' @param ref A `data.frame` with the same format as [elco::irms_standards()]
#' and exactly one row. `ref` contains the data for the standard to use for the
#' correction.
#'
#' @param irms_standards_to_use A `data.frame` with the same format as
#' [elco::irms_standards()]. This can optionally be used to make available other
#' reference values for plotting.
#'
#' @param check A `data.frame` with the same format as [elco::irms_standards()].
#' `check` contains data for standards with which to check the correction. If
#' the median of their corrected isotope signature values deviates (absolutely)
#' by more than a threshold value as defined by `check$threshold_13C` or
#' `check$threshold_15N` (depending on the value of `isotope`), a warning will
#' be printed. Can alternatively be set to `NULL`, in which case no warning is
#' printed.
#'
#' @param t An integer value specifying the number of measurements that must be
#' available for a particular standard to make it eligible for being a reference
#' for correcting the measured isotope values.
#'
#' @param by_file A logical value indicating if medians of standards are
#' computed across different files as indicated by `x$file_id` or (`FALSE`)
#' individually for each file (`TRUE`).
#'
#' @param plotit A logical value indicating if a plot for checking should be printed (`TRUE`) or not
#' (`FALSE`).
#'
#' @export
elco_irms_correct_isotopes <- function(x,
                                       ref = irms_standards[irms_standards$standard_name == "BBOT", ],
                                       irms_standards_to_use = elco::irms_standards,
                                       check = irms_standards,
                                       isotope = "13C",
                                       t = 5,
                                       by_file = TRUE,
                                       plotit = FALSE) {

  lifecycle::deprecate_warn("0.1.0", "elco_irms_correct_isotopes()", "iloekirms::irms_correct_isotopes()")

  # checks
  elco_check_irms(x)
  if(!is.character(isotope)) {
    rlang::abort(paste0("`isotope` must be a character value, but is of class ", class(isotope)[[1]], "."))
  }
  cond <- length(isotope)
  if(cond != 1) {
    rlang::abort(paste0("`isotope` must be a character value, but is a vector of length ", cond, "."))
  }
  if(!isotope %in% c("13C", "15N")) {
    rlang::abort(paste0("`isotope` must be one of c('13C', '15N'), but is ", isotope, "."))
  }
  if(!is.data.frame(ref)) {
    rlang::abort(paste0("`ref` must be a data.frame, but is of class ", class(ref)[[1]], "."))
  }
  if(nrow(ref) != 1) {
    rlang::abort(paste0("`ref` must have exactly one row, but has ", nrow(ref), " rows."))
  }
  if(! is.null(irms_standards_to_use)) {
    irms_standards <- irms_standards_to_use
  } else {
    utils::data("irms_standards", envir = environment())
  }
  irms_standards_colnames <- colnames(irms_standards)
  cond <- ! colnames(ref) %in% irms_standards_colnames
  if(any(cond)) {
    if(sum(cond) == 1) {
      rlang::abort(paste0('`ref` must contain the columns ', paste(irms_standards_colnames, collapse = ", "), ', but contains a mismatching column ', colnames(ref)[cond], '.'))
    } else {
      rlang::abort(paste0('`ref` must contain the columns ', paste(irms_standards_colnames, collapse = ", "), ', but contains the mismatching columns ', paste(colnames(ref)[cond], collapse = ", "), '.'))
    }
  }
  if(!(is.data.frame(check) | is.null(check))) {
    rlang::abort(paste0("`check` must be a data.frame or `NULL`, but is of class ", class(check)[[1]], "."))
  }
  if(is.data.frame(check)) {
    cond <- !colnames(check) %in% irms_standards_colnames
    if(any(cond)) {
      if(sum(cond) == 1) {
        rlang::abort(paste0('`check` must contain the columns ', paste(irms_standards_colnames, collapse = ", "), ', but contains a mismatching column ', colnames(ref)[cond], '.'))
      } else {
        rlang::abort(paste0('`check` must contain the columns ', paste(irms_standards_colnames, collapse = ", "), ', but contains the mismatching columns ', paste(colnames(ref)[cond], collapse = ", "), '.'))
      }
    }
  }
  if(!is.numeric(t)) {
    rlang::abort(paste0("`t` must be a numeric value, but is of class ", class(t)[[1]], "."))
  }
  cond <- length(t)
  if(cond != 1) {
    rlang::abort(paste0("`t` must be a numeric value, but is a vector of length ", cond, "."))
  }
  if(t %% 1 != 0) {
    rlang::abort(paste0("`t` must be an integer value, but is ", t, "."))
  }
  if(!is.logical(by_file)) {
    rlang::abort(paste0("`by_file` must be a logical value, but is of class ", class(by_file)[[1]], "."))
  }
  cond <- length(by_file)
  if(cond != 1) {
    rlang::abort(paste0("`by_file` must be a value, but is a vector of length ", cond, "."))
  }
  if(!is.logical(plotit)) {
    rlang::abort(paste0("`plotit` must be a logical value, but is of class ", class(plotit)[[1]], "."))
  }
  cond <- length(plotit)
  if(cond != 1) {
    rlang::abort(paste0("`plotit` must be a value, but is a vector of length ", cond, "."))
  }

  # format data for processing; copy original data for plotting
  disotope <- rlang::sym(isotope)
  disotope_area <- rlang::sym(paste0(isotope, "_area"))
  x_or <- x
  if(by_file) {
    x <- split(x, f = x$file_id)
  } else {
    x <- list(x)
  }

  # get data on standards
  irms_standards_sel <- ref[, c("standard_name", as.character(disotope))]
  colnames(irms_standards_sel) <- c("sample_label", "disotope")

  # new column names for `check` to facilitate joining
  if(!is.null(check)) colnames(check)[[1]] <- "sample_label"

  # get median isotope signature and isotope signal area and the number of measurements for the reference standard
  x_standards <-
    purrr::map(x, function(.x) {
      .x %>%
        elco_irms_extract_standards(irms_standards_to_use = irms_standards) %>%
        dplyr::filter(.data$sample_label == irms_standards_sel$sample_label)
    })

  x_standards_signature_median <-
    purrr::map(x_standards, function(.x) {
      .x %>%
        dplyr::group_by(.data$sample_label) %>%
        dplyr::summarise(
          file_id = unique(.data$file_id),
          signature_measured = stats::median(!!disotope, na.rm = TRUE),
          area_measured = stats::median(!!disotope_area, na.rm = TRUE),
          count = length(.data$sample_mass),
          .groups = "keep"
        ) %>%
        dplyr::left_join(
          irms_standards_sel,
          by = "sample_label"
        ) %>%
        dplyr::mutate(
          signature_difference = .data$signature_measured - disotope
        )
    })

  # print warning if there are too few measurements in any file
  cond <-
    purrr::map2_lgl(x_standards_signature_median, seq_along(x_standards_signature_median), function(y, i) {
      cond <- !y$count >= t
      if(cond) rlang::warn(paste0("For file ", i, ", there are less than ", t, " measurements for the selected reference standard."))
      cond
    })

  # subtract
  x <-
    purrr::map2(x, x_standards_signature_median, function(y, z) {
      y[, as.character(disotope)] <- y[, as.character(disotope), drop = TRUE] - z$signature_difference
      y
    })

  # check if the signature of any standards in `check` deviates too much from the median corrected signature of the standards
  if(!is.null(check)) {
    purrr::map(x, function(.x) {

      # compute median signature of corrected standards
      .x <-
        .x %>%
        elco_irms_extract_standards() %>%
        dplyr::group_by(.data$sample_label) %>%
        dplyr::summarise(
          file_id = unique(.data$file_id),
          signature_measured = stats::median(!!disotope),
          .groups = "keep"
        ) %>%
        dplyr::left_join(
          check,
          by = "sample_label"
        ) %>%
        dplyr::mutate(
          signature_difference = .data$signature_measured - !!disotope
        )

      # check thresholds
      for(i in seq_len(nrow(.x))) {
        if(is.na(.x[i, paste0("threshold_", disotope)])) {
          rlang::warn(paste0("For standard ", .x$sample_label[[i]], " isotope correction cannot be checked because the threshold given in `check` is `NA`."))
        } else {
          if(abs(.x$signature_difference[[i]]) > .x[i, paste0("threshold_", disotope)]) {
            rlang::warn(paste0("For file ", .x$file_id[[i]], " and standard ", .x$sample_label[[i]], " the corrected isotope signature is larger than the reference threshold value."))
          }
        }
      }

      .x

    })
  }

  # bind rows
  x <- dplyr::bind_rows(x)

  # plot
  if(plotit) {

    # format for plotting
    x_or <-
      dplyr::bind_rows(
        dplyr::mutate(x_or, Corrected = FALSE),
        dplyr::mutate(x, Corrected = TRUE)
      ) %>%
      dplyr::mutate(
        sample_label =
          dplyr::case_when(
            sample_label %in% irms_standards$standard_name ~ sample_label,
            sample_label == "bla" ~ "Blank",
            TRUE ~ "Sample"
          )
      )

    if(by_file) {
      x_or <- split(x_or, f = x_or$file_id)
    } else {
      x_or <- list(x_or)
    }

    # plot (don't show blanks and samples because these often have such small values that checking for standards and samples gets impossible)
    for(i in seq_along(x_or)) {

      p1 <-
        ggplot2::ggplot(
          data = x_or[[i]] %>% dplyr::filter(! .data$sample_label %in% c("Blank", "Sample"))
        ) +
        ggplot2::geom_point(
          ggplot2::aes(x = .data$sample_label, y = !!disotope, color = .data$Corrected),
          position = ggplot2::position_jitterdodge()
        ) +
        ggplot2::geom_point(
          data = irms_standards %>% dplyr::filter(.data$standard_name %in% !!x$sample_label),
          ggplot2::aes(x = .data$standard_name, y = !!disotope)
        ) +
        ggplot2::geom_boxplot(
          ggplot2::aes(x = .data$sample_label, y = !!disotope, color = .data$Corrected),
          fill = NA
        ) +
        ggplot2::guides(colour = ggplot2::guide_legend(nrow = 2)) +
        ggplot2::theme(legend.position = "bottom") +
        ggplot2::labs(
          x = "Standards",
          y = bquote(delta^{.(stringr::str_extract(isotope, "[1-9]+"))}*.(stringr::str_extract(isotope, "[:alpha:]"))),
          title = paste0("File id: ", x_or[[i]]$file_id[[1]], ", Corrected and uncorrected isotope signatures")
        )

      print(p1)

    }

    # same plot as before, but now also with samples
    for(i in seq_along(x_or)) {

      p1 <-
        ggplot2::ggplot(
          data = x_or[[i]] %>% dplyr::filter(! .data$sample_label %in% c("Blank"))
        ) +
        ggplot2::geom_point(
          ggplot2::aes(x = .data$sample_label, y = !!disotope, color = .data$Corrected),
          position = ggplot2::position_jitterdodge()
        ) +
        ggplot2::geom_point(
          data = irms_standards %>% dplyr::filter(.data$standard_name %in% !!x$sample_label),
          ggplot2::aes(x = .data$standard_name, y = !!disotope)
        ) +
        ggplot2::geom_boxplot(
          ggplot2::aes(x = .data$sample_label, y = !!disotope, color = .data$Corrected),
          fill = NA
        ) +
        ggplot2::guides(colour = ggplot2::guide_legend(nrow = 2)) +
        ggplot2::theme(legend.position = "bottom") +
        ggplot2::labs(
          x = "Standards",
          y = bquote(delta^{.(stringr::str_extract(isotope, "[1-9]+"))}*.(stringr::str_extract(isotope, "[:alpha:]"))),
          title = paste0("File id: ", x_or[[i]]$file_id[[1]], ", Corrected and uncorrected isotope signatures")
        )

      print(p1)

    }

  }

  x

}
