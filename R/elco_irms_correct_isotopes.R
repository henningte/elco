#' Heuristic function to correct IRMS isotope signals by substracting differences measured values for standards and their known isotope signature.
#'
#' \code{elco_irms_correct_isotopes} is a heuristic function to correct measured isotope signatures of samples
#' during IRMS analysis. Correction can be performed for either \eqn{^{13}}C or \eqn{^{13}}N and, if multiple
#' files are batch-processed, using either standards from all files or only from the file where the respective
#' sample is located in.
#' The procedure is as follows:
#' \enumerate{
#'   \item For each standard, compute the median isotope signature and median signal area for the respective isotope.
#'   The median is used instead of an average to prevent potential outliers from having too large an effect.
#'   \item For each standard, record the number of measurements. This is used to exclude using standards
#'   as reference with too few measurmements since these will have large uncertainties in their estimated
#'   median. From the list of standards, discard all with <t measurements, where t is an integer value >1.
#'   \item For each sample (including the measured standards), compute the absolute difference in the signal area for the respective isotope to all
#'   standards' median signal area values. Record for each sample the standard with the least absolute difference.
#'   \item Look up the known reference isotope signatures for each assigned standard and substract this value
#'   from the measured value.
#'   \item (Optionally): create a plot showing uncorrected and corrected isotope signatures for all measured
#'   standards along their known reference values. This plot may be used as a visual check of the heuristic
#'   correction procedure.
#' }
#'
#' @param x An object of class \code{\link[elco:elco_new_irms]{irms}}.
#' @param isotope A character value representing the isotope for which to correct isotope signatures.
#' This must be one of "13C" or "15N".
#' @param t An integer value specifying the number of measurements that must be available for a particular
#' standard to make it eligble for being a reference for correcting the measured isotope values.
#' @param by_file A logical value indicating if medians of standards are computed across different files
#' as indicated by \code{x$file_id} or (\code{FALSE}) individually for each file (\code{TRUE}).
#' @param plotit A logical value indicating if a plot for checking should be printed (\code{TRUE}) or not
#' (\code{FALSE}).
#' @note ___ todo: Klaus suggested that to remove outliers in standard measurements that may not be discarded
#' by the median approach, we should define a threshold value for each standard to exclude outliers before
#' computing medians. Check if this should still be done.
#' @return nothig.
#' @export
elco_irms_correct_isotopes <- function(x,
                                      isotope = "13C",
                                      t = 5,
                                      by_file = TRUE,
                                      plotit = FALSE) {

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
  if(!is.numeric(t)) {
    rlang::abort(paste0("`isotope` must be a numeric value, but is of class ", class(t)[[1]], "."))
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
  data("irms_standards", package = "elco")
  irms_standards_sel <- irms_standards[, c("standard_name", as.character(disotope))]
  colnames(irms_standards_sel) <- c("sample_label", "disotope")

  # get median isotope signature and isotope signal area and the number of measurements for each standard
  x_standards <- purrr::map(x, elco_irms_extract_standards)
  x_standards_signature_median <- purrr::map(x_standards, function(y) {
    y <- dplyr::group_by(y, sample_label)
    y <- dplyr::summarise(y,
                          file_id = unique(file_id),
                          signature_measured = median(!!disotope),
                          area_measured = median(!!disotope_area),
                          count = length(sample_mass),
                          .groups = "keep")
    y <- dplyr::left_join(y, irms_standards_sel, by = "sample_label")
    y$signature_difference <- y$signature_measured - y$disotope
    y
  })

  # exclude standards with too few measurements
  x_standards_signature_median <- purrr::map(x_standards_signature_median, function(y) {
    y[y$count >= t, , drop = FALSE]
  })

  # assign the isotope signature to substract from each measured value
  x <- purrr::map2(x, x_standards_signature_median, function(y, z) {
    res <-
      purrr::map_df(seq_len(nrow(y)), function(i) {
        index <- which.min(abs(y[i , as.character(disotope_area), drop = TRUE] - z$area_measured))
        tibble::tibble(
          standard_name = z$sample_label[[index]],
          signature_measured = z$signature_measured[[index]],
          signature_known = z$disotope[[index]],
          area_measured = z$area_measured[[index]],
          signature_difference = z$signature_difference[[index]]
        )
      })
    dplyr::bind_cols(y, res)
  })

  # substract
  x <- purrr::map(x, function(y) {
    y[, as.character(disotope)] <- y[, as.character(disotope), drop = TRUE] - y$signature_difference
    y
  })

  # bind rows
  x <- dplyr::bind_rows(x)

  # plot
  if(plotit) {
    x_or <-
      dplyr::bind_rows(
        dplyr::mutate(x_or, Corrected = FALSE),
        dplyr::mutate(x, Corrected = TRUE)
      )
    x_or$sample_label <- ifelse(x_or$sample_label %in% irms_standards_sel$sample_label, x_or$sample_label, "Sample")
    x_or$signature_known[x_or$sample_label == "Sample"] <- NA_real_

    # plot without corrected samples (these often have differing values which makes it difficult to check the correction for the standards)
    print(
      ggplot(dplyr::filter(x_or, sample_label != "Sample"), aes(x = sample_label)) +
        geom_jitter(aes(y = !!disotope, colour = Corrected, shape = standard_name)) +
        stat_boxplot(aes(y = !!disotope, colour = Corrected), fill = NA) +
        geom_point(aes(x = sample_label, y = signature_known)) +
        guides(shape = guide_legend(title = "Standard used for correction", nrow = 2),
               colour = guide_legend(nrow = 2)) +
        theme(legend.position = "bottom") +
        labs(x = "Standards",
             y = bquote(delta^{.(stringr::str_extract(isotope, "[1-9]+"))}*.(stringr::str_extract(isotope, "[:alpha:]"))),
             title = "Corrected and uncorrected isotope signatures")
    )
    print(
      ggplot(x_or, aes(x = sample_label)) +
        geom_jitter(aes(y = !!disotope, colour = Corrected, shape = standard_name)) +
        stat_boxplot(aes(y = !!disotope, colour = Corrected), fill = NA) +
        geom_point(aes(x = sample_label, y = signature_known)) +
        guides(shape = guide_legend(title = "Standard used for correction", nrow = 2),
               colour = guide_legend(nrow = 2)) +
        theme(legend.position = "bottom") +
        labs(x = "Standards and Samples",
             y = bquote(delta^{.(stringr::str_extract(isotope, "[1-9]+"))}*.(stringr::str_extract(isotope, "[:alpha:]"))),
             title = "Corrected and uncorrected isotope signatures")
    )

  }

  # discard unneded columns
  x[, 1:14]

}
