#' Heuristic function to correct IRMS element contents by computing a manual calibration function from the standard measurements and apply this to predict element contents in samples.
#'
#' \code{elco_irms_correct_elements} is a heuristic function to correct measured element contents of samples
#' during IRMS analysis. Correction can be performed for either C or N and, if multiple
#' files are batch-processed, using either standards from all files or only from the file where the respective
#' sample is located in.
#' The procedure is as follows:
#' \enumerate{
#'   \item For each measured standard, assign the known reference element mass content from
#'   \code{\link[irms_standard]{irms_standard}}.
#'   \item Use the information on the mass of the measured samples and the assigned element mass content values
#'   to compute the respective absolute known element content [mg].
#'   \item For all measured standards, compute a linear regression model that predicts the absolute
#'   element content in dependency of the respetive signal area.
#'   \item For each sample, use the regression model and the respective measured signal area to compute
#'   the absolute element content.
#'   \item Divide the absolute predicted element contents by the samples' masses [mg] to get the mass fraction of
#'   the element.
#'   \item (Optionally): create plot: (1) showing uncorrected and corrected element mass fractions
#'   for all measured standards and samples. This plot may be used as a visual check of the heuristic
#'   correction procedure and shows how the values for all samples are corrected. In addition, for each
#'   fitted regression model, the measured absolute element masses are plotted in dependency of the signal
#'   areas with the regression line. These plot can be used to assess if the regression model captures well
#'   the measured data for the standards.
#' }
#'
#' @param x An object of class \code{\link[elco:elco_new_irms]{irms}}.
#' @param element A character value representing the chemical element for which to correct the mass fraction
#' values. This must be one of "C" or "N".
#' @param standards A character vale specifying standards to use for computing the regression equation.
#' This must be one of \code{\link[irms_standard]{irms_standard$standard_name}}. Default is to
#' use all standards.
#' @param by_file A logical value indicating if medians of standards are computed across different files
#' as indicated by \code{x$file_id} or (\code{FALSE}) individually for each file (\code{TRUE}).
#' @param plotit A logical value indicating if a plot for checking should be printed (\code{TRUE}) or not
#' (\code{FALSE}).
#' @note ___ todo: Klaus suggested that to remove outliers in standard measurements that may not be discarded
#' by the median approach, we should define a threshold value for each standard to exclude outliers before
#' computing medians. Check if this should still be done.
#' @return \code{x} with corrected element content.
#' @export
elco_irms_correct_elements <- function(x,
                                       element = "C",
                                       standards = irms_standards$standard_name,
                                       by_file = TRUE,
                                       plotit = FALSE) {

  # checks
  elco_check_irms(x)
  if(!is.character(element)) {
    rlang::abort(paste0("`element` must be a character value, but is of class ", class(element)[[1]], "."))
  }
  cond <- length(element)
  if(cond != 1) {
    rlang::abort(paste0("`element` must be a character value, but is a vector of length ", cond, "."))
  }
  if(!element %in% c("C", "N")) {
    rlang::abort(paste0("`element` must be one of c('C', 'N'), but is ", element, "."))
  }
  data("irms_standards", package = "elco")
  if(!is.character(standards)) {
    rlang::abort(paste0("`standards` must be a character vector, but is of class ", class(standards)[[1]], "."))
  }
  cond <- !purrr::map_lgl(standards, function(y) y %in% irms_standards$standard_name)
  if(any(cond)) {
    if(sum(cond) == 1) {
      rlang::abort(paste0("Each of `standards` must be one of c(", paste(irms_standards$standard_name, collapse = ", "), "), but of `standards` is ", standards[[cond]], "."))
    } else {
      rlang::abort(paste0("Each of `standards` must be one of c(", paste(irms_standards$standard_name, collapse = ", "), "), but of some of `standards` are ", paste(standards[cond], collapse = ", "), "."))
    }
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
  element_m <- rlang::sym(element)
  n_neutrons <-
    switch(element,
           "C" = 13,
           "N" = 15)
  delement_area <- rlang::sym(paste0(n_neutrons, element, "_area"))
  x_or <- x
  if(by_file) {
    x <- split(x, f = x$file_id)
  } else {
    x <- list(x)
  }

  # get data on standards
  irms_standards_sel <- irms_standards[, c("standard_name", as.character(element_m))]
  colnames(irms_standards_sel) <- c("sample_label", "element_m")
  irms_standards_sel <- irms_standards_sel[irms_standards_sel$sample_label %in% standards, , drop = FALSE]

  # assign known C mass fractions to measured standards
  x_standards <- purrr::map(x, elco_irms_extract_standards)
  x_standards <- purrr::map(x_standards, function(y) {
    dplyr::left_join(y, irms_standards_sel, by = "sample_label")
  })

  # compute absolute C mass contents
  x_standards <- purrr::map(x_standards, function(y) {
    dplyr::mutate(y, element_m_abs = element_m * errors::drop_errors(sample_mass))
  })

  # compute regression models
  x_standards_lm <- purrr::map(x_standards, function(y) {
    y <- purrr::map_df(y[, c("element_m_abs", as.character(delement_area))], as.numeric) # drop units
    lm(y)
  })

  # predict values
  x <- purrr::map2(x, x_standards_lm, function(y, z) {
    res <- as.data.frame(predict(z, newdata = y, se.fit = TRUE, type = "response"))
    res$se_pi <- sqrt(res$se.fit^2 + res$residual.scale^2)
    res <- elco_new_elco(quantities::set_quantities(res$fit/as.numeric(y$sample_mass),
                                                    unit = units(y[, element, drop = TRUE]),
                                                    errors = res$se_pi/as.numeric(y$sample_mass), mode = "standard"), el_symbol = element)
    switch(element,
           "C" = dplyr::mutate(y, C = res),
           "N" = dplyr::mutate(y, N = res)
    )
  })

  # bind rows
  x <- dplyr::bind_rows(x)

  # plot
  if(plotit) {

    is_standard <- ifelse(x_or$sample_label %in% irms_standards$standard_name, x_or$sample_label, ifelse(x_or$sample_label == "bla", "Blank", "Sample"))

    # measured vs fitted values
    p1 <-
      ggplot(mapping = aes(y = as.numeric(x_or[, element, drop = TRUE]),
                         x = as.numeric(x[, element, drop = TRUE]),
                         colour = is_standard)) +
      geom_segment(data = irms_standards_sel,
                   mapping = aes(x = as.numeric(element_m),
                                 xend = as.numeric(element_m),
                                 y = 0,
                                 yend = as.numeric(element_m),
                                 colour = sample_label)) +
      geom_segment(data = irms_standards_sel,
                   mapping = aes(x = 0,
                                 xend = as.numeric(element_m),
                                 y = as.numeric(element_m),
                                 yend = as.numeric(element_m),
                                 colour = sample_label)) +
      geom_errorbarh(mapping = aes(y = as.numeric(x_or[, element, drop = TRUE]),
        xmin = as.numeric(x[, element, drop = TRUE]) - errors(x[, element, drop = TRUE]),
                                   xmax = as.numeric(x[, element, drop = TRUE]) + errors(x[, element, drop = TRUE]),), height = 0, colour = "dimgrey") +
      geom_point() +
      geom_abline(intercept = 0, slope = 1, colour = "dimgrey") +
      labs(y = "Measured", x = "Fitted", title = paste0("Element: ", element,", File: all files")) +
      guides(colour = guide_legend(title = "Sample type")) +
      theme(legend.position = "bottom")

    print(p1)

    # regression models
    purrr::map(x_standards, function(y) {
      p2 <-
        ggplot(y, aes(y = as.numeric(element_m_abs), x = !!delement_area)) +
        geom_smooth(method = "lm", se = FALSE, colour = "dimgrey") +
        geom_point(aes(colour = sample_label)) +
        geom_rug(data = x[is_standard == "Sample" & x$file_id == y$file_id[[1]], ], aes(y = as.numeric(C)/as.numeric(sample_mass), x = !!delement_area), sides="b") +
        labs(y = paste0("Absolute ", element, " mass [mg]"),
             x = "Signal area",
             title = paste0("Element: ", element,", File: ", y$file_id[[1]])) +
        guides(colour = guide_legend(title = "Standard")) +
        theme(legend.position = "bottom")
      print(p2)
    })

  }

  x

}
