#' Plots an object of class xrf.
#'
#' `plot.xrf` is the plot method for objects of class
#' `xrf`. This function can be used to create plots for checking the
#' data.
#'
#' @param x An object of class [`xrf()`][elco::elco_new_xrf].
#' @param ... Further arguments, will be ignored
#' @return An object of class [`ggplot2()`][ggplot2::ggplot]
#' @export
plot.xrf <- function(x,
                     ...) {

  elco_check_xrf(x)

  # reformat the data for plotting
  x_sample_id <- x$sample_id
  x <- x[, !colnames(x) %in% c("method_name", "time")]
  x <- purrr::map_df(x, as.character)
  x <- tidyr::pivot_longer(x, cols = -.data$sample_id, names_to = "variable", values_to = "value")
  x <-
    dplyr::mutate(
      x,
      index_flag = stringr::str_detect(.data$variable, "_flag"),
      variable = stringr::str_remove(.data$variable, "_flag")
    )
  x <- split(x, f = x$index_flag)
  x <- purrr::map(x, dplyr::select, -.data$index_flag)
  x <- dplyr::left_join(x[[1]], x[[2]],  by = c("sample_id", "variable"))
  x <- dplyr::rename(x, "value" = "value.x", "flag" = "value.y")
  x <-
    dplyr::mutate(
      x,
      "sample_id" = factor(.data$sample_id, levels = x_sample_id),
      "variable" = factor(.data$variable, levels = unique(.data$variable)),
      "value" = as.numeric(.data$value),
      "flag" = as.logical(.data$flag)
    )

  # plot
  ggplot2::ggplot(x, ggplot2::aes(x = .data$sample_id, y = .data$value, colour = .data$flag)) +
    ggplot2::geom_point() +
    ggplot2::coord_flip() +
    ggplot2::facet_grid(~ .data$variable, scales = "free_x") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.2))

}
