#' Takes an object of class `irms` and extracts all rows referring to standards in `irms_standards`.
#'
#' `elco_irms_extract_standards` takes an object of class `irms` and extracts all rows
#' referring to standards in `irms_standards`.
#'
#' @param x An object of class [`irms`][elco::elco_new_irms]
#' @return An object of class [`irms_std`][elco::elco_new_irms_std] containing only the rows
#' in `x` where `sample_name \%in\% elco::irms_standards$standard_name`.
#' @export
elco_irms_extract_standards <- function(x) {

  # checks
  elco_check_irms(x)

  # get data on standards
  data("irms_standards", package = "elco")
  irms_standards <- irms_standards[, 1, drop = FALSE]
  colnames(irms_standards) <- "sample_label"

  # join and discard empty cases
  x <- dplyr::left_join(irms_standards, x, by = "sample_label")
  x <- x[!is.na(x$file_id), ]

  # restore class
  elco_new_irms_std(elco_new_irms(x))

}

#' Takes an object of class `irms` and checks if it contains only measurements on standards.
#'
#' `elco_irms_check_standards` takes an object of class `irms` and checks if it has only rows
#' referring to standards in `irms_standards`.
#'
#' @param x An object of class [`irms()`][elco::elco_new_irms]
#' @return `x`.
elco_irms_check_standards <- function(x) {

  # checks
  elco_check_irms(x)

  # get data on standards
  data("irms_standards", package = "elco")

  cond <- !purrr::map_lgl(x$sample_label, function(y) y %in% irms_standards$standard_name)
  if(any(cond)) {
    if(sum(cond) == 1) {
      rlang::abort(paste0("`x` does must contain only measurements on standards, but contains a non-standard measurement in row ", which(cond), "."))
    } else {
      rlang::abort(paste0("`x` does must contain only measurements on standards, but contains non-standard measurements in rows ", paste0(which(cond), collapse = ", "), "."))
    }
  }

  x

}
