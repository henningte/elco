#' Takes an object of class `irms_std` and appends it to an existing rds file.
#'
#' `elco_irms_extract_standards` takes an object of class `irms_std` and extracts all rows
#' referring to standards in `irms_standards`.
#'
#' @param x An object of class [`irms_std`][elco::elco_new_irms_std].
#' @param file A character value representing a path to an existing rds file with
#' an object of class [`irms_std`][elco::elco_new_irms_std] or to such a file
#' to be created.
#' @param initialize A logical value indicating if the file pointed to by `file`
#' already exists and `x` should be appended to this (`TRUE`), or does not exist
#' and a new file should be created (`TRUE`).
#' @param verbose A logical value indicating if messages should be printed (`TRUE`) or
#' not (`FALSE`).
#' @return nothig.
#' @export
elco_irms_write_standards <- function(x,
                                      file,
                                      initialize = FALSE,
                                      verbose = FALSE) {

  # checks
  elco_check_irms_std(x)
  if(!is.character(file)) {
    rlang::abort(paste0("`file` must be a character value, but is of class ", class(file)[[1]], "."))
  }
  cond <- length(file)
  if(cond != 1) {
    rlang::abort(paste0("`file` must be a value, but is a vector of length ", cond, "."))
  }
  cond <- !stringr::str_detect(file, pattern = "\\.rds$")
  if(cond) {
    rlang::abort('`file` must points to a file with extension ".rds".')
  }
  if(!is.logical(initialize)) {
    rlang::abort(paste0("`initialize` must be a logical value, but is of class ", class(initialize)[[1]], "."))
  }
  cond <- length(initialize)
  if(cond != 1) {
    rlang::abort(paste0("`initialize` must be a value, but is a vector of length ", cond, "."))
  }
  if(!is.logical(verbose)) {
    rlang::abort(paste0("`verbose` must be a logical value, but is of class ", class(verbose)[[1]], "."))
  }
  cond <- length(verbose)
  if(cond != 1) {
    rlang::abort(paste0("`verbose` must be a value, but is a vector of length ", cond, "."))
  }
  if(file.exists(file) && initialize) {
    rlang::abort("File `file` exists, but `initialize` is set to TRUE. No file was created and `x` was not exported.")
  }
  if(!file.exists(file) && !initialize) {
    rlang::abort("File `file` does not exist, but `initialize` is set to FALSE. No file was created and `x` was not exported.")
  }

  # get and join data from file o create new file
  if(file.exists(file)) {
    x_file <- elco_check_irms_std(readRDS(file))
    x <- dplyr::bind_rows(x, x_file)

    # update file_id
    x$file_id <- data.table::rleidv(x$file_id)

  } else {
    file.create(file)
    if(verbose) {
      message(paste0("File ", file, " successfully created."))
    }
  }

  # export
  saveRDS(x, file = file)

}
