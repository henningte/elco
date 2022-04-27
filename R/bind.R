#'
#' @export
cbind.elco  <- function(..., deparse.level = 1) {
  dots <- list(...)
  stopifnot(all(sapply(dots, inherits, "units")))
  u <- units(dots[[1]])
  dots <- getS3method("set_units", "mixed_units")(dots, as.character(u))

  nm <- names(as.list(match.call()))
  nm <- nm[nm != "" & nm != "deparse.level"]
  if (is.null(nm))
    names(dots) <- sapply(substitute(list(...))[-1], deparse)
  else names(dots) <- nm

  call <- as.character(match.call()[[1]])
  assign(call, getS3method(call, "errors"))
  value <- do.call(call, c(dots, deparse.level=deparse.level))
  attr(value, "units") <- u
  structure(value, class = c("elco", "quantities", "units", "errors"))
}

#'
#' @export
rbind.elco <- cbind.elco
