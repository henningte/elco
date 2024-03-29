% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/elco.R
\name{elco-cast}
\alias{elco-cast}
\alias{elco_as_elco}
\alias{elco_as_elco.quantities}
\alias{elco_as_elco.elco}
\alias{as.data.frame.elco}
\title{Cast an object to an \code{elco} object}
\usage{
elco_as_elco(x, ...)

\method{elco_as_elco}{quantities}(x, el_symbol, ...)

\method{elco_as_elco}{elco}(x, ...)

\method{as.data.frame}{elco}(x, row.names = NULL, optional = FALSE, ...)
}
\arguments{
\item{x}{An object to be converted to class \code{\link[=elco_new_elco]{elco}} or
and \code{elco} object to be converted to a different class.}

\item{...}{Additional arguments passed on to methods.}

\item{el_symbol}{A character vector representing a chemical element symbol to
set for the \code{elco} object \code{x} gets converted to.}

\item{row.names}{\code{NULL} or a character vector giving the row
    names for the data frame.  Missing values are not allowed.}

\item{optional}{logical. If \code{TRUE}, setting row names and
    converting column names (to syntactic names: see
    \code{\link[base]{make.names}}) is optional.  Note that all of \R's
    \pkg{base} package \code{as.data.frame()} methods use
    \code{optional} only for column names treatment, basically with the
    meaning of \code{\link[base]{data.frame}(*, check.names = !optional)}.
    See also the \code{make.names} argument of the \code{matrix} method.}
}
\value{
\itemize{
\item \code{elco_as_elco} returns \code{x} converted to an object of class \code{elco}.
\item \code{as.data.frame.elco} returns \code{x} converted to a data frame.
}
}
\description{
Cast an object to an \code{elco} object
}
\examples{
# elco_as_elco.quantities
elco::chno$C \%>\%
  elco_drop_elco() \%>\%
  elco_as_elco(el_symbol = "C")

# elco_as_elco.elco
elco::chno$C \%>\%
  elco_as_elco()

# elco_as_elco.elco
elco::chno$C \%>\%
  as.data.frame() \%>\%
  setNames("C")

}
