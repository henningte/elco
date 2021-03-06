% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/elco.R
\name{c.elco}
\alias{c.elco}
\title{Combine Values into a Vector or List}
\usage{
\method{c}{elco}(...)
}
\arguments{
\item{...}{objects to be concatenated.}
}
\description{
S3 method for \code{quantities} objects (see \code{\link{c}}).
}
\examples{
c(set_quantities(1, m/s, 0.2), set_quantities(30, km/h, 0.1))

}
