% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/elco_elco_convert.R
\name{elco_elco_convert_df}
\alias{elco_elco_convert_df}
\title{Converts the unit of objects of class \code{elco} in a \code{data.frame}.}
\usage{
elco_elco_convert_df(x, to, sample_mass = NULL)
}
\arguments{
\item{x}{A \code{data.frame}.}

\item{to}{A character value representing the unit of measurement to which the element contents
should be transformed. This can be any combinations of masses and molar amounts (e.g. "g",
"mol", "g/g", "mol/g").}

\item{sample_mass}{Optionally. A numeric vector of class \code{\link[quantities:quantities]{quantities::quantities()}}
with t\code{nrow(x)} elements that contains the mass of the corresponding samples. This is used for all
unit conversions that require the sample mass, for example to convert from a mass fraction [g/g] to molar
amount [mol].}
}
\value{
\code{x} with converted element contents.
}
\description{
\code{elco_elco_convert_df} applies \code{\link[=elco_elco_convert]{elco_elco_convert()}} to all columns of
class \code{elco} in a specified \code{data.frame}
}
