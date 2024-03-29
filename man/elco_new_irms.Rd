% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/irms.R
\name{elco_new_irms}
\alias{elco_new_irms}
\title{Creates an object of class \code{irms}}
\usage{
elco_new_irms(x)
}
\arguments{
\item{x}{A \code{data.frame} with a row for each measured sample or standard and the
following columns:
\describe{
\item{file_id}{An integer increasing by 1 and starting from 1, representing a unique ID for each
csv file that has been imported.}
\item{measurement_id}{An integer increasing by 1 and starting from 1 for each imported file,
representing a unique ID for each measurement per file.}
\item{measurement_type}{A character vector representing the measurement type. One of "Sam" (the
measurement is a measurement of a sample) or "Ref" (the measurement is a measurement of a
standard/reference).}
\item{sample_label}{A character vector with names for the samples. If \code{measurement_type == "Sam"},
this is a label for the sample. If \code{measurement_type == "Ref"}, this is the name of the standard
as given in \code{\link[=irms_standards]{irms_standards()}}}.
\item{sample_mass}{A quantities vector representing the mass of the sample [g] with measurement errors (these are set to 0 by default).}
\item{time}{A POSIXct vector with the date and time when the sample was measured.}
\item{file_name}{A character vector with the full path to the .RUN file containing the raw
data for the corresponding measurement.}
\item{15N_area}{A numeric vector with the area of the chromatogram peak for the N content and
\eqn{\delta^{15}\text{N\textperthousand}} value.}
\item{15N}{A numeric vector with the \eqn{\delta^{15}\text{N\textperthousand}} value.}
\item{13C_area}{A numeric vector with the area of the chromatogram peak for the C content and
\eqn{\delta^{13}\text{C\textperthousand}} value.}
\item{13C}{A numeric vector with the \eqn{\delta^{13}\text{C\textperthousand}} value.}
\item{18O}{A numeric vector with the \eqn{\delta^{18}\text{O\textperthousand}} value.}
\item{C}{A numeric vector with the relative mass fraction of C [g/g].}
\item{N}{A numeric vector with the relative mass fraction of N [g/g].}
}}
}
\value{
An object of class \code{irms}. This is identical to \code{x}, but has
an additional class attribute.
}
\description{
\code{elco_new_irms} is the internal constructor function for objects of class
\code{irms}.
An object of class \code{irms} is a \code{\link[tibble:tibble]{tibble::tibble()}} with a
sample in each row and a prescribed set of columns for metadata and
element contents. Objects of class \code{irms} are intended to be created
only internally.
}
