% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nif_class.R
\name{add_dose_level}
\alias{add_dose_level}
\title{Add dose level (\code{DL}) column}
\usage{
add_dose_level(obj)
}
\arguments{
\item{obj}{A NIF dataset.}
}
\value{
A NIF dataset.
}
\description{
Dose level is defined as the starting dose. For data sets with single drug
administration, \code{DL}is a numerical value, for drug combinations, it is a
character value specifying the \code{PARENT} and dose level for the individual
components.
}
\examples{
head(add_dose_level(examplinib_sad_nif))
head(add_dose_level(examplinib_sad_min_nif))
}
