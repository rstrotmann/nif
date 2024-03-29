% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nif_class.R
\name{max_time}
\alias{max_time}
\title{Maximal time in nif object}
\usage{
max_time(obj, analyte = NULL, only_observations = FALSE)
}
\arguments{
\item{obj}{A nif object.}

\item{analyte}{The analyte to filter for, as character.}

\item{only_observations}{Maximal ovservation time as logical.}
}
\value{
Numeric.
}
\description{
Maximal time in nif object
}
\examples{
max_time(examplinib_poc_nif)
max_time(examplinib_poc_nif, analyte = "RS2023")
max_time(examplinib_poc_nif, only_observations = TRUE)
}
