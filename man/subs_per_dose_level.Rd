% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nif_exploration.R
\name{subs_per_dose_level}
\alias{subs_per_dose_level}
\title{Subjects per dose level}
\usage{
subs_per_dose_level(obj, analyte = NULL, group = NULL)
}
\arguments{
\item{obj}{A NIF object.}

\item{analyte}{The analyte as character. If \code{NULL} (default), all analytes
are selected.}

\item{group}{An (optional) grouping variable as character. Defaults to}
}
\value{
A data frame.
}
\description{
The number of subjects that have observations, per dose level and analyte.
}
\examples{
subs_per_dose_level(examplinib_poc_nif)
subs_per_dose_level(examplinib_sad_nif)
subs_per_dose_level(examplinib_poc_nif, group = "SEX", analyte = "RS2023")
}
