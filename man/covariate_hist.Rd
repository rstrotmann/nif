% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nif_exploration.R
\name{covariate_hist}
\alias{covariate_hist}
\title{Generic covariate distribution histogram}
\usage{
covariate_hist(obj, field, nbins = 11)
}
\arguments{
\item{obj}{The NIF data set.}

\item{field}{The field of the NIF object as character.}

\item{nbins}{The number of bins to be used if no binwidth is specified.
Defaults to 11.}
}
\value{
A plot object.
}
\description{
Generic covariate distribution histogram
}
\examples{
covariate_hist(examplinib_sad_nif, "AGE")
covariate_hist(examplinib_sad_nif, "BL_CRCL")

}
