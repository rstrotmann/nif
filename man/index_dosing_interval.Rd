% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nif_class.R
\name{index_dosing_interval}
\alias{index_dosing_interval}
\title{Index dosing intervals}
\usage{
index_dosing_interval(obj)
}
\arguments{
\item{obj}{The NIF object.}
}
\value{
A new NIF object.
}
\description{
This function adds a column \code{DI} that indicates the dosing interval. All
baseline observations before the first dosing interval get assigned to the
first dosing interval, too. In addition to \code{DI}, the function also calls
\code{index_nif()}, thus creating the field \code{REF} as a side effect.
}
\examples{
head(index_dosing_interval(examplinib_fe_nif))
head(index_dosing_interval(examplinib_poc_nif))
head(index_dosing_interval(examplinib_poc_min_nif))
}
