% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nif_class.R
\name{add_obs_per_dosing_interval}
\alias{add_obs_per_dosing_interval}
\title{Add the number of observations per dosing interval}
\usage{
add_obs_per_dosing_interval(obj)
}
\arguments{
\item{obj}{A NIF object.}
}
\value{
Result as NIF object.
}
\description{
This function adds a variable, \code{OPDI}, to the NIF object that indicates the
number of observations per analyte and dosing interval. This field can be
helpful to identify dosing intervals across which rich sampling was
conducted.
}
\examples{
add_obs_per_dosing_interval(examplinib_poc_nif)
add_obs_per_dosing_interval(examplinib_poc_min_nif)
}
