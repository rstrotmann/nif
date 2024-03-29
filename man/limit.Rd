% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_nif.R
\name{limit}
\alias{limit}
\title{Subset nif to rows with DTC before the last individual or global observation}
\usage{
limit(obj, individual = TRUE, keep_no_obs_sbs = FALSE)
}
\arguments{
\item{obj}{A nif object.}

\item{individual}{Apply by ID, as logical.}

\item{keep_no_obs_sbs}{Retain subjects without observations.}
}
\value{
A nif object.
}
\description{
Subset nif to rows with DTC before the last individual or global observation
}
