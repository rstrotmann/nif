% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sdtm_class.R
\name{testcd}
\alias{testcd}
\title{Extract TESTCD fields by domain from a sdtm object}
\usage{
testcd(obj, domain = NULL)
}
\arguments{
\item{obj}{A sdtm object.}

\item{domain}{Domains to select, as character. Defaults to all domains, if
NULL.}
}
\value{
A data frame with columns DOMAIN and TESTCD. Returns an empty data
frame if no TESTCD columns are found.
}
\description{
Extract TESTCD fields by domain from a sdtm object
}
