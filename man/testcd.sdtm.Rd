% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sdtm_class.R
\name{testcd.sdtm}
\alias{testcd.sdtm}
\title{Extract TESTCD fields by domain from a sdtm object}
\usage{
\method{testcd}{sdtm}(obj)
}
\arguments{
\item{obj}{A sdtm object.}
}
\value{
A data frame with columns DOMAIN and TESTCD. Returns an empty data frame
if no TESTCD columns are found.
}
\description{
Extract TESTCD fields by domain from a sdtm object
}
