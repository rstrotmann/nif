% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sdtm_class.R
\name{validate_sdtm}
\alias{validate_sdtm}
\title{Check whether sdtm object is compliant with SDTM standard}
\usage{
validate_sdtm(sdtm, silent = NULL)
}
\arguments{
\item{sdtm}{SDTM object.}

\item{silent}{Suppress optional messages, as logical. Defaults to global
nif_options if NULL.}
}
\value{
Invisibly returns TRUE if validation passes, or stops with an error
if required columns are missing.
}
\description{
Check whether sdtm object is compliant with SDTM standard
}
