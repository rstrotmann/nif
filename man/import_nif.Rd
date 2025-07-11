% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nif_load.R
\name{import_nif}
\alias{import_nif}
\title{Import nif file}
\usage{
import_nif(
  filename,
  ...,
  format = NULL,
  delimiter = ",",
  no_numeric = c("USUBJID", "STUDYID"),
  silent = NULL
)
}
\arguments{
\item{filename}{Filename as character.}

\item{...}{Renaming terms as function.}

\item{format}{The input data format, can be 'csv' or 'fixed_width', or NULL
(default) to automatically determine the format.}

\item{delimiter}{Delimiter character.}

\item{no_numeric}{Fields that will not be converted to numeric.}

\item{silent}{Suppress message output, as logical.}
}
\value{
A nif object.
}
\description{
Import nif file
}
