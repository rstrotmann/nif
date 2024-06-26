% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{standardize_date_format}
\alias{standardize_date_format}
\title{Convert date fields to POSIX format}
\usage{
standardize_date_format(obj, fields = NULL)
}
\arguments{
\item{obj}{A data frame.}

\item{fields}{Date variable names as character.}
}
\value{
A data frame
}
\description{
Convert date-time code (DTC) variables from the \href{https://w.wiki/8Bzr}{ISO 8601} format used in SDTM (i.e., something like
"2001-01-02T09:59" where date and time are separated by "T") to standard
POSIXct format. The names of the variables to be converted need to be
provided by \code{fields}.
}
