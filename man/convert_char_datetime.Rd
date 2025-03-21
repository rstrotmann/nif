% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nif_load.R
\name{convert_char_datetime}
\alias{convert_char_datetime}
\title{Convert character vector to DTC}
\usage{
convert_char_datetime(x, min_prob = 0.9, silent = NULL)
}
\arguments{
\item{x}{The input as character.}

\item{min_prob}{The minimal fraction of values to correctly parse as numeric.}

\item{silent}{No message output.}
}
\value{
POSIXct.
}
\description{
Convert character vector to DTC
}
