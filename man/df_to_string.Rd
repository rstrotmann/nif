% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{df_to_string}
\alias{df_to_string}
\title{Render data frame object to string}
\usage{
df_to_string(
  df,
  indent = 0,
  n = NULL,
  header = TRUE,
  header_sep = FALSE,
  color = FALSE,
  show_none = FALSE,
  na_string = "NA"
)
}
\arguments{
\item{df}{The data.frame to be rendered}

\item{indent}{Indentation level, as numeric.}

\item{n}{The number of lines to be included, or all if NULL.}

\item{header}{Boolean to indicate whether the header row is to be included.}

\item{header_sep}{Show separation line after header, as logical.}

\item{color}{Print headers in grey as logical.}

\item{show_none}{Show empty data frame as 'none', as logical.}

\item{na_string}{String to use for NA values. Defaults to "NA".}
}
\value{
The output as string.
}
\description{
This function renders a data.frame into a string similar to its
representation when printed without line numbers
}
\keyword{internal}
