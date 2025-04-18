% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{find_duplicates}
\alias{find_duplicates}
\title{Find duplicate rows in a data frame}
\usage{
find_duplicates(
  df,
  fields = NULL,
  count_only = FALSE,
  return_all_cols = TRUE,
  additional_cols = NULL
)
}
\arguments{
\item{df}{A data frame to check for duplicates}

\item{fields}{A character vector of field names to check for duplicates. If NULL,
defaults to c("USUBJID", "TIME", "ANALYTE") for NIF data.}

\item{count_only}{Logical indicating whether to return only the count of duplicates (default: FALSE)}

\item{return_all_cols}{Logical indicating whether to return all columns from the original data frame (default: TRUE)}

\item{additional_cols}{Character vector of additional columns to include in the output when return_all_cols is FALSE}
}
\value{
A data frame containing the duplicate rows and their counts, or just the count if count_only is TRUE
}
\description{
This function identifies duplicate rows in a data frame based on specified fields.
It returns a data frame containing the duplicate rows and their counts.
}
