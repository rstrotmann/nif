% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nif_nca.R
\name{nca_from_pp}
\alias{nca_from_pp}
\title{Generate NCA table from the SDTM.PP domain}
\usage{
nca_from_pp(
  obj,
  sdtm_data,
  analyte = NULL,
  keep = NULL,
  group = NULL,
  observation_filter = "TRUE"
)
}
\arguments{
\item{obj}{A nif object.}

\item{sdtm_data}{A stdm data object.}

\item{analyte}{The analyte as character.}

\item{keep}{Column names to keep, as character.}

\item{group}{Grouping variable as character.}

\item{observation_filter}{Observation filter term, as character.}
}
\value{
A data frame.
}
\description{
Generate NCA table from the SDTM.PP domain
}
