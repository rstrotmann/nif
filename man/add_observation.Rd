% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_nif.R
\name{add_observation}
\alias{add_observation}
\title{Add an observation to a nif object}
\usage{
add_observation(
  nif,
  sdtm,
  domain,
  testcd,
  analyte = NULL,
  parent = NULL,
  DTC_field = NULL,
  DV_field = NULL,
  TESTCD_field = NULL,
  cmt = NULL,
  observation_filter = {
     TRUE
 },
  subject_filter = {
     !ACTARMCD \%in\% c("SCRNFAIL", "NOTTRT")
 },
  NTIME_lookup = NULL,
  silent = FALSE,
  cleanup = TRUE
)
}
\arguments{
\item{nif}{A nif object.}

\item{sdtm}{A sdtm object. Needs at least the 'DM' and 'VS' domains, and the
domain the observations come from.}

\item{domain}{The domain as character.}

\item{testcd}{The xxTESTCD entry that corresponds to the analyte of interest,
as character.}

\item{analyte}{The name for the analyte. Defaults to the 'testcd', if NULL.}

\item{parent}{The name of the parent analyte for the observation as
character.}

\item{DTC_field}{The field to use as the date-time code for the observation.
Defaults to the two-character domain name followed by 'DTC', if NULL.}

\item{DV_field}{the field to use as the dependent variable. Defaults to the
two-character domain name followed by 'STRESN', if NULL.}

\item{TESTCD_field}{The xxTESTCD field. Defaults to the two-character domain
name followed by 'TESTCD', if NULL.}

\item{cmt}{The compartment for the observation as numeric.}

\item{observation_filter}{The filtering to apply to the observation source
data.}

\item{subject_filter}{The filtering to apply to the DM domain.}

\item{NTIME_lookup}{A data frame with two columns, a column that defines the
custom nominal time information in the target domain (e.g., 'PCELTM'), and
'NTIME'. This data frame is left_join()ed into the observation data frame
to provide the NTIME field.}

\item{silent}{Suppress messages as logical.}

\item{cleanup}{Keep only necessary fields, as logical.}
}
\value{
A nif object.
}
\description{
Add an observation to a nif object
}
