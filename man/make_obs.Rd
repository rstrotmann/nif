% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_nif_deprecated.R
\name{make_obs}
\alias{make_obs}
\title{Make observation data set from PC}
\usage{
make_obs(
  pc,
  drug_mapping,
  time_mapping = NULL,
  spec = NULL,
  silent = FALSE,
  use_pctptnum = FALSE
)
}
\arguments{
\item{pc}{The SDTM PC domain as a data.frame.}

\item{drug_mapping}{The drug mapping as data frame.}

\item{time_mapping}{The time mapping.}

\item{spec}{The specimen to be represented in the NIF object as string (e.g.,
"BLOOD", "PLASMA", "URINE", "FECES"). When spec is an empty string (""),
which is the default setting, the most likely specimen, i.e., "BLOOD" or
"PLASMA" is selected, depending what is found in the PC data.}

\item{silent}{Switch to disable message output.}

\item{use_pctptnum}{Use PCTPTNUM as nominal time.}
}
\value{
A data frame with individual observations with certain NONMEM input
variables set
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}}

This function creates an observation data frame from PC SDTM data.
}
\details{
Nominal time is either derived from \code{PCTPTNUM} (if
\code{use_pctptnum=TRUE}), or from \code{PCELTM} (the relative nominal time). Both
are permissible fields per the CDISC specification and may be absent from
the clinical data. In contrast to \code{PCTPOTNUM}, \code{PCELTM} follows a defined
format, i.e., the \href{https://w.wiki/8Bzr}{ISO 8601} specification for time
durations. Note that the DV is converted into mg/l assuming that PCSTRESN
is provided in mg/ml!
}
\seealso{
\code{\link[=add_time_mapping]{add_time_mapping()}}
}
\keyword{internal}
