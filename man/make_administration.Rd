% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nif_administrations.R
\name{make_administration}
\alias{make_administration}
\title{Compile administration data frame}
\usage{
make_administration(
  sdtm,
  extrt,
  analyte = NA,
  cmt = 1,
  subject_filter = "!ACTARMCD \%in\% c('SCRNFAIL', 'NOTTRT')",
  cut_off_date = NULL,
  keep = "",
  silent = NULL
)
}
\arguments{
\item{sdtm}{A sdtm object.}

\item{extrt}{The EXTRT for the administration, as character.}

\item{analyte}{The name of the analyte as character.}

\item{cmt}{The compartment for the administration as numeric.}

\item{subject_filter}{The filtering to apply to the DM domain, as string,}

\item{cut_off_date}{The data cut-off date as Posix date-time.}

\item{keep}{Columns to keep after cleanup, as character.}

\item{silent}{Suppress messages, defaults to nif_option standard, if NULL.}
}
\value{
A data frame.
}
\description{
Compile administration data frame
}
\details{
A discussion on EC vs EX is provided \href{https://www.cdisc.org/kb/ecrf/exposure-collected#:~:text=In\%20the\%20SDTMIG\%2C\%20the\%20Exposure,data\%20collected\%20on\%20the\%20CRF.}{here}
}
\section{Time imputations and filtering}{
The following time imputations and filters are applied in the given
order:
\subsection{1. \code{\link[=impute_exendtc_to_rfendtc]{impute_exendtc_to_rfendtc()}}}{

If EXENDTC is missing in the last administration episode for a given subject,
it is replaced with DM.RFENDTC, if available.
}

\subsection{2. \code{\link[=filter_EXSTDTC_after_EXENDTC]{filter_EXSTDTC_after_EXENDTC()}}}{

Administration episodes in which EXSTDTC is after EXENDT are deleted from the
data set.
}

\subsection{3. \code{\link[=impute_exendtc_to_cutoff]{impute_exendtc_to_cutoff()}}}{

If in the last administration episode per subject and treatment, EXENDTC is
missing, for example because the treatment is still ongoing at the time of
the SDTM generation, EXENDTC is replaced with the cut-off date.
}

\subsection{4. \code{\link[=impute_missing_exendtc]{impute_missing_exendtc()}}}{

If in any further episode, EXENDTC is missing, it is replaced with the day
before the subsequent administration episode start (EXSTDTC). It should be
understood that this reflects a rather strong assumption, i.e., that the
treatment was continued into the next administration episode. This imputation
therefore issues a warning that cannot be suppressed.
}

\subsection{5. Expand administration episodes}{

All administration episodes, i.e., the intervals between EXSTDTC and EXENDTC
for a given row in EX, are expanded into a sequence of rows with one
administration day per row. The administration times for all rows except for
the last are taken from the time information in EXSTDTD, whereas the time
for the last administration event in the respective episode is taken from the
time information in EXENDTC.

\strong{Development note:} In the present version of the function, once-daily (QD)
dosing is assumed. Multiple-daily dosings are not supported. In future
versions, the dosing frequency provided in \code{EXDOSFRQ} may be taken into
account to adequately handle multiple daily administrations.
}

\subsection{6. \code{\link[=impute_admin_times_from_pcrftdtc]{impute_admin_times_from_pcrftdtc()}}}{

For administration days for which PK sampling events are recorded in PC, the
administration time is taken from PC.PCRFTDTC, if this field is available.

\strong{Development note:} This may be updated in future versions of the function
to work with multiple-daily administrations.
}

\subsection{7. Carry forward time}{

For all administration events per subject and treatment, missing time
information is finally carried forward from available time information.
}
}

\seealso{
\code{\link[=add_administration]{add_administration()}}
}
\keyword{internal}
