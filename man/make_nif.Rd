% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_nif_deprecated.R
\name{make_nif}
\alias{make_nif}
\title{Make a NIF object from SDTM-formatted data}
\usage{
make_nif(
  sdtm_data,
  spec = NULL,
  silent = FALSE,
  truncate_to_last_observation = TRUE,
  truncate_to_last_individual_obs = FALSE,
  use_pctptnum = FALSE,
  analyte_cmt_mapping = NULL
)
}
\arguments{
\item{sdtm_data}{A \code{sdtm} object, i.e., essentially a list of SDTM domains as
data tables. Typically, the SDTM data are loaded using \code{\link[=read_sdtm_sas]{read_sdtm_sas()}} or
\code{\link[=read_sdtm_xpt]{read_sdtm_xpt()}}. As a minimum, the following SDTM domains are needed: DM,
VS, PC and EX.}

\item{spec}{The sample specimen for the PC data as string (e.g., "BLOOD",
"PLASMA", "URINE", "FECES"). When spec is NULL (default), the most likely
specimen is selected.}

\item{silent}{Switch to disable message output.}

\item{truncate_to_last_observation}{Boolean to indicate whether the data set
should be truncated to the last observation. In this case, administrations
after the last observation time point will deleted. The default is 'TRUE'.}

\item{truncate_to_last_individual_obs}{Boolean to indicate whether
observations should be truncted to the last individual observation.}

\item{use_pctptnum}{Boolean to indicate whether to derive nominal time
('NTIME') from 'PCTPTNUM'.}

\item{analyte_cmt_mapping}{The analyte-compartment association as data frame
with the columns 'ANALYTE' and 'CMT'.}
}
\value{
A NIF object.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}}

This function creates a basic NONMEM input file (NIF) data set from
SDTM-formatted clinical study data following the conventions summarized in
\href{https://doi.org/10.1002/psp4.12404}{Bauer, CPT Pharmacometrics Syst. Pharmacol. (2019)}. For a more in-depth tutorial,
see \code{vignette("nif-vignette")}.
}
\section{Imputations}{
 Subjects with administration but no observations for
the respective analyte are deleted from the data set. For further
imputations, see \code{vignette("nif-imputations")}.
}

\section{Output fields}{

\itemize{
\item \code{ID} Subject identification number
\item \code{TIME} Recorded time of administration or observation events in hours
relative to the first individual event.
\item \code{AMT} Dose administered for dosing record, or zero for observations.
\item \code{DOSE} Dose in mg for administrations and post-dose observations.
\item \code{DV} The dependent variable, i.e., observed concentration, or zero for
administration records, in mg/l.
\item \code{LNDV} The natural Log of DV.
\item \code{RATE} Rate of infusion of drug or zero if drug is given as a bolus.
\item \code{MDV} One for missing DV, else zero.
\item \code{EVID} Event ID: 0 for observations, 1 for administrations.
\item \code{CMT} Pharmacokinetic compartment. Will be set to 1 for administrations
and 2 for observations. Should be changed afterwards, if needed.
\item \code{DTC} The date-time of the data record.
\item \code{FIRSTDTC} Date and time of first event per subject. This field is used
internally for the calculation of \code{TIME}. Although it is not needed for
NONMEM analysis, it is provided for subsequent NIF file building steps,
e.g., addition of further time-dependent endpoints.
\item \code{FIRSTADMINDTC} The date-time of the first administration of the
respective parent drug for the respective subject.
\item \code{FIRSTTRTDTC} The date-time of the first administration of any parent
drug for the respective subject.
\item \code{ANALYTE} The analyte or drug in the data record.
\item \code{TRTDY} The treatment day, i.e., the relative day after the first
treatment for the respective subject.
}
}

\seealso{
\code{\link[=summary]{summary()}}

\code{\link[=plot.nif]{plot.nif()}}

\code{\link[=write_nif]{write_nif()}}
}
\keyword{internal}
