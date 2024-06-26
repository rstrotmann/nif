% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_nif.R
\name{nif_auto}
\alias{nif_auto}
\title{Automatically generate a nif object with pharmacokinetic observations}
\usage{
nif_auto(sdtm, bl_creat = TRUE, bl_odwg = TRUE, keep = NULL)
}
\arguments{
\item{sdtm}{The source SDTM object.}

\item{bl_creat}{Include baseline creatinine, creatinine clearance and renal
function class fields.}

\item{bl_odwg}{Include baseline ODWG hepatic function class.}

\item{keep}{Columns to keep, as character.}
}
\value{
A nif object.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}
}
\details{
Automatically generate a nif object from SDTM data. All treatments and
analytes must be defined in the \code{treatment_mapping} and \code{metabolite_mapping}
objects that are attached to the sdtm object.
}
\examples{
nif_auto(examplinib_sad)
}
\seealso{
\code{\link[=add_analyte_mapping]{add_analyte_mapping()}}

\code{\link[=add_metabolite_mapping]{add_metabolite_mapping()}}
}
