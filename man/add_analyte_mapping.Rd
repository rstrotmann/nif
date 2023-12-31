% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sdtm_class.R
\name{add_analyte_mapping}
\alias{add_analyte_mapping}
\title{Attach a treatment-analyte mapping to an SDTM object}
\usage{
add_analyte_mapping(obj, extrt = "", pctestcd = "")
}
\arguments{
\item{obj}{A SDTM object.}

\item{extrt}{The treatment as defined in EX.}

\item{pctestcd}{The analyte as defined in PC.}
}
\description{
In some studies, multiple drugs are co-administered, and there may be plasma
concentration data from different parent drugs.
In order to appropriately correlate observations with administrations, the
\code{\link[=make_nif]{make_nif()}} algorithm needs to know which analyte (PCTESTCD within PC)
belongs to which drug (EXTRT within EX). If the respective names differ,
add_treatment_mapping() can be used to attach this information to the SDTM
object. Multiple mappings may be needed.
}
\examples{
sdtm_object <- add_analyte_mapping(examplinib, "EXAMPLINIB", "RS2023")

}
\seealso{
\code{\link[=make_nif]{make_nif()}}
}
