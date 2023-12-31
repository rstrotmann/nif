% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nif_class.R
\name{subject_info.nif}
\alias{subject_info.nif}
\title{Subject information}
\usage{
\method{subject_info}{nif}(obj, id)
}
\arguments{
\item{obj}{A NIF object.}

\item{id}{The USUBJID or ID.}
}
\description{
This function summarizes baseline information for a subject or a list of
subjects, including sex, age, weight, height, BMI, ACTARMCD, analytes, IMPs
and baseline covariates as available.
}
\details{
The output is an object of the class 'subject_info' which is a wrapper for
the named list of the above.

The field \code{administrations} is not printed automatically but can be accessed
as list item (see example).
}
\examples{
subject_info(examplinib_poc_nif, 1)
unclass(subject_info(examplinib_poc_nif, 1))
subject_info(examplinib_poc_nif, 1)$administrations

}
