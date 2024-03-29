% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sdtm_synthesis.R
\name{synthesize_crea}
\alias{synthesize_crea}
\title{Synthesize baseline serum creatinine for fictional subjects}
\usage{
synthesize_crea(dm, crea_method = crea_mdrd)
}
\arguments{
\item{dm}{The DM domain as data frame.}

\item{crea_method}{The creatinine calculation function as function reference.
Can currently be \code{crea_mdrd} or \code{crea_raynaud}.}
}
\value{
A DM domain with additional fields as data frame.
}
\description{
Synthesize baseline serum creatinine for fictional subjects
}
\details{
This function makes use of empirical eGFR data from a study in
non-diseased Caucasian subjects as published by \href{https://doi.org/10.1038/sj.ki.5002374}{Wetzels et al.}, Table 1. In this study, eGFR
were calculated by the Modification of Diet in Renal Disease (MDRD) method.

The tabulated data is modeled using a generalized linear model based on age
and sex. The model is used to predict the target eGFR (\code{target_egfr}) for the
subjects in DM. An actual eGFR (\code{egfr}) is then generated for each subjects
as a random sample from a normal distribution with the target eGFR as mean
and a standard deviation of 13 (i.e., the mean SD in the empirical data).

A corresponding baseline creatinine concentration is then calculated from the
actual eGFR using the method specified in \code{crea_method}.
}
\keyword{internal}
