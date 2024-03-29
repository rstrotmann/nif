% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nif_nca.R
\name{dose_lin}
\alias{dose_lin}
\title{Test for dose linearity}
\usage{
dose_lin(nca, parameters = c("aucinf.obs", "cmax"), lower = 0.8, upper = 1.25)
}
\arguments{
\item{nca}{The non-compartmental analysis data.}

\item{parameters}{The NCA parameters to investigate for dose linearity.}

\item{lower}{The lower threshold for Rdnm.}

\item{upper}{the upper threshold for Rdnm.}
}
\value{
A data frame.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}

Currently experimental. Don't use in production!

Using the power model described by
\href{https://doi.org/10.1002/pst.326}{Hummel, 2009}.
In brief, a power model is fitted with

ln(AUC) = mu + beta* ln(dose)

and the 90\% CI of beta compared to the below acceptance criteria, assuming

theta_L of 0.8 and theta_U of 1.25:

(beta_L, beta_U) = ( 1 + ln(theta_L) / ln(r), 1 + lntheta_U) / ln(r) )

with ln(r) the logarithm of the ratio of the highest dose to the lowest dose.
}
