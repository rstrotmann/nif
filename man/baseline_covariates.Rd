% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_nif.R
\name{baseline_covariates}
\alias{baseline_covariates}
\title{Impute missing administration times}
\usage{
baseline_covariates(vs, silent = FALSE)
}
\arguments{
\item{vs}{The VS domain as data frame.}

\item{silent}{Boolean to indicate whether message output should be provided.}
}
\value{
Baseline VS data as wide data frame.
}
\description{
This function fills in administration times from the PCREFDTC field when
available, and carries forward last administration dates
}
\details{
PCRFTDTC is the time for nominal_time=0
(ref: https://www.lexjansen.com/phuse/2016/pp/PP20.pdf)

#@param admin An admin data set as created by 'make_admin()'.
#@param obs An observation data set as created by 'make_obs()'.
#@return An admin data set.
#@import tidyr
#@import dplyr
Extract baseline vital sign covariates from VS
}
\examples{
baseline_covariates(examplinib_sad$vs)

}
