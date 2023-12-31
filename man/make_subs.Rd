% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/synthesize_test_data.R
\name{make_subs}
\alias{make_subs}
\title{Simulate fictional subject disposition data}
\usage{
make_subs(
  studyid = "2023001",
  nsubs = 10,
  nsites = 4,
  screenfailure_rate = 0.25,
  start_date = "2000-12-20 10:00"
)
}
\arguments{
\item{studyid}{The study identifier as string.}

\item{nsubs}{The number of subjects to be simulated.}

\item{nsites}{The number of clinical sites to be simulated.}

\item{screenfailure_rate}{The fraction of subjects to be screeing failures.}

\item{start_date}{The fictional study start date.}
}
\value{
The disposition data for the simulated subjects as data frame.
}
\description{
This function generates a pre-specified number of subjects across different
clinical sites with fictional dates for signing the informed consent and
treatment start.
}
