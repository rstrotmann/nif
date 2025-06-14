% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sdtm_mappings.R
\name{add_time_mapping}
\alias{add_time_mapping}
\title{Attach a time mapping to an sdtm object}
\usage{
add_time_mapping(obj, ...)
}
\arguments{
\item{obj}{The SDTM object.}

\item{...}{Mappings in the form '"\if{html}{\out{<PCTPT>}}"=\if{html}{\out{<NTIME>}}' with multiple mappings
separated by commas. \if{html}{\out{<PCTPT>}} corresponds to the value in the PCTPT fields,
and NTIME corresponds to the nominal time in hours.}
}
\value{
The SDTM object
}
\description{
The nominal time of observations (e.g., \code{PCTPT}) is not required
to follow a strict format and is in most cases provided as a composite
string. This function can be used to explicitly define the nominal
observation times (in hours) for the values of, e.g., \code{PCTPT}.
}
\examples{
sdtm_object <- add_time_mapping(examplinib_fe,
  "PREDOSE" = 0,
  "HOUR 0.5" = 0.5,
  "HOUR 1" = 1,
  "HOUR 1.5" = 1.5,
  "HOUR 2" = 2,
  "HOUR 3" = 3,
  "HOUR 4" = 4,
  "HOUR 6" = 6,
  "HOUR 8" = 8,
  "HOUR 10" = 10,
  "HOUR 12" = 12,
  "HOUR 24" = 24,
  "HOUR 48" = 48,
  "HOUR 72" = 72,
  "HOUR 96" = 96,
  "HOUR 144" = 144,
  "HOUR 168" = 168
)
}
\seealso{
\code{\link[=suggest]{suggest()}}
}
\keyword{internal}
