% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{recode_sex}
\alias{recode_sex}
\title{Re-code SEX field in a data frame}
\usage{
recode_sex(obj)
}
\arguments{
\item{obj}{The data.frame containing a SEX field}
}
\value{
The output data frame with SEX coded as:
\itemize{
\item 0: "M", "男", "0"
\item 1: "F", "女", "1"
\item NA: Any other values (with warning)
}
}
\description{
Re-code SEX field in a data frame
}
\keyword{internal}
