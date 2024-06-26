% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_nif_deprecated.R
\name{exclude_exstdtc_after_rfendtc}
\alias{exclude_exstdtc_after_rfendtc}
\title{Filter out EX events after the last dose as specified in RFENDTC}
\usage{
exclude_exstdtc_after_rfendtc(ex, dm, silent = FALSE)
}
\arguments{
\item{ex}{The EX domain as data.frame.}

\item{dm}{The DM domain as data.frame}

\item{silent}{Switch to disable message output.}
}
\value{
The modified EX domain as data.frame.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}}
}
\keyword{internal}
