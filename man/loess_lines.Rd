% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/loess_lines.R
\name{loess_lines}
\alias{loess_lines}
\title{Function to generate loess lines from point estimates}
\usage{
loess_lines(liwc_results)
}
\arguments{
\item{liwc_results}{Dataframe with loessed point estimates and "date" as the last column}
}
\description{
This function returns a list of loessed lines
}
\examples{
loess_lines(FARC_results)

}
\keyword{LIWC,}
\keyword{loess}
