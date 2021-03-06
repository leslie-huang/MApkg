% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert_fitted_data.R
\name{convert_fitted_data}
\alias{convert_fitted_data}
\title{Convert fitted data}
\usage{
convert_fitted_data(fitted_vals, date_df)
}
\arguments{
\item{fitted_vals}{Vector of fitted values}

\item{date_df}{Dataframe that has date in same order in variable $date}
}
\description{
This function combines a vector of fitted values and a vector of dates and melts it into the correct format for plotting.
}
\examples{
convert_fitted_data(mnl_fitted_ts, sentiment_states_wide)
}
\keyword{fitted,}
\keyword{melt,}
\keyword{timeseries}
