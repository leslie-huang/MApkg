% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_lead_state.R
\name{generate_lead_state}
\alias{generate_lead_state}
\title{Function to create a new lead variable from an existing variable}
\usage{
generate_lead_state(df, date, existing_state, lead_state)
}
\arguments{
\item{df}{Dataframe containing sentiment levels}

\item{date}{Name of var that contains date var for sorting}

\item{existing_state}{Variable in df containing current data (at time = t)}

\item{lead_state}{Name for variable in df to contain data for time = t+1}
}
\description{
Function to create a new lead variable from an existing variable
}
\examples{
generate_lead_state(df, "date", "state_t", "state_t_lead")
}
\keyword{data}
\keyword{lag,}
\keyword{lead,}
