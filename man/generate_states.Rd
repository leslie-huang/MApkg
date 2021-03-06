% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_states.R
\name{generate_states}
\alias{generate_states}
\title{Function to coarsen sentiment data into binary states}
\usage{
generate_states(df, date, side_var, side1, side2, state_var, sentiment_var)
}
\arguments{
\item{df}{Dataframe containing sentiment levels}

\item{date}{Name of var that contains date var for sorting}

\item{side_var}{Name of var in df that contains side}

\item{side1}{Name to assign side 1}

\item{side2}{Name to assign side 2}

\item{state_var}{Name to assign the state variable}

\item{sentiment_var}{Name of the var in df that contains sentiment level}
}
\description{
Function to coarsen sentiment data into binary states
}
\examples{
generate_states(mnl_df, "date", "side", "FARC", "govt", "state_t", "sentiment_level")
}
\keyword{coarsen,}
\keyword{sentiment}
