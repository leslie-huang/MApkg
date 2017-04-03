#' Convert fitted data
#'
#' This function combines a vector of fitted values and a vector of dates and melts it into the correct format for plotting.
#' @param fitted_vals Vector of fitted values
#' @param date_df Dataframe that has date in same order in variable $date
#'
#' @keywords melt, fitted, timeseries
#' @export
#' @examples convert_fitted_data(mnl_fitted_ts, sentiment_states_wide)

convert_fitted_data <- function(fitted_vals, date_df) {
  # construct vector of dates with NAs removed

  dates <- date_df[stats::complete.cases(date_df), ]$date

  # cbind it with the fitted values into a time series
  fitted_ts <- data.frame(cbind(fitted_vals, dates))
  colnames(fitted_ts) <- c("Pr_1", "Pr_2", "Pr_3", "Pr_4", "date")
  fitted_ts$date <- as.Date(fitted_ts$date, origin = "1970-01-01")

  # melt it
  fitted_ts$id <- rep(1:length(fitted_ts[,1]))
  fitted_ts <- reshape2::melt(fitted_ts, id = c("date", "id"), variable.name = "state", value.name = "predicted_Pr")

  return(fitted_ts)

}
