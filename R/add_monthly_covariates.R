#' Function to add covariate data to a dataframe
#'
#' This function performs a merge of monthly covariate data into a dataframe with day-level observations. E.g. I have multiple observations from January 2012 and I want to add the monthly January covariate data to each of those observations.
#' @param target_df Target dataframe with day-level observations, where "date" is the last column
#' @param monthly_data Dataframe of covariates, recorded on a monthly basis on the 1st day of each month, where "date" is the last column
#'
#' @keywords monthly_covariates
#' @export
#' @examples add_monthly_covariates(FARC_results, monthly_viol)

add_monthly_covariates <- function(target_df, monthly_data) {
  dates <- df["date"]

  covariate_names <- colnames(monthly_data)[1:(length(monthly_data) - 1)] # exclude the last column, date
  num_of_covars <- length(covariate_column_names)

  # add these columns to the dataframe with NA
  df[, covariate_names] <- NA

  # populate these columns: for each observation in the target df, construct a "1st of the month" date to get the monthly covariate data
  for (i in 1:length(dates[[1]])) {

    date <- dates[i, 1] # current date
    year <- format(date, "%Y") # we only need year and month to get a match for the monthly data
    month <- format(date, "%m")
    monthly_date <- as.Date(paste(year, month, "01", sep = "-")) # monthly data is in the format of "1st of the month"

    # get the observation in question from the covariate dataset
    covar_data <- filter(monthly_data, date == monthly_date)

    # populate the observation dataset
    for (j in 1:num_of_covars) {
      df[as.character(covariate_names[j])][i, 1] <- as.numeric(covar_data[j])
    }

  }

  return(df)
}
