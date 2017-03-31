#' Function to create a new lead variable from an existing variable
#'
#'
#' @param df Dataframe containing sentiment levels
#' @param date Name of var that contains date var for sorting
#' @param existing_state Variable in df containing current data (at time = t)
#' @param lead_state Name for variable in df to contain data for time = t+1
#'
#' @keywords lag, lead, data
#' @export
#' @examples generate_lead_state(df, "state_t", "state_t_lead")

generate_lead_state <- function(df, date, existing_state, lead_state) {
  df <- df[order(as.Date(df$date, format = "%Y-%m-%d")), ]

  df[lead_state] <- NA

  for (i in 1:length(df[, 1]) -1) {
    df[lead_state][i, ] <- df[existing_state][i+1, ]
  }

  return(df)
}
