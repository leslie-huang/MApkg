#' Function to coarsen sentiment data into binary states
#'
#'
#' @param df Dataframe containing sentiment levels
#' @param date Name of var that contains date var for sorting
#' @param side_var Name of var in df that contains side
#' @param side1 Name to assign side 1
#' @param side2 Name to assign side 2
#' @param state_var Name to assign the state variable
#' @param sentiment_var Name of the var in df that contains sentiment level
#'
#' @keywords coarsen, sentiment
#' @export
#' @examples generate_states(mnl_df, "date", "side", "FARC", "govt", "state_t", "sentiment_level")


generate_states <- function(df, date, side_var, side1, side2, state_var, sentiment_var) {

  # first sort the data by date -- OK if it's not regular time series!

  df <- df[order(as.Date(df[date], format = "%Y-%m-%d")), ]

  df[state_var] <- NA

  # fill in the appropriate states
  for (i in 1:length(df[, 1])) {

    # Divide side1 into states 1 and 2
    if (df[side_var][i, ] == side1) {

      if (df[sentiment_var][i, ] == 0) {
        df[state_var][i, ] <- 1
      }

      else {
        df[state_var][i, ] <- 2
      }
    }

    # Divide side2 into states 3 and 4
    if (df[side_var][i, ] == side2) {

      if (df[sentiment_var][i, ] == 0) {
        df[state_var][i, ] <- 3
      }

      else {
        df[state_var][i, ] <- 4
      }

    }
  }


  return(df)
}
