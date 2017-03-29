#' Function to get raw LIWC measures for a corpus of text
#'
#' This function gets LIWC values for a corpus of text and returns them in a formatted dataframe.
#' @param df Dataframe where text is in column "text" and date is in column "date"
#' @param LIWC_dict Name of LIWC dictionary to use
#'
#' @keywords LIWC
#' @export
#' @examples
#' extract_liwc_values(FARC, spanish_dict)

extract_liwc_values <- function(df, LIWC_dict) {
  # run liwc
  liwc_results <- LIWCalike::liwcalike(df$text, LIWC_dict)

  # get date metadata from the corpus df
  df_dates <- dplyr::select(df, date)

  # extract the measures we want
  liwc_results <- dplyr::select(liwc_results, EmoPos, EmoNeg)

  # make the dataframe
  results_df <- data.frame(cbind(sapply(liwc_results, function(x) {as.numeric(x)})))
  results_df$date <- as.Date(df_dates[[1]], "%Y-%m-%d") # make sure to get the dates into Date format
  return(results_df)
}
