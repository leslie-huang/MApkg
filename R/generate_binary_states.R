#' Function to calculate binary sentiment level (low or high)
#'
#' This function takes LIWC point estimates (can be loessed) based on a measure from Pennebaker et al.
#' Sentiment = high if 2 * % negative emotion is less than or equal to % positive emotion
#'
#' @param df Dataframe of LIWC estimates
#' @param side Side string
#'
#' @keywords binary_sentiment
#' @export
#' @examples
#' generate_binary_states(df, side)

generate_binary_states <- function(df, side) {

  df["sentiment_level"] <- as.numeric(2 * df$EmoNeg <= df$EmoPos)

  df["side"] <- side

  return(df)

}
