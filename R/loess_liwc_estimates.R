#' Function to calculate loess point estimates of LIWC estimates
#'
#' This function returns a dataframe of loessed point estimates of raw LIWC measures.
#' @param liwc_results Dataframe of LIWC values, where "date" is the last observation
#'
#' @keywords LIWC, loess
#' @export
#' @examples
#' loess_liwc_estimates(FARC_raw)
#'

loess_liwc_estimates <- function(liwc_results) {
  date <- as.Date(liwc_results$date, origin = "1970-01-01")

  results_df <- data.frame(cbind(sapply(liwc_results[ , 1:(length(liwc_results) - 1)], function(x) { loess(x ~ as.numeric(liwc_results$date), control = loess.control(surface = "direct"))$y})))

  results_df$date <- date

  return(results_df)
}
