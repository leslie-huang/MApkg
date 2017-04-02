#' Function to generate summary statistics from a LIWC matrix
#'
#' @param df Dataframe of texts
#'
#' @keywords summary statistics
#' @export
#' @examples liwc_summary_stats(FARC)

liwc_summary_stats <- function(df) {
  liwc_results <- liwcalike(df$text, spanish_dict)
  num_docs <- length(liwc_results[, 1])

  wc_mean <- mean(as.numeric(liwc_results$WC))
  wc_sd <- sd(as.numeric(liwc_results$WC))
  neg_mean <- mean(as.numeric(liwc_results$EmoNeg))
  neg_sd <- sd(as.numeric(liwc_results$EmoNeg))
  pos_mean <- mean(as.numeric(liwc_results$EmoPos))
  pos_sd <- sd(as.numeric(liwc_results$EmoPos))

  results <- c(num_docs,
               wc_mean,
               wc_sd,
               neg_mean, neg_sd,
               pos_mean, pos_sd
  )
  return(results)
}
