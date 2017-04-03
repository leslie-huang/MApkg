#' Function to generate summary statistics for a LIWC matrix
#'
#' @param df Dataframe of texts
#' @param dict LIWC dictionary
#'
#' @keywords summary statistics, LIWC
#' @export
#' @examples liwc_summary_stats(FARC, spanish_dict)

liwc_summary_stats <- function(df, dict) {
  liwc_results <- LIWCalike::liwcalike(df$text, dict)
  num_docs <- length(liwc_results[, 1])

  wc_mean <- mean(as.numeric(liwc_results$WC))
  wc_sd <- stats::sd(as.numeric(liwc_results$WC))
  neg_mean <- mean(as.numeric(liwc_results$EmoNeg))
  neg_sd <- stats::sd(as.numeric(liwc_results$EmoNeg))
  pos_mean <- mean(as.numeric(liwc_results$EmoPos))
  pos_sd <- stats::sd(as.numeric(liwc_results$EmoPos))

  results <- c(num_docs,
               wc_mean,
               wc_sd,
               neg_mean, neg_sd,
               pos_mean, pos_sd
  )
  return(results)
}
