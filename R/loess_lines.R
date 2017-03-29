#' Function to generate loess lines from point estimates
#'
#' This function returns a list of loessed lines
#' @param liwc_results Dataframe with loessed point estimates and "date" as the last column
#'
#' @keywords LIWC, loess
#' @export
#' @examples
#' loess_lines(FARC_results)
#'

loess_lines <- function(liwc_results) {

  list_models <- list(
    sapply(
      liwc_results[, 1:(length(liwc_results - 1))],
      function(x) {
        loess(x ~ as.numeric(liwc_results$date), control = loess.control(surface = "direct"))
        }
      )
    )

  return(list_models)
}
