#' Function to generate a base graph by actor
#'
#' This function generates a time series graph for 1 actor
#' @param data Dataframe of LIWC estimates
#' @param title Title for the graph
#'
#' @keywords ggplot, graph
#' @export
#' @examples generate_base_actor(FARC_results, "FARC: Loessed % of Document Contributed by Sentiment Type")

generate_base_actor <- function(data, title) {
  data$id <- rep(1:length(data[,1]))
  melted_data <- melt(data, id = c("date", "id"), variable.name = "sentiment_type", value.name = "sentiment_measure")

  graph <- ggplot(data = melted_data, aes(y = sentiment_measure, x = as.Date(date, origin = "1970-01-01"), group = sentiment_type)) +
    geom_smooth(method = "loess", se = FALSE, aes(linetype = sentiment_type, color = sentiment_type)) +
    labs(
      x = "Date",
      y = "Loessed % of Document") +
    scale_x_date(date_minor_breaks = "1 month",
                 limits = c(as.Date("2012-06-01", "%Y-%m-%d"), NA)) +
    scale_colour_manual(name = "Sentiment",
                        labels = c("Neg. emotion", "Pos. emotion"),
                        values = c("grey30", "black")) +
    scale_linetype_manual(name = "Sentiment",
                          labels = c("Neg. emotion", "Pos. emotion"),
                          values = c(1,2)) +
    ggtitle(title) +
    theme_bw()

  return(graph)
}
