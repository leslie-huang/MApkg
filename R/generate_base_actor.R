#' Function to generate a base graph by actor
#'
#' This function generates a time series graph for 1 actor
#' @param data Dataframe of LIWC estimates
#' @param y_var Name of y_var for plotting
#' @param title Title for the graph
#' @param sentiment_measure String of the sentiment measure value
#'
#' @keywords ggplot, graph
#' @export
#' @examples generate_base_actor(FARC_results, "FARC: Loessed % of Document Contributed by Sentiment Type", "sentiment_measure")

generate_base_actor <- function(data, title, sentiment_measure) {
  data$id <- rep(1:length(data[,1]))
  melted_data <- reshape2::melt(data, id = c("date", "id"), variable.name = "sentiment_type", value.name = sentiment_measure)

  graph <- ggplot2::ggplot(data = melted_data, ggplot2::aes(y = sentiment_measure, x = as.Date(date, origin = "1970-01-01"), group = sentiment_type)) +
    ggplot2::geom_smooth(method = "loess", se = FALSE, ggplot2::aes(linetype = sentiment_type, color = sentiment_type)) +
    ggplot2::geom_jitter() +
    ggplot2::labs(
      x = "Date",
      y = "Loessed % of Document") +
    ggplot2::scale_x_date(date_minor_breaks = "1 month",
                 limits = c(as.Date("2012-06-01", "%Y-%m-%d"), NA)) +
    ggplot2::scale_colour_manual(name = "Sentiment",
                        labels = c("Neg. emotion", "Pos. emotion"),
                        values = c("grey20", "grey50", "grey70", "black")) +
    ggplot2::scale_linetype_manual(name = "Sentiment",
                          labels = c("Neg. emotion", "Pos. emotion"),
                          values = c(1,2,3,4,5)) +
    ggplot2::ggtitle(title) +
    ggplot2::theme_bw()

  return(graph)
}
