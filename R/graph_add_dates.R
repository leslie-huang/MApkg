#' Function to add major dates to a base graph
#'
#' This function adds major agreements to a base graph
#' @param base_graph Ggplot graph to which datelines will be added
#' @param title Title for the graph
#' @param data Dataframe of dates with categorical labels
#' @param group_label Group i.e. categorical variable value to graph
#' @param legend_label Name of group for the graph legend
#'
#' @keywords ggplot, graph
#' @export
#' @examples graph_add_dates(negative_sentiment_graph, "Negative Sentiment and Major Agreements", dates, "major_agree", "Major agreements")

graph_add_dates <- function(base_graph, title, data, group_label, legend_label){
  filtered_data <- data[data["group"] == group_label, ]
  filtered_data["group"] <- legend_label

  graph = base_graph + ggplot2::ggtitle(title) +
    ggplot2::geom_vline(data = filtered_data, mapping = ggplot2::aes(xintercept = as.numeric(date), color = group), linetype = 2)

  return(graph)
}
