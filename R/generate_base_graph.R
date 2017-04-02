#' Function to generate a base graph of sentiment
#'
#' This function generates a time series graph of 1 type of sentiment x the 3 actors. Includes scatterplot of observed values as well as loess curves.
#' @param y_var Name of the y variable to be plotted (must match column name in the dataframes)
#' @param y_legend Name for the y variable to appear in the legend
#' @param title Title for the graph
#'
#' @keywords ggplot, graph
#' @export
#' @examples generate_base_graph("EmoNeg", "Negative Emotion", "Graph of "Negative Emotion")

generate_base_graph <- function(y_var, y_legend, title){
  ggplot(FARC_results, aes(x = as.Date(date, origin = "1970-01-01"), y = get(y_var), color = "FARC statement")) +
    geom_smooth(method = "loess", se = FALSE, linetype = 2) +
    geom_jitter() +
    geom_point(data = joint_results, aes(x = as.Date(date, origin = "1970-01-01"), y = get(y_var), color = "Joint statement")) +
    geom_smooth(method = "loess", se = FALSE, data = joint_results, aes(x = as.Date(date, origin = "1970-01-01"), y = get(y_var), color = "Joint statement"), linetype = 1) +
    geom_point(data = govt_results, aes(x = as.Date(date, origin = "1970-01-01"), y = get(y_var), color = "Govt statement")) +
    geom_smooth(method = "loess", se = FALSE, data = govt_results, aes(x = as.Date(date, origin = "1970-01-01"), y = get(y_var), color = "Govt statement"), linetype = 3) +
    labs(
      x = "Date",
      y = y_legend,
      color = "Legend") +
    scale_x_date(date_minor_breaks = "1 month",
                 limits = c(as.Date("2012-06-01", "%Y-%m-%d"), NA)) +
    ggtitle(title)
}
