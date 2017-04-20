#' Function to generate a base graph of sentiment
#'
#' This function generates a time series graph of 1 type of sentiment x the 3 actors. Includes scatterplot of observed values as well as loess curves.
#' @param y_var Name of the y variable to be plotted (must match column name in the dataframes)
#' @param y_legend Name for the y variable to appear in the legend
#' @param title Title for the graph
#' @param FARC_results df of FARC LIWC results
#' @param joint_results df of joint LIWC results
#' @param govt_results df of govt LIWC results
#'
#' @keywords ggplot, graph
#' @export
#' @examples generate_base_graph("EmoNeg", "Negative Emotion", "Graph of Negative Emotion", FARC_results, joint_results, govt_results)

generate_base_graph <- function(y_var, y_legend, title, FARC_results, joint_results, govt_results){
  ggplot2::ggplot(FARC_results, ggplot2::aes(x = as.Date(date, origin = "1970-01-01"), y = get(y_var), color = "FARC statement")) +
    ggplot2::geom_smooth(method = "loess", se = FALSE, linetype = 2) +
    ggplot2::geom_jitter() +
    ggplot2::geom_point(data = joint_results, ggplot2::aes(x = as.Date(date, origin = "1970-01-01"), y = get(y_var), color = "Joint statement")) +
    ggplot2::geom_smooth(method = "loess", se = FALSE, data = joint_results, aes(x = as.Date(date, origin = "1970-01-01"), y = get(y_var), color = "Joint statement"), linetype = 1) +
    ggplot2::geom_point(data = govt_results, ggplot2::aes(x = as.Date(date, origin = "1970-01-01"), y = get(y_var), color = "Govt statement")) +
    ggplot2::geom_smooth(method = "loess", se = FALSE, data = govt_results, ggplot2::aes(x = as.Date(date, origin = "1970-01-01"), y = get(y_var), color = "Govt statement"), linetype = 3) +
    ggplot2::labs(
      x = "Date",
      y = y_legend,
      color = "Legend") +
    ggplot2::scale_x_date(date_minor_breaks = "1 month",
                 limits = c(as.Date("2012-06-01", "%Y-%m-%d"), NA)) +
    ggplot2::ggtitle(title)

  filename = paste(title, ".eps", collapse = "")
  ggsave(file = filename, width = 8, height = 5, units = "in")
}
