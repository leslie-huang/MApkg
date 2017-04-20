#' Function to generate a base graph by actor
#'
#' This function generates a time series graph of predicted probabilities of states
#' @param data Dataframe of predicted probabilities in wide format
#' @param title Title for the graph
#'@param y_var Name of y_var in the data
#'
#' @keywords ggplot, graph
#' @export
#' @examples generate_graph_predicted(mnl_fitted_ts, "MLE Estimation of Probability of Each State Over Time", "predicted_Pr")

generate_graph_predicted <- function(data, title, y_var) {
  ggplot2::ggplot(data = data, ggplot2::aes(y = get(y_var), x = as.Date(date, origin = "1970-01-01"), group = state)) +
    ggplot2::geom_smooth(method = "loess", se = FALSE, ggplot2::aes(linetype = state, color = state), span = 0.12) +
    labs(
      x = "Date",
      y = "Probability (loessed)") +
    ggplot2::scale_x_date(date_minor_breaks = "1 month",
                 limits = c(as.Date("2012-09-01", "%Y-%m-%d"), NA)) +
    ggplot2::scale_colour_manual(name = "State",
                        labels = c("FARC-low", "FARC-high", "Govt-low", "Govt-high"),
                        values = c("grey30", "grey50", "grey70", "black")) +
    ggplot2::scale_linetype_manual(name = "State",
                          labels = c("FARC-low", "FARC-high", "Govt-low", "Govt-high"),
                          values = c(1, 2, 3, 4)) +
    ggplot2::ggtitle(title) +
    ggplot2::theme_bw()
  
  filename = paste(title, ".eps", collapse = "")
  ggsave(file = filename)
  
  
}
