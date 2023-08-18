#' Plot AIC Bar plot
#' 
#' @param model A [PCNMA::fitted_distribution] object
#'
#' @export
plot_aic <- function(model){
  p <- ggplot(model, aes(x = reorder(Distribution, AIC), y = AIC)) +
    geom_bar(stat = "identity", fill = colours$blue) +
    labs(
      x = "Distribution",
      y = "AIC Score"
    ) +
  theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1))
  p
}