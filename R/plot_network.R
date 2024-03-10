#' Plot a network of evidence
#'
#' @param network A network dataset
#'
#'
#' @export
plot_network <- function(network, ...){
  ggraph::ggraph(igraph::as.igraph(network), layout = "kk") +
    ggraph::geom_edge_fan(ggplot2::aes(edge_width = .data$.nstudy)) +
    ggraph::scale_edge_width_continuous("Number of Studies") +
    ggraph::geom_node_point(ggplot2::aes()) +
    ggraph::geom_node_text(ggplot2::aes(label = .data$name))
}