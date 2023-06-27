plot_network <- function(network, ...){
  ggraphh::ggraph(igraph::as.igraph(network), layout = "kk") +
    ggraph::geom_edge_fan(ggraph::aes(edge_width = .data$.nstudy)) +
    ggraph::scale_edge_width_continuous("Number of Studies") +
    ggraph::geom_node_point(aes()) +
    ggraph::geom_node_text(aes(label = .data$name))
}