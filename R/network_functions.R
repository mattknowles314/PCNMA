gen_network_data <- function(data, ref){
  data %>% select(Study, Total.Patients, Treatment.1, Treatment.2, `N.(Trt1)`, `N.(Trt2)`, `Median.OS.(Trt.1)`, `Median.OS.(Trt.2)`) %>% 
    rename(N = Total.Patients) %>% 
    pivot_longer(cols = 3:4, values_to = "Treatment") %>% 
    mutate(r = ifelse(Treatment == ref, `N.(Trt1)`, `N.(Trt2)`)) %>% 
    mutate(Median.OS = ifelse(Treatment == ref, `Median.OS.(Trt.1)`, `Median.OS.(Trt.2)`)) %>% 
    select(-c(name, `N.(Trt1)`, `N.(Trt2)`, `Median.OS.(Trt.1)`, `Median.OS.(Trt.2)`))
}

gen_network <- function(net_data, ref){
  net <- multinma::set_agd_arm(
    net_data,
    study = Study,
    trt = Treatment,
    n = N,
    r = r,
    trt_ref = ref)
}

plot_network <- function(network, ...){
  ggraph(as.igraph(network), layout = "kk") +
    geom_edge_fan(aes(edge_width = .data$.nstudy)) +
    scale_edge_width_continuous("Number of Studies") +
    geom_node_point(aes()) +
    geom_node_text(aes(label = .data$name))
}
