library(dplyr)
library(tidyr)
library(ggraph)
library(igraph)

source("R/utils.R")
source("R/network_functions.R")

data <- read_def("Data/DE.xlsx")
ref <- "GEM"
df <- gen_network_data(data, ref)
net <- gen_network(df, ref)
plot(net)
