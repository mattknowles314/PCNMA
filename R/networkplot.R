library(dplyr)
library(tidyr)
library(ggraph)
library(igraph)
library(ggplot2)

source("R/utils.R")
source("R/network_functions.R")

data <- read_def("Data/DE.xlsx")
ref <- "GEM"
df <- gen_network_data(data, ref)

OS_network <- df %>% 
  filter(Reported.OS == 1) %>% 
  gen_network(ref)

PFS_network <- df %>% 
  filter(Reported.PFS == 1) %>% 
  gen_network(ref)

plot(OS_network, layout = "star")
plot(PFS_network, layout = "star") 

