library(flexsurv)
library(dplyr)

dists <- c("exponential", 
           "gengamma",
           "weibull",
           "lognormal",
           "gamma",
           "gengamma")

models <- fit_distribution(dists, IPD_GemOS_Colluci)