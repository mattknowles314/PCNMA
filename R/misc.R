library(ggplot2)
library(dplyr)

boxTid <- function(x, p) {
  ifelse(p == 0, return(log(x)), return(x^p))
}

H <- function(x, P, zeta, j){
  if (P[j] == 0) {return(1)}
  
  if (P[j] != P[j - 1]) {return(boxTid(x, P[j]))}
  
  if (P[j] == P[j - 1]) {return(log(x)*H(x, P, zeta, j - 1))}
}

phi <- function(x, m, P, zeta){
  vals <- c()
  for (j in 2:m) {
    vals[j] <- H(x, P, zeta, j)
  }
  return(vals)
}

