read_def <- function(path){
  openxlsx::read.xlsx(path, sheet = "DEF", startRow = 2)
}

colours <- list(
  "green" = "#7EBE91",
  "blue" = "#7EABBE",
  "purple" = "#b17ebe"
)

nice_parametric_dists <- list(
  "Exponential" = "exp",
  "Gamma" = "gamma",
  "Generalised Gamma" = "gengamma",
  "Gompertz" = "gompertz",
  "Log-Logistic" = "llogis",
  "Log-normal" = "lnorm",
  "Weibull" = "weibull"
)

nice_parametric_paramlist <- c(
  "rate",
  "tx",
  "shape",
  "rate",
  "tx",
  "mu",
  "sigma",
  "Q",
  "tx",
  "shape",
  "rate",
  "tx",
  "shape", 
  "scale",
  "tx",
  "meanlog",
  "sdlog",
  "tx",
  "shape",
  "scale",
  "tx")

.get_attribute <- function(Model, attribute) {
  return(Model[[attribute]])
}

.gen_surv_formula <- function(strata) {
  as.formula(paste0("Surv(time = time, event = status, type = 'right') ~ ", strata))
}

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