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

.get_attribute <- function(Model, attribute){
  return(Model[[attribute]])
}
