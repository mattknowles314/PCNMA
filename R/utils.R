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

parsForStan <- c(
  "d[GEM-AXI]",
  "d[GEM-CAP]",
  "d[GEM-CIS]",
  "d[GEM-IRI]",
  "d[GEM-PEM]"
)

.get_attribute <- function(Model, attribute) {
  return(Model[[attribute]])
}

# For use with ggsurvfit
.gen_surv_formula <- function(strata) {
  as.formula(paste("survival::Surv(time = time, event = status, type = 'right') ~ ", paste0(strata, collapse = "+")))
}

# For use with flexsurv
.gen_surv_formula2 <- function(strata) {
  as.formula(paste("survival::Surv(time, status) ~ ", paste0(strata, collapse = "+")))
}

