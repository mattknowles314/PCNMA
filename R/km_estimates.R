#' Generate KM estimates
#'
#' Generate Kaplan-Meier estimates from a TTE object. Uses `ggsurvfit::survfit2` to generate KM estimates. Only requires strata to be specified.
#'
#' @param TTE A TTE dataframe
#'
#' @returns A [PCNMA::km_obj] object
#' 
#' @export
km_estimates <- function(TTE, strata = "1"){
  out <- ggsurvfit::survfit2(formula = .gen_surv_formula(strata), data = TTE)
  class(out) <- c("km_obj", class(obj))
  out
}
