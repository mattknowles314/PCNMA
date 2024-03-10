#' Run an NMA
#'
#' @param network A [multinma::nma_data] object
#' @param L Character string specifying a likelihood function
#' @param link Character string specifying a link function (defaults to "log")
#' @param ... Other parameters to pass to [multinma::nma]
#'  
#' @export
fit_model <- function(network, effects, iter, seed = 1, chains = 4){
  out <- multinma::nma(
    network = network,
    trt_effects = effects,
    prior_intercept = normal(scale = 100),
    prior_trt = normal(scale = 5),
    iter = iter,
    chains = chains,
    seed = seed
  )
  class(out) <- c("fitted_model", class(out))
  out
}

#' Plots for an NMA model
#'
#' @param model A `PancSurv::fitted_model` obj
#' @param type Type of plot to produce
#'
#'
#' @export
plot.fitted_model <- function(model, type = "trace", pars = parsForStan) {
  if (type == "trace") {
    rstan::traceplot(model[["stanfit"]], pars = pars)
  } 
}