#' Run an NMA
#'
#' @param network A [multinma::nma_data] object
#' @param llhood Character string specifying a likelihood function
#' @param link Character string specifying a link function (defaults to "log")
#' @param ... Other parameters to pass to [multinma::nma]
#'  
#' @export
fit_model <- function(network, effects, seed = 1, chains = 4,
                      llhood = "weibull"){
  out <- multinma::nma(
    network = network,
    trt_effects = effects,
    chains = chains,
    likelihood = llhood,
    seed = seed,
    prior_intercept = normal(0, 100),
    prior_trt = normal(0, 10),
    QR = TRUE,
    aux_regression = ~.trt
  )
  class(out) <- c("fitted_model", class(out))
  out
}

#' Plots for an NMA modelr
#'
#' @param model A `PancSurv::fitted_model` object
#' @param type Type of plot to produce
#'
#'
#' @export
plot.fitted_model <- function(model, type = "trace", pars = parsForStan, prob = 0.95, ordered = FALSE, xLims = NULL) {
  if (type == "trace") {
    p <- rstan::traceplot(model[["stanfit"]], pars = pars)
  }
  
  if (type == "forest") {
    post <- as.array(model[["stanfit"]])
    p <- bayesplot::mcmc_intervals(post,
                              pars = pars,
                              prob = prob) + ggplot2::theme_bw()
    if (!is.null(xLims)) {p <- p + scale_x_continuous(limits = xLims)}
  }
  
  p
}

#' Summary of a  NMA model
#'
#' @param model A `PancSurv::fitted_model` object
#' 
#' @export
summary.fitted_model <- function(model, dic = FALSE, looic = FALSE) {
  if (dic) {
    out <- multinma::dic(model)
  } else if (looic) {
    out <- loo::loo(model)
  }
  out
}