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
#' @param model A [PCNMA::fitted_model] object
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

#' Summary of an NMA model
#'
#' @param model A [PCNMA::fitted_model] object
#' 
#' @export
summary.fitted_model <- function(model, likelihood, effect) {
  a <- try({multinma::dic(model)[["dic"]]})
  if (inherits(a, "try-error")) {a <- NA}
  b <- try({loo::loo(model)[["looic"]]})
  if (inherits(b, "try-error")) {b <- NA}
  out <- tibble("Likelihood" = rep(likelihood, 2), "Effect" = rep(effect, 2), "IC" = c("DIC", "LOOIC"), "Result" = c(a, b)) |> 
    tidyr::pivot_wider(names_from = "IC", values_from = "Result")
  out
}
