#' Run an NMA
#'
#' @param network A [multinma::nma_data] object
#' @param effects A string specifiying the effects, either "fixed" or "random"
#' @param seed Random number generating seed
#' @param chains Number of chains used by Stan
#' @param iter Number of iterations
#' @param llhood Character string specifying a likelihood function
#' @param ... Other parameters to pass to [multinma::nma]
#'  
#' @export
fit_model <- function(network, effects, seed = 1, chains = 4,
                      iter = 1000, 
                      llhood = "weibull"){
  out <- multinma::nma(
    network = network,
    trt_effects = effects,
    chains = chains,
    likelihood = llhood,
    seed = seed,
    iter = iter,
    prior_intercept = normal(0, 100),
    prior_trt = normal(0, 10),
    QR = TRUE,
    aux_regression = ~.trt + Male
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
plot.fitted_model <- function(model, type = "trace", pars = parsForStan, prob = 0.95, ordered = FALSE, xLims = NULL, study) {
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
  
  if (type == "survival") {
    f <- plot(predict(model, type = type))
    d <- f$data %>% filter(.study == study)
    f$data <- d
    f
    f <- f +
      theme(legend.position = "top", legend.box.spacing = unit(0, "lines")) +
      labs(x = "Time (Months)", y = "Overall Survival") +
      theme_bw()
    return(f)
  }
  
  return(p)
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
