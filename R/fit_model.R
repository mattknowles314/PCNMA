#' Run an NMA
#'
#' @param network A [multinma::nma_data] object
#' @param L Character string specifying a likelihood function
#' @param link Character string specifying a link function (defaults to "log")
#' @param ... Other parameters to pass to [multinma::nma]
#'  
#' @export
fit_model <- function(network, L, link = "log", iter, seed){
  out <- multinma::nma(
    network,
    likelihood = L,
    link = link,
    consistency = "consistency",
    trt_effects = "random",
    iter = iter,
    regression = NULL,
    seed = seed
  )
  out
}
