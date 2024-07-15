#' Generate a network of evidence
#'
#' @param net_data A data set created by [PCNMA::gen_network_data]
#' @param ref A character reference treatment
#' @param covs Any covariates to be included in the ML-NMR
#'
#' @returns A [mutlinma::nma_data] object
#' 
#' @export
gen_network <- function(net_data, ref, covs){
  net <- multinma::set_agd_surv(
      net_data,
      study = Study,
      trt = Treatment,
      Surv = Surv(time, status),
      trt_ref = ref,
      covariates = covs,
      trt_class = trtclass
  )
  net
}
