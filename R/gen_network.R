#' Generate a network of evidence
#'
#' @param net_data A dataset created by [PCNMA::gen_network_data]
#' @param ref A character reference treatment
#'
#' @returns A [mutlinma::nma_data] object
#' 
#' @export
gen_network <- function(net_data, ref){
  net <- multinma::set_agd_arm(
    net_data,
    study = Study,
    trt = Treatment,
    n = N,
    r = r,
    trt_ref = ref)
}