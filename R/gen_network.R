#' Generate a network of evidence
#'
#' @param net_data A dataset created by [PCNMA::gen_network_data]
#' @param ref A character reference treatment
#'
#' @returns A [mutlinma::nma_data] object
#' 
#' @export
gen_network <- function(net_data, ref, y = "RMST"){
  net <- multinma::set_agd_arm(
    net_data,
    study = Study,
    trt = Treatment,
    y = ifelse(y == "RMST", Estimate, Median),
    se = SE, 
    sample_size = n,
    trt_ref = ref)
  net
}