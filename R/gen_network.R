gen_network <- function(net_data, ref){
  net <- multinma::set_agd_arm(
    net_data,
    study = Study,
    trt = Treatment,
    n = N,
    r = r,
    trt_ref = ref)
}