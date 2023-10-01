fit_model <- function(network, L, lnk){
  out <- multinma::nma(
    test,
    likelihood = L,
    link = lnk
  )
}
