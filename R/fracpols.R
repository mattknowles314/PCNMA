#' Box-Tidwell Transformation
#'
#' @param x A real value
#' @param p The p-value to raise x to
#'
#' @export
#' 
boxTid <- function(x, p) {
  ifelse(p == 0, return(log(x)), return(x^p))
}


#' H-function for FPs
#'
#' @param x A real value
#' @param P A vector of powers
#' @param zeta A vector of zeta values 
#' @param j The index
#'
#' @export
#' 
H <- function(x, P, zeta, j){
  if (P[j] == 0) {return(1)}
  
  if (P[j] != P[j - 1]) {return(boxTid(x, P[j]))}
  
  if (P[j] == P[j - 1]) {return(log(x)*H(x, P, zeta, j - 1))}
}

#' Fractional Polynomial Function
#'
#' @param x A real valye
#' @param m The degree of the polynomial
#' @param P A vector of powers
#' @param zeta A vector of zeta values
#'
#' @export
#' 
phi <- function(x, m, P, zeta){
  vals <- c()
  for (j in 2:m) {
    vals[j] <- H(x, P, zeta, j)
  }
  return(vals)
}