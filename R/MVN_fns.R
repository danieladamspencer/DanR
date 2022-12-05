#' Multivariate Normal Draws
#'
#' This function draws from a multivariate normal distribution of dimension p
#' more efficiently than the rmvnorm function in the mvtnorm package.
#'
#' @param n The number of draws to make from the multivariate normal
#'   distribution. Must be a positive integer.
#' @param mu The mean vector of length p
#' @param V The p x p covariance matrix
#' @keywords Gaussian, Normal
#' @export
#' @import stats
#' @examples
#' \dontrun{
#' rmvn(5,c(2,2),matrix(c(2,-1,-1,5),2,2))
#'           [,1]      [,2]      [,3]     [,4]     [,5]
#' [1,] 0.2962443 3.3602800 0.9445853 2.869058 1.860385
#' [2,] 7.5907838 0.4752729 1.7318177 1.648304 3.482479
#' }
rmvn <- function(n,mu=0,V=matrix(1)){
  p <- length(mu)
  if(any(is.na(match(dim(V),p)))){stop("Dimension Problem!")}
  D <- chol(V)
  return(t(matrix(stats::rnorm(n*p),ncol=p)%*%D + rep(mu,rep(n,p))))
}
