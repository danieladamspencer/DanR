# I'm finally getting around to writing this package. It'll happen bit by bit, but it should make my life better
# How to update:
# library(devtools)
# library(roxygen2) #devtools::install_github("klutometis/roxygen")
# Navigate to directory containing this file
# document()
# setwd("../../")
# install("DanR")
# library(DanR)

#' Gelfand and Ghosh Posterior Predictive Loss Criterion Function
#' 
#' This function implements the Gelfand and Ghosh predictive loss criterion as proposed in 1998 by Alan Gelfand and Sujit Ghosh.
#' @param k This is the weight parameter that decides how much goodness-of-fit matters in your loss function.
#' @param preds This is an n by s matrix of predictions
#' @param obs This is an n-dimensional observation vector
#' @keywords Model Comparisons, Bayesian
#' @export
#' @references Gelfand, Alan E., and Sujit K. Ghosh. "Model choice: a minimum posterior predictive loss approach." Biometrika 85.1 (1998): 1-11.
#' 

gelfand_and_ghosh <- function(k,preds,obs){
  if(nrow(preds) != length(obs)) return("You have to have an n by s prediction matrix corresponding to a length n vector of observations!")
  GP_indiv <- apply(cbind(obs,preds),1,function(z){
    g <- (mean(z[-1]) - z[1])^2
    p <- var(z[-1])
    c(g,p)
  })
  GP <- apply(GP_indiv,1,sum)
  (k/(1+k))*GP[1] + GP[2]
}

#' Multivariate Normal Draws 
#' 
#' This function draws from a multivariate normal distribution of dimension p more efficiently than the rmvnorm function in the mvtnorm package.
#' @param n The number of draws to make from the multivariate normal distribution. Must be a positive integer.
#' @param mu The mean vector of length p
#' @param V The $p\times p$ covariance matrix
#' @keywords Gaussian, Normal 
#' @export
#' @examples 
#' rmvn(5,c(2,2),matrix(c(2,-1,-1,5),2,2))
#'           [,1]      [,2]      [,3]     [,4]     [,5]
#' [1,] 0.2962443 3.3602800 0.9445853 2.869058 1.860385
#' [2,] 7.5907838 0.4752729 1.7318177 1.648304 3.482479

rmvn <- function(n,mu=0,V=matrix(1)){
  p <- length(mu)
  if(any(is.na(match(dim(V),p)))){stop("Dimension Problem!")}
  D <- chol(V)
  t(matrix(rnorm(n*p),ncol=p)%*%D + rep(mu,rep(n,p)))
}

#' Inverse Gamma Parameters
#' 
#' This function gives appropriate shape and rate parameters for an inverse gamma distribution when you give a desired mean and variance for the distribution.
#' @param mean Desired mean of the distribution
#' @param var Desired variance of the distribution
#' @keywords Inverse Gamma, hyperparameters
#' @export
#' @examples 
#' inv_gamma_params(mean = 2,var = 1)
#' [1]  6 10
#' mean(1/rgamma(1000,6,10))
#' [1] 2.027287
#' var(1/rgamma(1000,6,10))
#' [1] 0.9945234

inv_gamma_params <- function(mean, var){
  if(mean <= 0 || var <= 0) stop("Both the mean and the variance must be greater than zero!")
  shape <- mean^2 / var + 2
  rate <- mean*(shape - 1)
  c(shape,rate)
}

#' Gamma Parameters
#' 
#' This function gives  appropriate shape and rate parameters for a gamma distribution given a desired mean and variance
#' @param mean Desired mean of the distribution
#' @param var Desired variance of the distribution
#' @keywords Gamma, hyperparameters
#' @export
#' @examples 
#' > gamma_params(mean = 5, var = 3)
#' [1] 8.333333 1.666667
#' > mean(rgamma(1000,8.333333,1.666667))
#' [1] 4.937651
#' > var(rgamma(1000,8.333333,1.666667))
#' [1] 3.172861

gamma_params <- function(mean,var){
  if(mean <= 0 || var <= 0) stop("Both the mean and the variance must be greater than zero!")
  shape <- mean^2 / var 
  rate <- mean / var
  c(shape,rate)
}