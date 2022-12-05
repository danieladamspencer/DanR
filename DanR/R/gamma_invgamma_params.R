#' Inverse Gamma Parameters
#'
#' This function gives appropriate shape and rate parameters for an inverse
#' gamma distribution when you give a desired mean and variance for the
#' distribution.
#'
#' @param mean Desired mean of the distribution
#' @param var Desired variance of the distribution
#' @keywords Inverse Gamma, hyperparameters
#' @export
#' @examples
#' \dontrun{
#' inv_gamma_params(mean = 2,var = 1)
#' [1]  6 10
#' mean(1/rgamma(1000,6,10))
#' [1] 2.027287
#' var(1/rgamma(1000,6,10))
#' [1] 0.9945234
#' }
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
#' \dontrun{
#' > gamma_params(mean = 5, var = 3)
#' [1] 8.333333 1.666667
#' > mean(rgamma(1000,8.333333,1.666667))
#' [1] 4.937651
#' > var(rgamma(1000,8.333333,1.666667))
#' [1] 3.172861
#' }
gamma_params <- function(mean,var){
  if(mean <= 0 || var <= 0) stop("Both the mean and the variance must be greater than zero!")
  shape <- mean^2 / var
  rate <- mean / var
  c(shape,rate)
}
