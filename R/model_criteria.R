#' Gelfand and Ghosh Posterior Predictive Loss Criterion Function
#'
#' This function implements the Gelfand and Ghosh predictive loss criterion as
#' proposed in 1998 by Alan Gelfand and Sujit Ghosh.
#'
#' @param k This is the weight parameter that decides how much goodness-of-fit
#'   matters in your loss function.
#' @param preds This is an n by s matrix of predictions
#' @param obs This is an n-dimensional observation vector
#' @keywords Model Comparisons, Bayesian
#' @export
#' @importFrom stats var
#' @references Gelfand, Alan E., and Sujit K. Ghosh. "Model choice: a minimum
#'   posterior predictive loss approach." Biometrika 85.1 (1998): 1-11.
#'
gelfand_and_ghosh <- function(k,preds,obs){
  if(nrow(preds) != length(obs)) return("You have to have an n by s prediction
                                        matrix corresponding to a length n \
                                        vector of observations!")
  GP_indiv <- apply(cbind(obs,preds),1,function(z){
    g <- (mean(z[-1]) - z[1])^2
    p <- var(z[-1])
    c(g,p)
  })
  GP <- apply(GP_indiv,1,sum)
  (k/(1+k))*GP[1] + GP[2]
}

#' Model selection criteria for AR models
#'
#' This calculates the statistics and optimal model orders for AR time series
#' models
#'
#' @param y time series
#' @param order_limit limit to the AR model order (integer)
#'
#' @return a list of the optimal order found for each criterion and the criteria
#'   values
#' @export
#'
#' @importFrom stats ar.yw dnorm
#'
#' @examples
#' set.seed(1)
#' y <- arima.sim(list(ar = c(.35,.1)), n = 1200)
#' ar_information_criteria(y,10)
ar_information_criteria <- function(y, order_limit = 6) {
  N <- length(y)
  if(order_limit %% 1 !=0) stop("order_limit should be an integer")
  if(order_limit < 1) stop("order_limit should be at least 1")
  aic_vals <- aicc_vals <- bic_vals <- numeric(order_limit)
  for(k in seq(order_limit)){
    ar_mdl <- stats::ar.yw(x = y,order.max = k, aic = FALSE)
    log_lik <- sum(stats::dnorm(ar_mdl$resid[-seq(k)],sd = sqrt(ar_mdl$var.pred), log = TRUE))
    aic_vals[k] <- N * (log(ar_mdl$var.pred) + 1) + 2*(k+1)
    bic_vals[k] <- -2*log_lik + k*log(N)
    aicc_vals[k] <- N * (log(ar_mdl$var.pred) + 1) + 2*(k+1) * N / (N - k - 2)
  }
  optimal_order <- c(AIC = which.min(aic_vals),AICc = which.min(aicc_vals), BIC = which.min(bic_vals))
  criteria <- cbind(AIC = aic_vals, AICc = aicc_vals, BIC = bic_vals)
  return(list(order = optimal_order, criteria = criteria))
}
