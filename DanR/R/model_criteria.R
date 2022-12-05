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
