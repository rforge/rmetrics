###############################################################################
# Mango Solutions, Chippenham SN14 0SQ 2008
# posteriorEst
# Author: Francisco
###############################################################################
# DESCRIPTION: Computes the Black-Litterman posterior estimate 
# KEYWORDS: math
###############################################################################

posteriorEst <- function
(
    views,     # full BLview object.  
    tau,       # Degree of uncertainty in prior
    alphas,    # Equilibrium expected returns
    sigma,     # variance-covariance matrix of asset returns
    kappa = 0  # if greater than 0, the view confidences will be ignored and the
               # omega matrix in the BL model will be replaced by kappa * P %*% sigma %*% t(P)
)
{

  # preallocate the return
  numAssets <- length(assetSet(views))
  
  P <- views@P
  if(kappa == 0)
  {    
      P <- views@P
      if(length(views@confidences) > 1) 
        omega <- diag( 1/ views@confidences) 
      else 
        omega <- matrix(1 / views@confidences, 1,1)
  }
  
  else    
  {
      omega <- kappa * tcrossprod(views@P %*% sigma, views@P)
      omegaInv <- solve(omega)
  }     

  qv <- views@qv
  sigmaInv <- solve(sigma)

  # The following steps are the core Black-Litterman calculations
  
  temp <- tcrossprod(sigma, P)
 
  postMu <- alphas + tau * temp %*% solve(tau * P %*% temp + omega, qv - P %*% alphas)
  postMu <- as.numeric(postMu)
  
  postSigma <- (1 + tau) * sigma - tau^2 * temp %*% solve(tau * P %*% temp + omega, P %*% sigma)
  names(alphas) <- assetSet(views)
  names(postMu) <- assetSet(views)
  
  new("BLResult", views = views, tau = tau, priorMean = alphas, priorCovar = sigma,
               posteriorMean = postMu, posteriorCovar = postSigma, kappa = kappa )
}

###############################################################################
# Mango Solutions, Chippenham SN14 0SQ 2008
# posteriorEst
# Author: Francisco
###############################################################################
# DESCRIPTION: wrapper function to facilitate Black-Litterman calculations. prevents users from having to manually compute alphas 
# and variance-covariance matrix manually
# KEYWORDS: datagen
###############################################################################

BLPosterior <- function
(
  returns,
  views,
  tau = 1,         
  marketIndex,
  riskFree = NULL,
  kappa = 0
)
{
  # TODO: Replace this so that there is no longer a dependency on fAssets
  alphaInfo <- CAPMList(returns, marketIndex, riskFree = riskFree)

  post <- posteriorEst(views, tau = tau, alphas = alphaInfo[["alphas"]], 
      sigma = unclass(cov.shrink(returns)),  kappa = kappa)
  post
}