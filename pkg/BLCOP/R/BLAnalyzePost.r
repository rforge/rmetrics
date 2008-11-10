
###############################################################################
# Mango Solutions, Chippenham SN14 0SQ 2008
# posteriorFeasibility
# Author: Francisco
###############################################################################
# DESCRIPTION: Tries to assess the "feasibility" of a set of Black-Litterman views using the method described by Meucci and Fusai 
# in "Assessing Views".  This method is based on the Mahalanobis distance between the posterior and prior mean
# KEYWORDS: math
# TODO: Appears not to be completely correct at the moment
###############################################################################


posteriorFeasibility <- function(
    result                     # BLResult class object
)
{
    views <- result@views
    qv <- views@qv
    P <- views@P
    numAssets <- length(assetSet(views))
    sigmaInv <- solve(result@priorCovar)
    
    # calculates the Mahalanobis distance as described by the papaer
    mahal <- mahalanobis(result@posteriorMean, result@priorMean, cov = result@priorCovar, inverted = FALSE)
    mahalProb <- 1 - pchisq(mahal, df = numAssets)
    
    if(! result@kappa == 0)
      omega <-  diag(1 / views@confidences)
    else    
      omega <- result@kappa * tcrossprod(P %*% result@priorCovar, P)

    # 
    if(result@tau != 1)    
        warning("This function is not yet implemented for tau != 1, so the calculation of view senstivities will proceed assuming tau = 1")
   #  sensitivities <- -2 * dchisq(mahal, df = numAssets) * (solve(tcrossprod(P %*% result@priorCovar, P) 
   #     + omega) %*%  P %*%(result@posteriorMean-result@priorMean))    
    list("mahalDist" = mahal, "mahalDistProb" = mahalProb, sensitivities = "Not implemented yet") 
}

.optimalWeights.default <- function(mu, sigma, constraints=NULL, tol = 1e-6)
{
    if(is.null(constraints))        
    {    
        numAssets <- length(mu)
        Amat <- rbind(rep(1, numAssets), diag(length(mu)))
        constraints <- list("Amat" = t(Amat), "bvec" = NULL, "meq" = 1)
        constraints$bvec <- c(1, rep(0, length(mu)))
        
    }
    stopifnot(class(constraints) == "list")
    stopifnot(all(c("Amat", "bvec", "meq") %in% names(constraints)))
 

    wts <- solve.QP(sigma, mu, constraints$Amat, constraints$bvec, constraints$meq)
#    else
#        wts <- solve.QP(sigma, mu, constraints$Amat, meq = constraints$meq)
    wts$solution[abs(wts$solution) < tol] <- 0
    names(wts$solution) <- names(mu)
    wts$solution
}

###############################################################################
# Mango Solutions, Chippenham SN14 0SQ 2008
# optimalPortfolios
# Author: Francisco
###############################################################################
# DESCRIPTION: A utility function that calculates "optimal" portfolios with respect to a prior and (Black-Litterman) posterior distribution, 
# and then returns the weights and optionally plots them with barplots.  The optimizer is provided by the user, but there is a "toy" 
# Markowitz optimizer that is used by default
# KEYWORDS: optimize
###############################################################################

                                                                               
optimalPortfolios <- function
(                                         
    result,                               #
    optimizer = .optimalWeights.default,  # Function that performs optimization.  Its first argument should be the mean vector, its
                                          # second the variance-covariance matrix
    ...,                                  # Additional paramters to be passed to the optimizer
    doPlot = TRUE,                        # Should a barplot be created?
    beside = TRUE                         # should the barplot be a side-by-side plot or just a plot of the differences in weights?
) 
{
    BARWIDTH <- 1

    .assertClass(result, "BLResult")
    optimizer <- match.fun(optimizer)
    
    # calculate the optimal prior and posterior weigths
    priorPortfolioWeights <- optimizer(result@priorMean, result@priorCovar, ...)
    postPortfolioWeights <- optimizer(result@posteriorMean, result@posteriorCovar, ...)
    if(doPlot)
    {        
        if(beside) {                                              
            plotData <- .removeZeroColumns(rbind(prior = priorPortfolioWeights, posterior = postPortfolioWeights))
            barplot(plotData, beside = TRUE,col = c("lightblue", "cyan"), border = "blue",
                legend.text = c("Prior", "Posterior"), horiz = FALSE, ylab = "Weights", main = "Optimal weights")
        }
        else
        {
            plotData <-  postPortfolioWeights -  priorPortfolioWeights
            plotData <- plotData[plotData != 0]
            barplot(plotData, col = c("lightblue"), ylab = "Difference", border = "blue", main = "Differences in weights", horiz = FALSE)
        }
        
    }
    return(list(priorPfolioWeights = priorPortfolioWeights, postPfolioWeights = postPortfolioWeights ))    
}
