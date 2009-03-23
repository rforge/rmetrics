.optimalWeights.simpleMV <- function(mu, sigma, constraints=NULL, tol = 1e-6)
{
	if(!require("quadprog", quiet = TRUE))
	{
		stop("This function depends on quadprog, which is not available")
	}
	
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
# optimalPortfolios.simple
# Author: Francisco
###############################################################################
# DESCRIPTION: A utility function that calculates "optimal" portfolios with respect to a prior and (Black-Litterman) posterior distribution, 
# and then returns the weights and optionally plots them with barplots.  The optimizer is provided by the user, but there is a "toy" 
# Markowitz optimizer that is used by default
# KEYWORDS: optimize
###############################################################################


#' 
#' @param result 
#' @param optimizer 
#' @param ... 
#' @param doPlot 
#' @param beside 
#' @return 
#' @author fgochez <fgochez@mango-solutions.com>
#' @export

optimalPortfolios <- function
(                                         
	result,                               #
	optimizer = .optimalWeights.simpleMV,  # Function that performs optimization.  Its first argument should be the mean vector, its
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

optimalPortfolios.fPort <- function(result, spec, constraints = "LongOnly", optimizer = "efficientPortfolio")
{
	assets <- assetSet(result@views)
	dmySeries <- as.timeSeries(matrix(0, nrow = 1, ncol = length(assets), dimnames = list(NULL, assets)))
	
	priorSpec <- spec
	
	.priorEstim <<- function(...)
	{
		list(mu = result@priorMean, Sigma = result@priorCovar)
	}
	.posteriorEstim <<- function(...)
	{
		list(mu = result@posteriorMean, Sigma = result@posteriorCovar)
	}
	setEstimator(priorSpec) <- ".priorEstim"
	posteriorSpec <- spec
	setEstimator(posteriorSpec) <- ".posteriorEstim"
	optimizer <- match.fun(optimizer)
	priorOptimPortfolio <- optimizer(dmySeries, priorSpec, constraints)
	posteriorOptimPortfolio <- optimizer(dmySeries, posteriorSpec, constraints)
	
	.priorEstim <<- NULL
	.posteriorEstim <<- NULL
	
	list(priorOptimPortfolio = priorOptimPortfolio, posteriorOptimPortfolio = posteriorOptimPortfolio)
}