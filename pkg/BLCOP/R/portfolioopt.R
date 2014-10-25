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



#'  A utility function that calculates "optimal" portfolios with respect to a prior and (Black-Litterman) posterior distribution, 
#' and then returns the weights and optionally plots them with barplots.  The optimizer is provided by the user, but there is a "toy" 
#' Markowitz optimizer that is used by default 
#' @param result BLPosterior result
#' @param optimizer Function that performs optimization.  Its first argument should be the mean vector, its
#' second the variance-covariance matrix
#' @param ... Additional paramters to be passed to the optimizer
#' @param doPlot Should a barplot be created?
#' @param beside should the barplot be a side-by-side plot or just a plot of the differences in weights?
#' @return The weights of the optimal prior and posterior portfolios
#' @author fgochez <fgochez@mango-solutions.com>
#' @export

optimalPortfolios <- function
(                                         
	result, optimizer = .optimalWeights.simpleMV, ...,	doPlot = TRUE, 	beside = TRUE  
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

#' A utility function that calculates "optimal" portfolios with respect to a prior and (Black-Litterman) 
#' posterior distribution using the functionality of the Rmetrics fPortfolio package, and then returns the weights
#' @param result BLPosterior result object
#' @param spec An object of class fPORTFOLIOSPEC containing the portfolio specification
#' @param constraints A set of portfolio constraints (as required by fPortfolio optimization routines)
#' @param optimizer Function (or name of a function) that performs portfolio optimization, e.g. "minriskPortfolio"
#' @title Calculate optimal prior and posterior portfolios using fPortfolios
#' @return A list with 2 elements: the prior and posterior portfolios (of class fPORTFOLIO)
#' @author fgochez
#' @export

optimalPortfolios.fPort <- function(result, spec = NULL, constraints = "LongOnly", optimizer = "minriskPortfolio", 
			inputData = NULL, numSimulations = BLCOPOptions("numSimulations"))
{
	stop("Not implemented for this class of result")
}

setGeneric("optimalPortfolios.fPort")

optimalPortfolios.fPort.BL <- function(result, spec = NULL ,constraints = "LongOnly", optimizer = "minriskPortfolio", 
		inputData = NULL, numSimulations = BLCOPOptions("numSimulations"))
{
	if(!require("fPortfolio"))
		stop("The package fPortfolio is required to execute this function, but you do not have it installed.")
	assets <- assetSet(result@views)
	# create a "dummy" series that will only be used because the "optimizer" function requires it (but the mean and
	# covariance will not be calculated using it)
	dmySeries <- as.timeSeries(matrix(0, nrow = 1, ncol = length(assets), dimnames = list(NULL, assets)))
	numAssets <- length(assets)
	if(is.null(spec))
	{
		spec <- portfolioSpec()
		# # setType(spec) <- "MV"
		# setWeights(spec) <- rep(1 / numAssets, times = numAssets)
		 #setSolver(spec) <- "solveRquadprog"
	}
	
	priorSpec <- spec
	
	.priorEstim <- function(...)
	{
		list(mu = result@priorMean, Sigma = result@priorCovar)
	}
	.posteriorEstim <- function(...)
	{
		# posterior mean and covariance estimates come from the BL calculations
		postMeanCov <- posteriorMeanCov(result)
		list(mu = postMeanCov$mean, Sigma = postMeanCov$covariance)
	}
	
	# we must assign global estimator functions in order for the optimization routines to use them to extract
	# the prior and posterior means and variances.  This code is not particularly elegant, but I know of no other way
	# to do this at the moment
	
	if(exists(".priorEstim", envir = .GlobalEnv) | exists(".posteriorEstim", envir = .GlobalEnv))
		stop("Unwilling to perform assignment of .priorEstim or .posteriorEstim because they already exist in the global environment.")
	assign(".priorEstim", .priorEstim, envir = .GlobalEnv)
	assign(".posteriorEstim", .posteriorEstim, envir = .GlobalEnv)
	# delete these when the function terminates
	on.exit({
				rm(.posteriorEstim, envir = .GlobalEnv)
				rm(.priorEstim, envir = .GlobalEnv)
			})
	setEstimator(priorSpec) <- ".priorEstim"
	posteriorSpec <- spec
	setEstimator(posteriorSpec) <- ".posteriorEstim"
	optimizer <- match.fun(optimizer)
	# calculate optimal portfolios
	priorOptimPortfolio <- optimizer(dmySeries, priorSpec, constraints)	
	posteriorOptimPortfolio <- optimizer(dmySeries, posteriorSpec, constraints)
	
	x <- list(priorOptimPortfolio = priorOptimPortfolio, posteriorOptimPortfolio = posteriorOptimPortfolio)
	class(x) <- "BLOptimPortfolios"
	x
}

setMethod("optimalPortfolios.fPort", signature(result = "BLResult"), optimalPortfolios.fPort.BL )

# plot methods, not yet exposed

plot.BLOptimPortfolios <- function(x,...)
{
	plotData <-   getWeights(x[[2]]@portfolio) - getWeights(x[[1]]@portfolio)
	
	plotData <- plotData[plotData != 0]
	
	barplot(plotData, col = c("lightblue"), ylab = "Difference", border = "blue", main = "Differences in weights", horiz = FALSE)	
}

#' A utility function that calculates "optimal" portfolios with respect to a prior and (COP) 
#' posterior distribution using the functionality of the Rmetrics fPortfolio package, and then returns the weights
#' @param result COPPosterior result object
#' @param spec An object of class fPORTFOLIOSPEC containing the portfolio specification.  For COP optimization, the user
#' will likely want to use something like a CVaR-type portfolio
#' @param constraints A set of portfolio constraints (as required by fPortfolio optimization routines)
#' @param optimizer function (or name of a function) that performs portfolio optimization, e.g. "minriskPortfolio"
#' @param inputData matrix, data.frame, or timeSeries class object of return data.  If NULL, a series of data
#' will be simulated using the prior distribution of the COPPosterior object
#' @param numSimulations  Number of posterior distribution simulations to use for the optimization.  Large numbers may cause out-of memory
#' errors or might take very long
#' @return A list with 2 elements: the prior and posterior portfolios (of class fPORTFOLIO)
#' @author Francisco
#' @export

optimalPortfolios.fPort.COP <- function(result, spec = NULL, constraints = "LongOnly", optimizer = "minriskPortfolio",
			inputData = NULL, numSimulations = BLCOPOptions("numSimulations"))
{

	if(!require("fPortfolio"))
		stop("The package fPortfolio is required to execute this function, but you do not have it installed.")
	if(is.null(inputData))
	{
		# no input time series provided, so simulate the asset returns
		inputData <- sampleFrom(result@marketDist, n = numSimulations)
		colnames(inputData) <- assetSet(result@views)
		inputData <- as.timeSeries(inputData)
	}
	numAssets <- length(assetSet(result@views))
	# missing portfolio spec, create mean-CVaR portfolio
	if(is.null(spec))
	{
		spec <- portfolioSpec()
		setType(spec) <- "CVAR"
		setWeights(spec) <- rep(1 / numAssets, times = numAssets)
		setSolver(spec) <- "solveRglpk"
	}
	# the "output" series data for the CVaR optimization will be taken from the simulations
	outData <- tail(posteriorSimulations(result), numSimulations)
	colnames(outData) <- assetSet(result@views)
	# kill row names to prevent errors when coercing to timeSeries object
	rownames(outData) <- NULL
	outData <- as.timeSeries(outData)
	optimizer <- match.fun(optimizer)
	# calculate prior and posterior optimal portfolios
	priorOptimPortfolio <- optimizer(inputData,spec, constraints)
	posteriorOptimPortfolio <- optimizer(outData, spec, constraints)
	x <- list(priorOptimPortfolio = priorOptimPortfolio, posteriorOptimPortfolio = posteriorOptimPortfolio)
	class(x) <- "COPOptimPortfolios"
	x
}

plot.COPOptimPortfolios <- function(x,...)
{
	plotData <-   getWeights(x[[2]]@portfolio) - getWeights(x[[1]]@portfolio)
	
	plotData <- plotData[plotData != 0]
	
	barplot(plotData, col = c("lightblue"), ylab = "Difference", border = "blue", main = "Differences in weights", horiz = FALSE)	
}


setMethod("optimalPortfolios.fPort", signature(result = "COPResult"), optimalPortfolios.fPort.COP )