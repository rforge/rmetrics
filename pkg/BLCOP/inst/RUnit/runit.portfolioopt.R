# $LastChangedDate$
# $Rev$
# Author: Francisco
###############################################################################

# check the basic "toy" portfolio optimizer and the portfolio optimizer that uses fPortfolio
test.optimalPortfolios.BL <- function()
{

	BLEx <- BLCOP:::BLExample()
	myPosterior <- BLEx$posterior
	res <- optimalPortfolios(myPosterior, doPlot = TRUE)
	expected <- c(0.00000000,0.00000000,0.38204176, 0.00000000, 0.08198505,0.00000000, 0.36548138,0.17049181)
	checkEquals(res$priorPfolioWeights, expected, checkNames = FALSE, tolerance = 1e-06)
	expected <- c(0.00000000, 0.00000000, 0.32444891, 0.08071719, 0.09377903, 0.00000000 ,0.32478427 ,0.17627060)
	checkEquals(res$postPfolioWeights, expected, checkNames = FALSE, tolerance = 1e-06)
	
	# now try the fPortfolio optimizer
	
	#optimalPortfolios.fPort.BL <- function(result, spec,constraints = "LongOnly", optimizer = "minriskPortfolio", 
	#		inputData = NULL, numSimulations = NA)
	if(!require("fPortfolio"))
	{
		warning("The fPortfolio package is required to run these tests, but you don't have it installed")
		return()
	}
	res2 <- optimalPortfolios.fPort(myPosterior, spec = portfolioSpec(), optimizer = "tangencyPortfolio")
	
	# there should be two portfolios, each of class
	checkEquals(c(class(res2[[1]]), class(res2[[2]])), c("fPORTFOLIO", "fPORTFOLIO"), check.names = FALSE)
	
	# posterior weights should be similar to those in Idzorek's paper
	checkEquals(getWeights(res2$"posteriorOptimPortfolio"), structure(c(0.277900279601881, 0.168100949804396, 0.0990111058722582, 
							0.143485334128929, 0.0136813854600612, 0.0129876649193811, 0.249161680453947, 
							0.0356715997591463), invest = 1))
	# try another optimizer
	
	res3 <- optimalPortfolios.fPort(myPosterior, spec = portfolioSpec(), optimizer = "minriskPortfolio")
	
	checkEquals(sapply(res3,  getWeights), structure(c(0.94230707004467, 0, 0, 0.0397431006880323, 0, 0, 
							0.0179498292672979, 0, 0.942204885914348, 0, 0, 0.0396001651180883, 
							0, 0, 0.0181949489675634, 0), .Dim = c(8L, 2L), .Dimnames = list(
							NULL, c("priorOptimPortfolio", "posteriorOptimPortfolio"))), 
	   msg = " |minimum risk portfolios as expected")
	
}

# tests optimalPortfolios.fPort for COPResults objects

test.optimalPortfolios.COP <- function()
{

	COPEx <- get(load( file.path(BLCOPOptions("unitTestPath"), "copexample.RData") ))
	# Check optimization with COP
	myPosterior <- COPEx$posterior
	
	res4 <- optimalPortfolios.fPort(myPosterior, spec = NULL, optimizer = "minriskPortfolio", inputData = NULL, 
			numSimulations  = 100	) 
	
	checkEqualsNumeric(getWeights(res4$posteriorOptimPortfolio), c(0.534715878700172, 0.465284121299828, 0, 0))
	
	# second example, using input data
	COPEx2 <- get(load( file.path(BLCOPOptions("unitTestPath"), "copexample2.RData") ))
	
	spec <- portfolioSpec()
	setType(spec) <- "CVaR"
	setWeights(spec) <- rep(1 / 6, times = 6)
	setSolver(spec) <- "solveRglpk"
	setTargetReturn(spec) <- 0.005
	res5 <- optimalPortfolios.fPort( COPEx2, spec = spec, inputData = as.timeSeries(monthlyReturns), numSimulations  = nrow(monthlyReturns))
	
	checkEqualsNumeric(getWeights(res5$priorOptimPortfolio), c(0.0707149756584066, 2.67397784323004e-07, 0.0105900756464006, 
					0.522435704424078, 0, 0.396258976873331))
	checkEqualsNumeric(getWeights(res5$posteriorOptimPortfolio), c(0.513269493960045, 0, 0, 0, 0, 0.486730506039955))
}
