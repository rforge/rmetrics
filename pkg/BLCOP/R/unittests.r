
runBLCOPTests <- function(testPath = BLCOPOptions("unitTestPath"), protocolFile = "BLCOPTests.html", writeProtocol = FALSE)
{
    if(!require("RUnit"))
	{
		stop("Unable to load the RUnit package, will not execute the tests.")
	}
	BLTestSuite <- defineTestSuite(name = "Black-Litterman / COP unit tests", dirs = testPath) 
    testResults <- runTestSuite(BLTestSuite)
    if(writeProtocol)
		printHTMLProtocol(testResults, file = protocolFile)
	testResults
}

# Utility function to generate a COP posterior data set, rather than reusing code over and over
# in the unit test scripts

COPExample <- function()
{
	if(!require("sn", quiet = TRUE))
	{
		warning("This test relies on the sn package which is not available \n")
		return()
	}
	NUMTESTSIMULATIONS <- 1000
	dispersion <- c(.376,.253,.360,.333,.360,.600,.397,.396,.578,.775) / 1000
	sigma <- BLCOP:::.symmetricMatrix(dispersion, dim = 4)
	caps <- rep(1/4, 4)
	mu <- 2.5 * sigma %*% caps
	dim(mu) <- NULL
	marketDistribution <- mvdistribution("mt", mean = mu, S = sigma, df = 5 )
	pick <- matrix(0, ncol = 4, nrow = 1, dimnames = list(NULL, c("SP", "FTSE", "CAC", "DAX")))
	pick[1,4] <- 1
	vdist <- list(distribution("unif", min = -0.02, max = 0))
	
	views <- COPViews(pick, vdist, 0.2, c("SP", "FTSE", "CAC", "DAX"))
	set.seed(3)
	posterior <- COPPosterior(marketDistribution, views, numSimulations = NUMTESTSIMULATIONS)
	
	list(prior = views, posterior = posterior)
	
}