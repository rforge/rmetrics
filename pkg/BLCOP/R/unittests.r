
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

# Utility function to generate a COP prior/posterior data set, rather than repeating code over and over
# in the unit test scripts

COPExample <- function(numSimulations = BLCOPOptions("numSimulations"))
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

# Utility function to generate a BL prior/posterior data set, rather than repeating code over and over
# in the unit test scripts

BLExample <- function()
{
	entries <- c(0.001005,0.001328,-0.000579,-0.000675,0.000121,0.000128,-0.000445,-0.000437 ,
			0.001328,0.007277,-0.001307,-0.000610,-0.002237,-0.000989,0.001442,-0.001535 ,
			-0.000579,-0.001307,0.059852,0.027588,0.063497,0.023036,0.032967,0.048039 ,
			-0.000675,-0.000610,0.027588,0.029609,0.026572,0.021465,0.020697,0.029854 ,
			0.000121,-0.002237,0.063497,0.026572,0.102488,0.042744,0.039943,0.065994 ,
			0.000128,-0.000989,0.023036,0.021465,0.042744,0.032056,0.019881,0.032235 ,
			-0.000445,0.001442,0.032967,0.020697,0.039943,0.019881,0.028355,0.035064 ,
			-0.000437,-0.001535,0.048039,0.029854,0.065994,0.032235,0.035064,0.079958 )
	
	myVarcov2 <- matrix(entries, ncol = 8, nrow = 8)
	mu <- c(0.08, 0.67,6.41, 4.08, 7.43, 3.70, 4.80, 6.60) / 100
	pick <- matrix(0, ncol = 8, nrow = 3, dimnames = list(NULL, letters[1:8]))
	pick[1,7] <- 1
	pick[2,1] <- -1; pick[2,2] <- 1
	pick[3, 3:6] <- c(0.9, -0.9, .1, -.1)
	confidences <- 1 / c(0.00709, 0.000141, 0.000866)
	myViews <- BLViews(pick, c(0.0525, 0.0025, 0.02), confidences, letters[1:8])
	myPosterior <- posteriorEst(myViews, tau = 0.025, mu = mu, myVarcov2 )
	list(prior = myViews, posterior = myPosterior)
}