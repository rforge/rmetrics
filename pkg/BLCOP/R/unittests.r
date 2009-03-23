
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