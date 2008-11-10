runBLCOPTestSuite <- function(testPath = BLCOPOptions("unitTestPath"))
{
    BLTestSuite <- defineTestSuite(name = "Black-Litterman / COP unit tests", dirs = testPath) 
    testResults <- runTestSuite(BLTestSuite)
    summary(testResults)                                  
}