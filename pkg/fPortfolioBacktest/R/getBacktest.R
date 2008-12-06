
################################################################################
# FUNCTION:                     				PORTFOLIO S4 EXTRACTORS FROM BACKTEST SPECIFICATION:
#  getWindows					 				Extract Windows slot
#   getWindowsFun							Extract Windows function
#	 getWindowsParams						Extract a list of Windows specific parameters
#   getWindowsHorizon						Extract Windows horizon
#  getStrategy									Extract Strategy slot
#   getStrategyFun							Extract the portfolio Strategy function
#	 getStrategyParams						Extract a list of portfolio Strategy specific parameters
#  getSmoother									Extract the Smoother slot
#   getSmootherFun							Extract the Smoother function
#	 getSmootherParams						Extract a list of Smoothing specific parameters
#   getSmootherLambda					Extract the smoothing parameter Lambda
#   getSmootherDoubleSmoothing	Extract setting for double smoothing
#   getSmootherInitialWeights			Extract the initial weights to used in the smoothing
#	getSmootherSkip 		 					Extract the number of skipped months
#  getMessages									Extract the Message slot

################################################################################


getWindows.fPFOLIOBACKTEST =
    function(object)
{   
    # Description:
    #   gets the "model" slot from an object of class 4
   
    # Arguments:
    #   object - an object of class S4
   
    # FUNCTION:
   
    # check if its of .fPFOLIOBACKTEST class
    stopifnot(class(object) == "fPFOLIOBACKTEST")
   
    # Return Value:
    getSlot(object, "windows")
}
	
	getWindowsFun.fPFOLIOBACKTEST = function(object) getWindows(object)$windows
	getWindowsParams.fPFOLIOBACKTEST = function(object) getWindows(object)$params
    getWindowsHorizon.fPFOLIOBACKTEST = function(object) getWindowsParams(object)$horizon
    
# ------------------------------------------------------------------------------

getSmoother.fPFOLIOBACKTEST =
    function(object)
{   
    # Description:
    #   gets the "model" slot from an object of class 4
   
    # Arguments:
    #   object - an object of class S4
   
    # FUNCTION:
   
     # check if its of .fPFOLIOBACKTEST class
    stopifnot(class(object) == "fPFOLIOBACKTEST")
   
    # Return Value:
    getSlot(object, "smoother")
}


    getSmootherFun.fPFOLIOBACKTEST = function(object) getSmoother(object)$smoother
    getSmootherParams.fPFOLIOBACKTEST = function(object) getSmoother(object)$params
    getSmootherLambda.fPFOLIOBACKTEST = function(object) getSmootherParams(object)$lambda
    getSmootherDoubleSmoothing.fPFOLIOBACKTEST = function(object) getSmootherParams(object)$doubleSmoothing
    getSmootherInitialWeights.fPFOLIOBACKTEST = function(object) getSmootherParams(object)$initialWeights
    getSmootherSkip.fPFOLIOBACKTEST = function(object) getSmootherParams(object)$skip
    
# ------------------------------------------------------------------------------


getStrategy.fPFOLIOBACKTEST =
    function(object)
{   
    # Description:
    #   gets the "model" slot from an object of class 4
   
    # Arguments:
    #   object - an object of class S4
   
    # FUNCTION:
   
    # check if its of .fPFOLIOBACKTEST class
    stopifnot(class(object) == "fPFOLIOBACKTEST")
   
    # Return Value:
    getSlot(object, "strategy")
}


    getStrategyFun.fPFOLIOBACKTEST = function(object) getStrategy(object)$strategy
    getStrategyParams.fPFOLIOBACKTEST = function(object) getStrategy(object)$params

# ------------------------------------------------------------------------------
   
getMessages.fPFOLIOBACKTEST =
    function(object)
{   
    # Description:
    #   gets the "model" slot from an object of class 4
   
    # Arguments:
    #   object - an object of class S4
   
    # FUNCTION:
   
     # check if its of .fPFOLIOBACKTEST class
    stopifnot(class(object) == "fPFOLIOBACKTEST")
   
    # Return Value:
    getSlot(object, "messages")
}

################################################################################

