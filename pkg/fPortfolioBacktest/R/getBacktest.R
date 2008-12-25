
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General
# Public License along with this library; if not, write to the
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston,
# MA  02111-1307  USA

# Copyrights (C)
# for this R-port:
#   1999 - 2008, Diethelm Wuertz, Rmetrics Foundation, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:                    DESCRIPTION:
#  getWindows					Extract Windows slot
#   getWindowsFun				Extract Windows function
#	 getWindowsParams		    Extract a list of windows specific parameters
#   getWindowsHorizon			Extract windows horizon
#  getStrategy					Extract Strategy slot
#   getStrategyFun				Extract the portfolio strategy function
#	 getStrategyParams			Extract a list of sStrategy specific parameters
#  getSmoother					Extract the Smoother slot
#   getSmootherFun				Extract the Smoother function
#	 getSmootherParams			Extract a list of smoothing specific parameters
#   getSmootherLambda			Extract the smoothing parameter Lambda
#   getSmootherDoubleSmoothing	Extract setting for double smoothing
#   getSmootherInitialWeights	Extract the initial weights in the smoothing
#	getSmootherSkip 		 	Extract the number of skipped months
#  getMessages					Extract the Message slot

################################################################################


getWindows.fPFOLIOBACKTEST =
    function(object)
{   
    # A function implemented by William Chen
    
    # FUNCTION:
    	
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


# ------------------------------------------------------------------------------
	

getWindowsFun.fPFOLIOBACKTEST = 
    function(object) getWindows(object)$windows


# ------------------------------------------------------------------------------


getWindowsParams.fPFOLIOBACKTEST = 
    function(object) getWindows(object)$params
 
   
# ------------------------------------------------------------------------------


getWindowsHorizon.fPFOLIOBACKTEST = 
    function(object) getWindowsParams(object)$horizon
    
    
# ------------------------------------------------------------------------------


getSmoother.fPFOLIOBACKTEST =
    function(object)
{   
    # A function implemented by William Chen
    
    # FUNCTION:
    	
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


# ------------------------------------------------------------------------------


getSmootherFun.fPFOLIOBACKTEST = 
    function(object) getSmoother(object)$smoother

    
# ------------------------------------------------------------------------------


getSmootherParams.fPFOLIOBACKTEST = 
    function(object) getSmoother(object)$params

    
# ------------------------------------------------------------------------------


getSmootherLambda.fPFOLIOBACKTEST = 
    function(object) getSmootherParams(object)$lambda

    
# ------------------------------------------------------------------------------


getSmootherDoubleSmoothing.fPFOLIOBACKTEST = 
    function(object) getSmootherParams(object)$doubleSmoothing

    
# ------------------------------------------------------------------------------


getSmootherInitialWeights.fPFOLIOBACKTEST = 
    function(object) getSmootherParams(object)$initialWeights

    
# ------------------------------------------------------------------------------


getSmootherSkip.fPFOLIOBACKTEST = 
    function(object) getSmootherParams(object)$skip
 
       
# ------------------------------------------------------------------------------


getStrategy.fPFOLIOBACKTEST =
    function(object)
{   
    # A function implemented by William Chen
    
    # FUNCTION:
    	
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

    
# ------------------------------------------------------------------------------


getStrategyFun.fPFOLIOBACKTEST = 
    function(object) getStrategy(object)$strategy

    
# ------------------------------------------------------------------------------


getStrategyParams.fPFOLIOBACKTEST = 
    function(object) getStrategy(object)$params
    

# ------------------------------------------------------------------------------
   

getMessages.fPFOLIOBACKTEST =
    function(object)
{   
    # A function implemented by William Chen
    
    # FUNCTION:
    	
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

