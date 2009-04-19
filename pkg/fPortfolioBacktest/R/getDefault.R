
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
#   1999 - 2009, Rmetrics Association, Zurich
#   1999 - 2009, Diethelm Wuertz <wuertz@itp.phys.ethz.ch>  
#   www.rmetrics.org
# for code accessed (or partly included) from other R-ports 
#   and other sources see R's copyright and license files


################################################################################
# FUNCTION:
#  getWindows               
#   getWindowsFun
#	getWindowsParams
#   getWindowsHorizon
#  getStrategy
#   getStrategyFun
#	getStrategyParams
#  getSmoother
#   getSmootherFun
#	getSmootherParams
#   getSmootherLambda
#   getSmootherDoubleSmoothing
#   getSmootherInitialWeights
#	getSmootherSkip
#  getMessages
################################################################################


getWindows <-
function(object)
{   
    # A function implemented by Diethelm Wuertz and William Chen
    
    # Description:
    
    # Arguments:
    
    # FUNCTION:
    
    # Return Value:
    UseMethod("getWindows")
}


# ------------------------------------------------------------------------------


getWindowsFun <-
function(object)
{   
    # A function implemented by Diethelm Wuertz and William Chen
    
    # Description:
    
    # Arguments:
    
    # FUNCTION:
    
    # Return Value:
    UseMethod("getWindowsFun")
}


# ------------------------------------------------------------------------------


getWindowsParams <-
function(object)
{   
    # A function implemented by Diethelm Wuertz and William Chen
    
    # Return Value:
    UseMethod("getWindowsParams")
}


# ------------------------------------------------------------------------------


getWindowsHorizon <-
function(object)
{   
    # A function implemented by Diethelm Wuertz and William Chen
    
    # Description:
    
    # Arguments:
    
    # FUNCTION:
    
    # Return Value:
    UseMethod("getWindowsHorizon")
}


# ------------------------------------------------------------------------------


getSmoother <-
function(object)
{   
    # A function implemented by Diethelm Wuertz and William Chen
    
    # Description:
    
    # Arguments:
    
    # FUNCTION:
    
    # Return Value:
    UseMethod("getSmoother")
}


# ------------------------------------------------------------------------------


getSmootherFun <-
function(object)
{   
    # A function implemented by Diethelm Wuertz and William Chen
    
    UseMethod("getSmootherFun")
}

# ------------------------------------------------------------------------------


getSmootherParams <-
function(object)
{   
    # A function implemented by Diethelm Wuertz and William Chen
    
    # Description:
    
    # Arguments:
    
    # FUNCTION:
    
    # Return Value:
    UseMethod("getSmootherParams")
}


# ------------------------------------------------------------------------------


getSmootherLambda <-
function(object)
{   
    # A function implemented by Diethelm Wuertz and William Chen
    
    # Return Value:
    UseMethod("getSmootherLambda")
}

# ------------------------------------------------------------------------------


getSmootherDoubleSmoothing <-
function(object)
{   
    # A function implemented by Diethelm Wuertz and William Chen
    
    # Description:
    
    # Arguments:
    
    # FUNCTION:
    
    # Return Value:
    UseMethod("getSmootherDoubleSmoothing")
}


# ------------------------------------------------------------------------------


getSmootherInitialWeights <-
function(object)
{   
    # A function implemented by Diethelm Wuertz and William Chen
    
    # Return Value:
    UseMethod("getSmootherInitialWeights")
}


# ------------------------------------------------------------------------------

getSmootherSkip <-
function(object)
{   
    # A function implemented by Diethelm Wuertz and William Chen
    
    # Description:
    
    # Arguments:
    
    # FUNCTION:
    
    # Return Value:
    UseMethod("getSmootherSkip")
}

# ------------------------------------------------------------------------------


getStrategy <-
function(object)
{   
    # A function implemented by Diethelm Wuertz and William Chen
    
    # Description:
    
    # Arguments:
    
    # FUNCTION:
    
    # Return Value:
    UseMethod("getStrategy")
}


# ------------------------------------------------------------------------------


getStrategyFun <-
function(object)
{   
    # A function implemented by Diethelm Wuertz and William Chen
    
    # Description:
    
    # Arguments:
    
    # FUNCTION:
    
    UseMethod("getStrategyFun")
}


# ------------------------------------------------------------------------------


getStrategyParams <-
function(object)
{   
    # A function implemented by Diethelm Wuertz and William Chen
    
    # Description:
    
    # Arguments:
    
    # FUNCTION:
    
    # Return Value:
    UseMethod("getStrategyParams")
}


# ------------------------------------------------------------------------------


getMessages <-
function(object)
{   
    # A function implemented by Diethelm Wuertz and William Chen
    
    # Description:
    
    # Arguments:
    
    # FUNCTION:
    
    # Return Value:
    UseMethod("getMessages")
}


################################################################################

