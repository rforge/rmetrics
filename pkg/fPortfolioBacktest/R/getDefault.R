
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
# FUNCTION:
#  getWindows               
#  getWindowsFun
#	getWindowsParams
#  getWindowsHorizon
#  get Strategy
#  getStrategyFun
#	getStrategyParams
#  getStrategyParams
#  getSmoother
#  getSmootherFun
#	getSmootherParams
#  getSmootherLambda
#  getSmootherDoubleSmoothing
#  getSmootherInitialWeights
#	getSmootherSkip
#  getMessages
################################################################################


getWindows <-
    function(object)
{   
    # A function implemented by Diethelm Wuertz
    UseMethod("getWindows")
}


# ------------------------------------------------------------------------------


getWindowsFun <-
    function(object)
{   
    # A function implemented by Diethelm Wuertz
    UseMethod("getWindowsFun")
}


# ------------------------------------------------------------------------------


getWindowsParams <-
    function(object)
{   
    # A function implemented by Diethelm Wuertz
    UseMethod("getWindowsParams")
}


# ------------------------------------------------------------------------------


getWindowsHorizon <-
    function(object)
{   
    # A function implemented by Diethelm Wuertz
    UseMethod("getWindowsHorizon")
}


# ------------------------------------------------------------------------------


getSmoother <-
    function(object)
{   
    # A function implemented by Diethelm Wuertz
    UseMethod("getSmoother")
}


# ------------------------------------------------------------------------------


getSmootherFun <-
    function(object)
{   
    # A function implemented by Diethelm Wuertz
    UseMethod("getSmootherFun")
}

# ------------------------------------------------------------------------------


getSmootherParams <-
    function(object)
{   
    # A function implemented by Diethelm Wuertz
    UseMethod("getSmootherParams")
}


# ------------------------------------------------------------------------------


getSmootherLambda <-
    function(object)
{   
    # A function implemented by Diethelm Wuertz
    UseMethod("getSmootherLambda")
}

# ------------------------------------------------------------------------------


getSmootherDoubleSmoothing <-
    function(object)
{   
    # A function implemented by Diethelm Wuertz
    UseMethod("getSmootherDoubleSmoothing")
}


# ------------------------------------------------------------------------------


getSmootherInitialWeights <-
    function(object)
{   
    # A function implemented by Diethelm Wuertz
    UseMethod("getSmootherInitialWeights")
}


# ------------------------------------------------------------------------------

getSmootherSkip <-
    function(object)
{   
    # A function implemented by Diethelm Wuertz
    UseMethod("getSmootherSkip")
}

# ------------------------------------------------------------------------------


getStrategy <-
    function(object)
{   
    # A function implemented by Diethelm Wuertz
    UseMethod("getStrategy")
}


# ------------------------------------------------------------------------------


getStrategyFun <-
    function(object)
{   
    # A function implemented by Diethelm Wuertz
    UseMethod("getStrategyFun")
}


# ------------------------------------------------------------------------------


getStrategyParams <-
    function(object)
{   
    # A function implemented by Diethelm Wuertz
    UseMethod("getStrategyParams")
}


# ------------------------------------------------------------------------------


getMessages <-
    function(object)
{   
    # A function implemented by Diethelm Wuertz
    UseMethod("getMessages")
}


################################################################################

