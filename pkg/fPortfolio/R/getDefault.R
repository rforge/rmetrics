
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR Description. See the 
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General 
# Public License along with this library; if not, write to the 
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
# MA 02111-1307 USA

# Copyrights (C)
# for this R-port: 
#   1999 - Diethelm Wuertz, GPL
#   2007 - Rmetrics Foundation, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
# for code accessed (or partly included) from other sources:
#   see Rmetric's copyright and license files


################################################################################
#     function:
#  getConstraints
#  getCov
#  getCovRiskBudgets
#  getData
#  getEstimator
#  getMean
#  getMu
#  getNAssets 
#  getNames
#  getNFrontierPoints
#  getOptim
#  getPortfolio
#  getParams
#  getRiskFreeRates
#  getSeries
#  getSigma
#  getSolver
#  getSpec
#  getStatistics
#  getStatus
#  getAlpha
#  getTailRisk
#  getTailRiskBudgets
#  getAlpha
#  getTargetReturn
#  getTargetRisk
#  getTrace
#  getType
#  getWeights
################################################################################


getConstraints <- 
    function(object)
{   
    # A function implemented by Diethelm Wuertz

    UseMethod("getConstraints")
}


# ------------------------------------------------------------------------------


getCov <- 
    function(object)
{   
    # A function implemented by Diethelm Wuertz

    UseMethod("getCov")
}


# ------------------------------------------------------------------------------


getData <-
    function(object)
{   
    # A function implemented by Diethelm Wuertz

    UseMethod("getData")
}


# ------------------------------------------------------------------------------


getCovRiskBudgets <- 
    function(object) 
{   
    # A function implemented by Diethelm Wuertz

    UseMethod("getCovRiskBudgets")
}


# ------------------------------------------------------------------------------


getEstimator <-
    function(object)
{   
    # A function implemented by Diethelm Wuertz

    UseMethod("getEstimator")
}


# ------------------------------------------------------------------------------


getMean <-
    function(object)
{   
    # A function implemented by Diethelm Wuertz

    UseMethod("getMean")
}


# ------------------------------------------------------------------------------


getMu <-
    function(object)
{   
    # A function implemented by Diethelm Wuertz

    UseMethod("getMu")
}


# ------------------------------------------------------------------------------
    
    
getNAssets <-
    function(object)
{   
    # A function implemented by Diethelm Wuertz

    UseMethod("getNAssets")
}


# ------------------------------------------------------------------------------
    
    
getNames <-
    function(object)
{   
    # A function implemented by Diethelm Wuertz

    UseMethod("getNames")
}


# ------------------------------------------------------------------------------


getNFrontierPoints <-
    function(object)
{   
    # A function implemented by Diethelm Wuertz

    UseMethod("getNFrontierPoints")
}


# ------------------------------------------------------------------------------


getOptim <-
    function(object)
{   
    # A function implemented by Diethelm Wuertz

    UseMethod("getOptim")
}


# ------------------------------------------------------------------------------


getPortfolio <-
    function(object)
{   
    # A function implemented by Diethelm Wuertz

    UseMethod("getPortfolio")
}


# ------------------------------------------------------------------------------


getParams <-
    function(object)
{   
    # A function implemented by Diethelm Wuertz

    UseMethod("getParams")
}


# ------------------------------------------------------------------------------


getRiskFreeRate <-
    function(object)
{   
    # A function implemented by Diethelm Wuertz

    UseMethod("getRiskFreeRate")
}


# ------------------------------------------------------------------------------
    
    
getSeries <-
    function(object)
{   
    # A function implemented by Diethelm Wuertz

    UseMethod("getSeries") 
}


# ------------------------------------------------------------------------------


getSigma <-
    function(object)
{   
    # A function implemented by Diethelm Wuertz

    UseMethod("getSigma")
}


# ------------------------------------------------------------------------------


getSolver <-
    function(object)
{   
    # A function implemented by Diethelm Wuertz

    UseMethod("getSolver")
}


# ------------------------------------------------------------------------------


getSpec <-
    function(object)
{   
    # A function implemented by Diethelm Wuertz

    UseMethod("getSpec")
}


# ------------------------------------------------------------------------------


getStatistics <-
    function(object)
{   
    # A function implemented by Diethelm Wuertz

    UseMethod("getStatistics")
}


# ------------------------------------------------------------------------------


getStatus <-
    function(object)
{   
    # A function implemented by Diethelm Wuertz

    UseMethod("getStatus")
}


# ------------------------------------------------------------------------------


getTailRisk <-
    function(object)
{   # A function implemented by Diethelm Wuertz

    UseMethod("getTailRisk")
}


# ------------------------------------------------------------------------------


getTailRiskBudgets <- 
    function(object) 
{   
    # A function implemented by Diethelm Wuertz

    UseMethod("getTailRiskBudgets")
}


# ------------------------------------------------------------------------------


getAlpha <-
    function(object)
{   
    # A function implemented by Diethelm Wuertz

    UseMethod("getAlpha")
}


# ------------------------------------------------------------------------------


getTargetReturn <-
    function(object)
{   
    # A function implemented by Diethelm Wuertz

    UseMethod("getTargetReturn")
}


# ------------------------------------------------------------------------------


getTargetRisk <-
    function(object)
{   
    # A function implemented by Diethelm Wuertz

    UseMethod("getTargetRisk")
}


# ------------------------------------------------------------------------------


getTrace <-
    function(object)
{   
    # A function implemented by Diethelm Wuertz

    UseMethod("getTrace")
}


# ------------------------------------------------------------------------------


getType <-
    function(object)
{   
    # A function implemented by Diethelm Wuertz

    UseMethod("getType")
}


# ------------------------------------------------------------------------------


getWeights <-
    function(object)
{   
    # A function implemented by Diethelm Wuertz

    UseMethod("getWeights")
}


################################################################################

