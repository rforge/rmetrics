
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
#   1999 - 2007, Rmetrics Foundation, GPL
#   Contact: Diethelm Wuertz <wuertz@phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:
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
#  getTargetAlpha
#  getTailRisk
#  getTailRiskBudgets
#  getTargetAlpha
#  getTargetReturn
#  getTargetRisk
#  getTrace
#  getType
#  getWeights
################################################################################


getConstraints =
function(object)
{   # A function implemented by Rmetrics

    UseMethod("getConstraints")
}


# ------------------------------------------------------------------------------


getCov =
function(object)
{   # A function implemented by Rmetrics

    UseMethod("getCov")
}


# ------------------------------------------------------------------------------


getData =
function(object)
{   # A function implemented by Rmetrics

    UseMethod("getData")
}


# ------------------------------------------------------------------------------


getCovRiskBudgets = 
function(object) 
{   # A function implemented by Rmetrics

    UseMethod("getCovRiskBudgets")
}


# ------------------------------------------------------------------------------


getEstimator =
function(object)
{   # A function implemented by Rmetrics

    UseMethod("getEstimator")
}


# ------------------------------------------------------------------------------


getMean =
function(object)
{   # A function implemented by Rmetrics

    UseMethod("getMean")
}


# ------------------------------------------------------------------------------


getMu =
function(object)
{   # A function implemented by Rmetrics

    UseMethod("getMu")
}


# ------------------------------------------------------------------------------
    
    
getNAssets =
function(object)
{   # A function implemented by Rmetrics

    UseMethod("getNAssets")
}


# ------------------------------------------------------------------------------
    
    
getNames =
function(object)
{   # A function implemented by Rmetrics

    UseMethod("getNames")
}


# ------------------------------------------------------------------------------


getNFrontierPoints =
function(object)
{   # A function implemented by Rmetrics

    UseMethod("getNFrontierPoints")
}


# ------------------------------------------------------------------------------


getOptim =
function(object)
{   # A function implemented by Rmetrics

    UseMethod("getOptim")
}


# ------------------------------------------------------------------------------


getPortfolio =
function(object)
{   # A function implemented by Rmetrics

    UseMethod("getPortfolio")
}


# ------------------------------------------------------------------------------


getParams =
function(object)
{   # A function implemented by Rmetrics

    UseMethod("getParams")
}


# ------------------------------------------------------------------------------


getRiskFreeRate =
function(object)
{   # A function implemented by Rmetrics

    UseMethod("getRiskFreeRate")
}


# ------------------------------------------------------------------------------
    
    
getSeries =
function(object)
{   # A function implemented by Rmetrics

    UseMethod("getSeries") 
}


# ------------------------------------------------------------------------------


getSigma =
function(object)
{   # A function implemented by Rmetrics

    UseMethod("getSigma")
}


# ------------------------------------------------------------------------------


getSolver =
function(object)
{   # A function implemented by Rmetrics

    UseMethod("getSolver")
}


# ------------------------------------------------------------------------------


getSpec =
function(object)
{   # A function implemented by Rmetrics

    UseMethod("getSpec")
}


# ------------------------------------------------------------------------------


getStatistics =
function(object)
{   # A function implemented by Rmetrics

    UseMethod("getStatistics")
}


# ------------------------------------------------------------------------------


getStatus =
function(object)
{   # A function implemented by Rmetrics

    UseMethod("getStatus")
}


# ------------------------------------------------------------------------------


getTailRisk =
function(object)
{   # A function implemented by Rmetrics

    UseMethod("getTailRisk")
}


# ------------------------------------------------------------------------------


getTailRiskBudgets = 
function(object) 
{   # A function implemented by Rmetrics

    UseMethod("getTailRiskBudgets")
}


# ------------------------------------------------------------------------------


getTargetAlpha =
function(object)
{   # A function implemented by Rmetrics

    UseMethod("getTargetAlpha")
}


# ------------------------------------------------------------------------------


getTargetReturn =
function(object)
{   # A function implemented by Rmetrics

    UseMethod("getTargetReturn")
}


# ------------------------------------------------------------------------------


getTargetRisk =
function(object)
{   # A function implemented by Rmetrics

    UseMethod("getTargetRisk")
}


# ------------------------------------------------------------------------------


getTrace =
function(object)
{   # A function implemented by Rmetrics

    UseMethod("getTrace")
}


# ------------------------------------------------------------------------------


getType =
function(object)
{   # A function implemented by Rmetrics

    UseMethod("getType")
}


# ------------------------------------------------------------------------------


getWeights =
function(object)
{   # A function implemented by Rmetrics

    UseMethod("getWeights")
}


################################################################################

