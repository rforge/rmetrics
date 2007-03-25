
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
#   1999 - 2007, Diethelm Wuertz, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION                      PORTFOLIO SPECIFICATION CLASS:
#  'fPFOLIOSPEC'                 S4 Portfolio Specification Class
#  portfolioSpec                 Specifies a portfolio
#  show.fPORTFOLIO               Print method for 'fPFOLIOSPEC' objects
# FUNCTION:                     MODEL SLOT:
#  setType                       Sets type of portfolio Optimization
#  setEstimator                  Sets name of mean-covariance estimator
# FUNCTION:                     PORTFOLIO SLOT:
#  setWeights                    Sets weights vector
#  setTargetReturn               Sets target return value
#  setRiskFreeRate               Sets risk-free rate value
#  setNFrontierPoints            Sets number of frontier points
#  setReturnRange                Sets range of target returns
#  setRiskRange                  Sets range of target risks 
# FUNCTION:                     Classical and Robust Estimators
#  portfolioStatistics           Estimates mu and Sigma Statistics
################################################################################


test.aaa = 
function()
{
    # Help File:
    helpFile = function() { 
        example(PortfolioExtractors, ask = FALSE)
        return() 
    }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.portfolioSpec =
function()
{ 
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.setType =
function()
{ 
    spec = portfolioSpec()
    spec@model$type

    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.setEstimator =
function()
{ 
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.setWeights =
function()
{ 
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.setTargetReturn =
function()
{ 
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.setRiskFreeRate =
function()
{ 
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.setNFrontierPoints =
function()
{ 
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.setReturnRange =
function()
{ 
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.setRiskRange =
function()
{ 
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.portfolioStatistics =
function()
{ 
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


if (FALSE) {
    require(RUnit)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fPortfolio/test/runit2B.R",
        rngKind = "Marsaglia-Multicarry", rngNormalKind = "Inversion")
    printTextProtocol(testResult)
}
   

################################################################################

