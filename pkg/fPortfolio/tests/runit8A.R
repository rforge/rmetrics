
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
# FUNCTION:            DESCRIPTION:
#  VaR                  Computes value-at-risk for a portfolio of assets
#  CVaRplus             Computes value-at-risk Plus for a portfolio of assets
#  CVaR                 Computes conditional value-at-risk for a PF of assets
#  lambdaCVaR           Computes CVaR's atomic split value lambda
# FUNCTION:            BENCHMARKS:   
#  pfolioMaxLoss        Computes maximum loss for a portfolio of assets
#  pfolioReturn         Computes return series of a portfolio
#  pfolioTargetReturn   Computes target return of a portfolio
#  pfolioTargetRisk     Computes target risk of a portfolio
#  pfolioHist           Plots a histogram of the returns of a portfolio                    
################################################################################


test.aaa = 
function()
{
    # Help File:
    helpFile = function() { 
        example(VaRModelling, ask = FALSE)
        return() 
    }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.VaR =
function()
{ 
    NA
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.CVaRplus =
function()
{ 
    NA
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.CVaR =
function()
{ 
    NA
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.lambdaCVaR =
function()
{ 
    NA
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.pfolioMaxLoss =
function()
{ 
    NA
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.pfolioReturn =
function()
{ 
    NA
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.pfolioTargetReturn =
function()
{ 
    NA
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.pfolioTargetRisk =
function()
{ 
    NA
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.pfolioHist =
function()
{ 
    NA
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


if (FALSE) {
    require(RUnit)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fPortfolio/tests/runit8A.R",
        rngKind = "Marsaglia-Multicarry", rngNormalKind = "Inversion")
    printTextProtocol(testResult)
}
   

################################################################################

