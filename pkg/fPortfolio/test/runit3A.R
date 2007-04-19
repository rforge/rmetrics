
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
# FUNCTION:                      SINGLE PORTFOLIOS:
#  .feasibleShortMVPortfolio      Returns a feasible MV portfolio
#  .cmlShortMVPortfolio           Returns a capital market line
#  .tangencyShortMVPortfolio      Returns the tangency MV portfolio
#  .minvarianceShortMVPortfolio   Returns the minimum variance portfolio
#  .frontierShortMVPortfolio      Returns a frontier MV portfolio
# FUNCTION:                      PORTFOLIO FRONTIER:
#  .portfolioShortMVFrontier      Returns the EF of a short selling MV portfolio
################################################################################


test.aaa = 
function()
{
    # Help File:
    helpFile = function() { 
        example(ShortMVPortfolio, ask = FALSE)
        return() 
    }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


# IMPORTANT NOTE:
#
#   All *ShortMV* portfolios and the portfolio frontier function assume no 
#   constraints on weights, neither box nor group constraints. All Weights 
#   can take unlimited values in the range [-Inf, Inf].
#
#   Note, all these functions are internal functions, not thought to be used 
#   by the enduser of the fPortfolio Package.


# ------------------------------------------------------------------------------


test.feasibleShortMVPortfolio = 
function()
{  
    # Data:
    Data = as.timeSeries(data(smallcap.ts))
    Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
    head(Data)
    
    # Specification - Equal Weights Portfolio:
    Spec = portfolioSpec()
    Spec
    
    # Constraints are ignored:
    Constraints = NULL
    .feasibleShortMVPortfolio(Data, Spec, Constraints)
    
    # Constraints can natursally also defined as:
    Constraints = "Short"
    .feasibleShortMVPortfolio(Data, Spec, Constraints)
    
    # Remember, it is not yet checked if the predefined weights
    #   are compatible with the specified constraints!
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.cmlShortMVPortfolio = 
function()
{
    # Data:
    Data = as.timeSeries(data(smallcap.ts))
    Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
    head(Data)
    
    # Specification:
    Spec = portfolioSpec()
    setRiskFreeRate = 0.01
    Spec
    
    # Constraints - Capital Market Line:
    Constraints = "Short"
    .cmlShortMVPortfolio(Data, Spec, Constraints)
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.tangencyShortMVPortfolio = 
function()
{
    # Data:
    Data = as.timeSeries(data(smallcap.ts))
    Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
    head(Data)
    
    # Specification:
    Spec = portfolioSpec()
    Spec
  
    # Constraints - Tangency Portfolio:
    Constraints = "Short"
    .tangencyShortMVPortfolio(Data, Spec, Constraints)
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.minvarianceShortMVPortfolio = 
function()
{
    # Data:
    Data = as.timeSeries(data(smallcap.ts))
    Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
    head(Data)
    
    # Specification:
    Spec = portfolioSpec()
    Spec
    
    # Constraints - Minimum Variance Portfolio:
    Constraints = "Short"
    .minvarianceShortMVPortfolio(Data, Spec, Constraints)
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.efficientShortMVPortfolio = 
function()
{
    # Data:
    Data = as.timeSeries(data(smallcap.ts))
    Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
    head(Data)
    
    # Specification:
    Spec = portfolioSpec()
    setTargetReturn(Spec) = 0.15
    Spec
    
    # Constraints - Efficient Portfolio:
    Constraints = "Short" 
    .efficientShortMVPortfolio(Data, Spec, Constraints)
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.portfolioShortMVFrontier = 
function()
{ 
    # Data:
    Data = as.timeSeries(data(smallcap.ts))
    Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
    head(Data)
    
    # Specification:
    Spec = portfolioSpec()
    Spec
    
    # Constraints:
    Constraints = "Short" 
    
    # Frontier:
    .portfolioShortMVFrontier(Data, Spec, Constraints)
    Frontier = portfolioFrontier(Data, Spec, Constraints)
    Frontier
    plot(Frontier, which = c(1, 2, 3, 5))
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


if (FALSE) {
    require(RUnit)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fPortfolio/test/runit3A.R",
        rngKind = "Marsaglia-Multicarry", rngNormalKind = "Inversion")
    printTextProtocol(testResult)
}
   

################################################################################