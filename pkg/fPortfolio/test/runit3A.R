
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
#  .efficientShortMVPortfolio     Returns a frontier MV portfolio
#  .cmlShortMVPortfolio           Returns a capital market line
#  .tangencyShortMVPortfolio      Returns the tangency MV portfolio
#  .minvarianceShortMVPortfolio   Returns the minimum variance portfolio
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
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # Specification - Equal Weights Portfolio:
    spec = portfolioSpec()
    spec
    
    # Constraints are ignored:
    constraints = NULL
    constraints
    
    # Portfolio:
    Portfolio = .feasibleShortMVPortfolio(data, spec, constraints)
    Portfolio
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.feasibleShortMVPortfolio.Short = 
function()
{  
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # Specification - Equal Weights Portfolio:
    spec = portfolioSpec()
    spec
    
    # Constraints can natursally also defined as:
    constraints = "Short"
    constraints
    
    # Portfolio:
    .feasibleShortMVPortfolio(data, spec, constraints)
    
    # Return Value:
    return()
}


################################################################################


test.efficientShortMVPortfolio = 
function()
{
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # Specification:
    spec = portfolioSpec()
    setTargetReturn(spec) = 0.15
    spec
    
    # Constraints - Efficient Portfolio:
    constraints = "Short" 
    constraints
    
    # Portfolio:
    .efficientShortMVPortfolio(data, spec, constraints)
    
    # Return Value:
    return()
}


################################################################################


test.cmlShortMVPortfolio = 
function()
{
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # Specification:
    spec = portfolioSpec()
    setRiskFreeRate(spec) = 0.01
    spec
    
    # Constraints - Capital Market Line:
    constraints = "Short"
    constraints
    
    # Portfolio:
    .cmlShortMVPortfolio(data, spec, constraints)
    
    # Return Value:
    return()
}


################################################################################


test.tangencyShortMVPortfolio = 
function()
{
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # Specification:
    spec = portfolioSpec()
    spec
  
    # Constraints - Tangency Portfolio:
    constraints = "Short"
    constraints
    
    # Portfolio:
    .tangencyShortMVPortfolio(data, spec, constraints)
    
    # Return Value:
    return()
}


################################################################################


test.minvarianceShortMVPortfolio = 
function()
{
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # Specification:
    spec = portfolioSpec()
    spec
    
    # Constraints - Minimum Variance Portfolio:
    constraints = "Short"
    constraints
    
    # Portfolio:
    Portfolio = .minvarianceShortMVPortfolio(data, spec, constraints)
    Portfolio 
    
    # Return Value:
    return()
}


################################################################################


test.portfolioShortMVFrontier = 
function()
{ 
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # Specification:
    spec = portfolioSpec()
    spec
    
    # Constraints:
    constraints = "Short" 
    constraints
    
    # Frontier:
    Frontier = .portfolioShortMVFrontier(data, spec, constraints)
    Frontier
    
    # Return Value:
    return()
}


################################################################################


if (FALSE) {
    require(RUnit)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fPortfolio/test/runit3A.R",
        rngKind = "Marsaglia-Multicarry", rngNormalKind = "Inversion")
    printTextProtocol(testResult)
}
   

################################################################################