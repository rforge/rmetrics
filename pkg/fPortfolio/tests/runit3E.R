
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
# FUNCTION:                            SINGLE PORTFOLIOS:
#  .feasibleConstrainedCVaRPortfolio    Returns constrained feasible M-CVaR PF
#  .efficientConstrainedCVaRPortfolio   Returns constrained frontier M-CVaR PF
#  .cmlConstrainedCVaRPortfolio         Returns constrained CML M-CVaR PF
#  .tangencyConstrainedCVaRPortfolio    Returns constrained tangency M-CVaR PF
#  .minvarianceConstraineCVaRPortfolio  Returns constrained min-Var M-CVaR PF
# FUNCTION:                            PORTFOLIO FRONTIER:
#  .portfolioConstrainedCVaRFrontier    Returns EF of a constrained M-CVaR PF
################################################################################


test.aaa = 
function()
{
    # Help File:
    helpFile = function() { 
        example(ConstrainedCVaRPortfolio, ask = FALSE)
        return() 
    }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
}


################################################################################


test.feasibleConstrainedCVaRPortfolio =
function()
{
    # Require lpSolve:
    require(lpSolve)
    
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # CVaR Specification:
    spec = portfolioSpec()
    setType(spec) = "CVaR"
    setWeights(spec) = rep(1/4, 4)
    setTargetAlpha(spec) = 0.05
    setSolver(spec) = "lpSolve"
    spec
    
    # Constraints:
    constraints = NULL
    constraints
    
    # CVaR Portfolio:
    Portfolio = .feasibleConstrainedCVaRPortfolio(data, spec, constraints)
    Portfolio
    
    # Return Value:
    return()
}


################################################################################


test.efficientConstrainedCVaRPortfolio.SmallCaps = 
function()
{  
    # Require lpSolve:
    require(lpSolve)
    
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # CVaR Specification:
    spec = portfolioSpec()
    setType(spec) = "CVaR"
    setTargetReturn(spec) = mean(data@Data)
    setTargetAlpha(spec) = 0.05
    setSolver(spec) = "lpSolve"
    spec
    
    # Constraints:
    constraints = NULL
    constraints
    
    # CVaR Portfolio Optimization:
    Portfolio = .efficientConstrainedCVaRPortfolio(data, spec, constraints)
    Portfolio 
    
    # Return Value:
    return()    
}


################################################################################

   
test.efficientConstrainedCVaRPortfolio.LPP = 
function()
{  
    # Require lpSolve:
    require(lpSolve)
    
    # Second Example:
    data = 100*as.timeSeries(data(LPP2005REC))[, 1:6]
    head(data)
    
    # CVaR Specification:
    spec = portfolioSpec()
    setType(spec) = "CVaR"
    setTargetReturn(spec) = mean(data@Data)
    setTargetAlpha(spec) = 0.05
    setSolver(spec) = "lpSolve"
    spec
    
    # Constraints:
    constraints = NULL
    constraints
    
    # CVaR Portfolio Optimization:
    Portfolio = .efficientConstrainedCVaRPortfolio(data, spec, constraints)
    Portfolio
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.efficientConstrainedCVaRPortfolio.TwoAssets =
function()
{
    # Require lpSolve:
    require(lpSolve)
    
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG")]
    head(data)
    
    # CVaR Specification:
    spec = portfolioSpec()
    setType(spec) = "CVaR"
    setTargetReturn(spec) = mean(data@Data)
    setTargetAlpha(spec) = 0.05
    setSolver(spec) = "lpSolve"
    spec
    
    # Constraints:
    constraints = NULL
    constraints
    
    # CVaR Portfolio:
    Portfolio = .efficientConstrainedCVaRPortfolio(data, spec, constraints)
    Portfolio 
    
    # Return Value:
    return()
}



################################################################################


test.cmlConstrainedCVaRPortfolio = 
function()
{
    # Require lpSolve:
    require(lpSolve)
    
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # CVaR Specification:
    spec = portfolioSpec()
    setType(spec) = "CVaR"
    setTargetReturn(spec) = mean(data@Data)
    setTargetAlpha(spec) = 0.05
    setSolver(spec) = "lpSolve"
    setRiskFreeRate(spec) = mean(data@Data)/10
    spec
    
    # Constraints:
    constraints = NULL
    constraints
    
    # CVaR Portfolio:
    Portfolio = .cmlConstrainedCVaRPortfolio(data, spec, constraints)
    Portfolio
    
    # Return Value:
    return()
}


################################################################################


test.tangencyConstrainedCVaRPortfolio = 
function()
{
    # Require lpSolve:
    require(lpSolve)
    
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # CVaR Specification:
    spec = portfolioSpec()
    setType(spec) = "CVaR"
    setTargetReturn(spec) = mean(data@Data)
    setTargetAlpha(spec) = 0.10
    setSolver(spec) = "lpSolve"
    spec
    
    # Constraints:
    constraints = NULL
    constraints
    
    # Portfolio:
    Portfolio = .tangencyConstrainedCVaRPortfolio(data, spec, constraints)
    Portfolio
    
    # Return Value:
    return()
}


################################################################################


test.minvarianceConstrainedCVaRPortfolio =
function()
{
    # Require lpSolve:
    require(lpSolve)
    
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # CVaR Specification:
    spec = portfolioSpec()
    setType(spec) = "CVaR"
    setTargetAlpha(spec) = 0.05
    setSolver(spec) = "lpSolve"
    spec
    
    # Constraints:
    constraints = NULL
    constraints
    
    # CVaR Portfolio Optimization:
    Portfolio = .minvarianceConstrainedCVaRPortfolio(data, spec, constraints)
    Portfolio 
    
    # Return Value:
    return()
}


################################################################################


test.portfolioConstrainedCVaRFrontier = 
function()
{  
    # Require lpSolve:
    require(lpSolve)
    
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # CVaR Specification:
    spec = portfolioSpec()
    setType(spec) = "CVaR"
    setNFrontierPoints(spec) = 10
    spec
    
    # Constraints:
    constraints = NULL
    constraints
    
    # CVaR Portfolio Optimization:
    Frontier = .portfolioConstrainedCVaRFrontier(data, spec, constraints)
    Frontier
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.portfolioConstrainedCVaRFrontier.TwoAssets =
function()
{
    # Require lpSolve:
    require(lpSolve)
    
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG")]
    head(data)
    
    # CVaR Specification:
    spec = portfolioSpec()
    setType(spec) = "CVaR"
    spec
    
    # Constraints:
    constraints = NULL
    constraints
    
    # CVaR Portfolio:
    Frontier = .portfolioConstrainedCVaRFrontier(data, spec, constraints)
    Frontier
    
    # Return Value:
    return()
}


################################################################################


if (FALSE) {
    require(RUnit)
    require(lpSolve)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fPortfolio/tests/runit3E.R",
        rngKind = "Marsaglia-Multicarry", rngNormalKind = "Inversion")
    printTextProtocol(testResult)
}
   

################################################################################

    