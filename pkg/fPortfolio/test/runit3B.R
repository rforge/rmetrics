
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
# FUNCTION:                          SINGLE PORTFOLIOS:
#  .feasibleConstrainedMVPortfolio    Returns a constrained feasible MV-PF
#  .efficientConstrainedMVPortfolio   Returns a constrained frontier MV-PF
#  .cmlConstrainedMVPortfolio         Returns constrained CML-Portfolio
#  .tangencyConstrainedMVPortfolio    Returns constrained tangency MV-PF
#  .minvarianceConstrainedMVPortfolio Returns constrained min-Variance-PF
# FUNCTION:                          PORTFOLIO FRONTIER:
#  .portfolioConstrainedMVFrontier    Returns the EF of a constrained MV-PF                    
################################################################################


test.aaa = 
function()
{
    # Help File:
    helpFile = function() { 
        example(ConstrainedMVPortfolio, ask = FALSE)
        return() 
    }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
}


################################################################################


test.feasibleConstrainedMVPortfolio = 
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
    constraints = NULL
    constraints
    
    # Feasible Portfolio:
    Portfolio = .feasibleConstrainedMVPortfolio(data, spec, constraints)
    Portfolio
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------
   
 
test.feasibleConstrainedMVPortfolio.LongOnly = 
function()
{   
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # Specification + Random Weights:
    spec = portfolioSpec()
    nAssets = ncol(Data)
    Weights = runif(nAssets, 0, 1)
    Weights = Weights/sum(Weights)
    setWeights(spec) <- Weights
    spec
    
    # Constraints:
    constraints = "LongOnly"
    constraints
    
    # Feasible Portfolio:
    Portfolio = .feasibleConstrainedMVPortfolio(data, spec, constraints)
    Portfolio
    
    # Return Value:
    return()
}


################################################################################


test.efficientConstrainedMVPortfolio.LongOnly = 
function()
{
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # Specification:
    spec = portfolioSpec()
    setTargetReturn(spec) <- mean(seriesData(data))
    spec
    
    # Efficient Portfolio:
    Portfolio = .efficientConstrainedMVPortfolio(data, spec, "LongOnly")
    Portfolio
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.efficientConstrainedMVPortfolio.Tailored = 
function()
{
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # Specification:
    spec = portfolioSpec()
    setTargetReturn(spec) <- mean(seriesData(data))
    spec
    
    # Consgtraints:
    constraints = "maxW[1:nAssets]=0.6"
    constraints
    
    # Efficient Portfolio:
    Portfolio = .efficientConstrainedMVPortfolio(data, spec, constraints)
    Portfolio
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.efficientConstrainedMVPortfolio.Rdonlp2 = 
function()
{
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # Specification:
    spec = portfolioSpec()
    setTargetReturn(spec) <- mean(seriesData(data))
    setSolver(spec) = "Rdonlp2"
    spec
    
    # Constraints:
    constraints = "maxW[1:nAssets]=0.6"
    constraints
    
    # Efficient Portfolio:
    Portfolio = .efficientConstrainedMVPortfolio(data, spec, constraints)
    Portfolio
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.efficientConstrainedMVPortfolio.twoAssets = 
function()
{
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG")]
    head(data)
    
    # Specification:
    spec = portfolioSpec()
    setTargetReturn(spec) = mean(data@Data)
    spec
    
    # Constraints:
    constraints = "LongOnly"
    constraints
    
    # Efficient Portfolio:
    Portfolio = .efficientConstrainedMVPortfolio(data, spec, constraints)
    Portfolio
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.efficientConstrainedMVPortfolio.RDonlp2 = 
function()
{
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # Specification:
    spec = portfolioSpec()
    setTargetReturn(spec) = mean(seriesData(data))
    setSolver(spec) = "Rdonlp2"
    spec
    
    # Tangency Portfolio:
    .efficientConstrainedMVPortfolio(data, spec, NULL)
    .efficientConstrainedMVPortfolio(data, spec, "LongOnly")
    .efficientConstrainedMVPortfolio(data, spec, "maxW[1:nAssets]=0.6")
    
    # Return Value:
    return()
}


################################################################################


test.cmlConstrainedMVPortfolio = 
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
    constraints = NULL
    constraints
    
    # Portfolio - Equals tangency Portfolio:
    Portfolio = .cmlConstrainedMVPortfolio(data, spec, constraints)
    Portfolio
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.cmlConstrainedMVPortfolio.LongOnly = 
function()
{
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # Specification:
    spec = portfolioSpec()
    spec
    
    # CML Portfolio - Equals Tangency Portfolio:
    constraints = "LongOnly"
    constraints
    
    # Portfolio - Equals tangency Portfolio:
    Portfolio = .cmlConstrainedMVPortfolio(data, spec, constraints)
    Portfolio
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.cmlConstrainedMVPortfolio.RiskFreeRate = 
function()
{
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # Specification:
    spec = portfolioSpec()
    setRiskFreeRate(spec) = mean(seriesData(data))
    spec
    
    # Constraints:
    constraints = "LongOnly"
    constraints
    
    # Portfolio:
    Portfolio = .cmlConstrainedMVPortfolio(data, spec, constraints)
    Portfolio 
    
    # Return Value:
    return()
}


################################################################################


test.tangencyConstrainedMVPortfolio.LongOnly = 
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
    constraints = "LongOnly"
    constraints
    
    # Tangency Portfolio:
    Portfolio = .tangencyConstrainedMVPortfolio(data, spec,  constraints)
    Portfolio
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.tangencyConstrainedMVPortfolio.Tailored = 
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
    constraints = "maxW[1:nAssets]=0.6"
    constraints
    
    # Tangency Portfolio:
    Portfolio = .tangencyConstrainedMVPortfolio(data, spec, constraints)
    Portfolio 
    
    # Return Value:
    return()
}


################################################################################


test.minvarianceConstrainedMVPortfolio.LongOnly = 
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
    constraints = "LongOnly"
    constraints
    
    # Minimum Variance Portfolio:
    Portfolio = .minvarianceConstrainedMVPortfolio(data, spec, constraints)
    Portfolio 
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.minvarianceConstrainedMVPortfolio.Tailored = 
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
    constraints = "maxW[1:nAssets]=0.6"
    constraints
    
    # Minimum Variance Portfolio:
    Portfolio = .minvarianceConstrainedMVPortfolio(data, spec, constraints)
    Portfolio
    
    # Return Value:
    return()
}


################################################################################


test.portfolioConstrainedMVFrontier =
function()
{
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # Specification:
    spec = portfolioSpec()
    setNFrontierPoints(spec) = 5
    spec
    
    # Constraints: 
    constraints = NULL
    constraints
    
    # Portfolio Frontier:
    Frontier = .portfolioConstrainedMVFrontier(data, spec, constraints)
    Frontier
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.portfolioConstrainedMVFrontier.RDonlp2 =
function()
{   
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # Specification:
    spec = portfolioSpec()
    setSolver(spec) = "Rdonlp2"
    setNFrontierPoints(spec) = 5
    spec
    
    # Constraints: 
    constraints = NULL                        
    constraints
    
    # Portfolio Frontier:
    Frontier = .portfolioConstrainedMVFrontier(data, spec, constraints)
    Frontier 
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.portfolioConstrainedMVFrontier.RiskBudgets =
function()
{   
    # Load:
    require(Rdonlp2)
    
    # Data:
    data = as.timeSeries(data(smallcap.ts))
    data = data[, c("BKE", "GG", "GYMB", "KRON")]
    head(data)
    
    # Specification:
    spec = portfolioSpec()
    setSolver(spec) = "Rdonlp2"
    setNFrontierPoints(spec) = 10
    spec
    
    # Add Risk Budgets:
    constraints = c("minW[1:4]=0", "maxB[1:4]=0.8")
    constraints
       
    # Frontier:
    Frontier = .portfolioConstrainedMVFrontier(data, spec, constraints)
    Frontier
      
    # Return Value:
    return()
}


################################################################################


if (FALSE) {
    require(RUnit)
    require(Rdonlp2)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fPortfolio/test/runit3B.R",
        rngKind = "Marsaglia-Multicarry", rngNormalKind = "Inversion")
    printTextProtocol(testResult)
}
   

################################################################################

