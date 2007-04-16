
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
#  .tangencyConstrainedMVPortfolio    Returns constrained tangency MV-PF
#  .cmlConstrainedMVPortfolio         Returns constrained CML-Portfolio
#  .minvarianceConstrainedMVPortfolio Returns constrained min-Variance-PF
#  .frontierConstrainedMVPortfolio    Returns a constrained frontier MV-PF
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


# ------------------------------------------------------------------------------


# IMPORTANT NOTE:
#   All .*ConstrainedMV* portfolio and frontier functions assume that the
#   constraints are predefined. If this is not the case, then the default 
#   value "LongOnly" will be used, i.e. 0 <= W_i <= 1, and no other 
#   constraints.


# ------------------------------------------------------------------------------


test.feasibleConstrainedMVPortfolio = 
function()
{
    # IMPORTANT NOTE:
    #   It is not yet checked if the portfolio is feasible, that
    #       means that the weights are compatible with the constraints.
    #       In the current version constraints are ignored!
    #   Note, the weights are taken from "spec@portfolio$eights"
    #       if this entry is NULL, equal weights are used to
    #       compute the portfolio.
    
    # Specification:
    Spec = portfolioSpec()
    Spec
    
    # Data:
    Data = as.timeSeries(data(smallcap.ts))
    Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
    Data
    
    # Constraints:
    Constraints = NULL
    
    # Feasible Portfolio:
    .feasibleConstrainedMVPortfolio(Data, Spec, Constraints)
    
    # Constraints:
    Constraints = "LongOnly"
    
    # Feasible Portfolio:
    .feasibleConstrainedMVPortfolio(Data, Spec, Constraints)
    
    # Random Weights:
    nAssets = ncol(Data)
    Weights = runif(nAssets, 0, 1)
    Weights = Weights/sum(Weights)
    setWeights(Spec) <- Weights

    # Feasible Portfolio:
    .feasibleConstrainedMVPortfolio(Data, Spec, Constraints)
    
    # Return Value:
    spec
}


# ------------------------------------------------------------------------------


test.tangencyConstrainedMVPortfolio = 
function()
{
    # Data:
    Data = as.timeSeries(data(smallcap.ts))
    Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
    Data
    
    # Specification:
    Spec = portfolioSpec()
    Spec
    
    # Tangency Portfolio:
    .tangencyConstrainedMVPortfolio(Data, Spec, "LongOnly")
    .tangencyConstrainedMVPortfolio(Data, Spec, "maxW[1:nAssets]=0.6")
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.cmlConstrainedMVPortfolio = 
function()
{
    # Data:
    Data = as.timeSeries(data(smallcap.ts))
    Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
    Data
    
    # Specification:
    Spec = portfolioSpec()
    Spec
    
    # Feasible Portfolio:
    .cmlConstrainedMVPortfolio(Data, Spec, NULL)
    .cmlConstrainedMVPortfolio(Data, Spec, "LongOnly")
    
    # Modify Risk Free Rate:
    setRiskFreeRate(Spec) = mean(colMeans(Data@Data))
    Spec
    .cmlConstrainedMVPortfolio(Data, Spec, "LongOnly")
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.minvarianceConstrainedMVPortfolio = 
function()
{
    # Data:
    Data = as.timeSeries(data(smallcap.ts))
    Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
    Data
    
    # Specification:
    Spec = portfolioSpec()
    Spec
    
    # Miimum Variance Portfolio:
    .minvarianceConstrainedMVPortfolio(Data, Spec, "LongOnly")
    .minvarianceConstrainedMVPortfolio(Data, Spec, "maxW[1:nAssets]=0.6")
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.efficientConstrainedMVPortfolio = 
function()
{
    # Data:
    Data = as.timeSeries(data(smallcap.ts))
    Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
    Data
    
    # Specification:
    Spec = portfolioSpec()
    Spec
    
    # Modify Target Return:
    targetReturn = mean(colMeans(Data@Data))
    targetReturn
    setTargetReturn(Spec) <- targetReturn
    Spec
    
    # Tangency Portfolio:
    .efficientConstrainedMVPortfolio(Data, Spec, "LongOnly")
    .efficientConstrainedMVPortfolio(Data, Spec, "maxW[1:nAssets]=0.6")
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.portfolioConstrainedMVFrontier =
function()
{
    # Data:
    Data = as.timeSeries(data(smallcap.ts))
    Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
    Data
    
    # Specification:
    Spec = portfolioSpec()
    Spec
    
    # Modify Target Return:
    riskFreeRate = mean(colMeans(Data@Data))
    riskFreeRate
    setRiskFreeRate(Spec) = riskFreeRate
    Spec
    
    # Portfolio Frontier:
    frontier = .portfolioConstrainedMVFrontier(Data, Spec, "LongOnly")
    frontier
    plot(frontier, which = c(1, 2, 5))
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.portfolioConstrainedMVFrontier.RDonlp2 =
function()
{
    # Load RDonlp2 Solver:
    require(RDonlp2)
    
    # Data:
    Data = as.timeSeries(data(smallcap.ts))
    Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
    Data
    
    # Specification:
    Spec = portfolioSpec()
    Spec
    
    # Solver:
    setSolver(Spec) = "RDonlp2"
    Spec
    
    # Portfolio Frontier:
    frontier = .portfolioConstrainedMVFrontier(Data, Spec, "LongOnly")
    frontier
    plot(frontier, which = c(1, 2, 5))
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.portfolioConstrainedMVFrontier.RiskBudgets =
function()
{
    # Load RDonlp2 Solver:
    require(Rdonlp2)
    
    # Data:
    Data = as.timeSeries(data(smallcap.ts))
    Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
    Data
    
    # Specification:
    Spec = portfolioSpec()
    setTargetReturn(Spec)<- mean(colAvgs(Data))
    setSolver(Spec) = "RDonlp2"
    Spec
    
    # Weight Constraints Only:
    Constraints = "LongOnly"
    
    # Portfolio Frontier:
    frontier = .efficientConstrainedMVPortfolio(Data, Spec, Constraints)
    frontier
    
    # Add Risk Budgets:
    Constraints = c("minW[1:4]=0", "maxB[1:4]=0.3")
        
    # Portfolio Frontier:
    # frontier = .efficientConstrainedMVPortfolio(Data, Spec, Constraints)
    portfolioFrontier(Data, Spec, Constraints)
    frontier
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


if (FALSE) {
    require(RUnit)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fPortfolio/test/runit3B.R",
        rngKind = "Marsaglia-Multicarry", rngNormalKind = "Inversion")
    printTextProtocol(testResult)
}
   

################################################################################

