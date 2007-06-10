
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
#  .cmlConstrainedMVPortfolio         Returns constrained CML-Portfolio
#  .tangencyConstrainedMVPortfolio    Returns constrained tangency MV-PF
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
#   All *ConstrainedMV* portfolios and the portfolio frontier function 
#   assume by default zero-one Box constrained weights, i.e. strict
#   "LongOnly" constraints. These can be modified to more general box
#   constraints including limites "Short" constraints, group constraints,
#   and risk budget constraints.


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
    
    # Data:
    Data = as.timeSeries(data(smallcap.ts))
    Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
    head(Data)
    
    # Specification:
    Spec = portfolioSpec()
    Spec
 
    # Constraints:
    Constraints = NULL
    Constraints
    
    # Feasible Portfolio:
    .feasibleConstrainedMVPortfolio(Data, Spec, Constraints)
}


# ------------------------------------------------------------------------------
   
 
test.feasibleConstrainedMVPortfolio.LongOnly = 
function()
{   
    # Data:
    Data = as.timeSeries(data(smallcap.ts))
    Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
    head(Data)
    
    # Specification + Random Weights:
    Spec = portfolioSpec()
    nAssets = ncol(Data)
    Weights = runif(nAssets, 0, 1)
    Weights = Weights/sum(Weights)
    setWeights(Spec) <- Weights
    Spec
    
    # Constraints:
    Constraints = "LongOnly"
    Constraints
    
    # Feasible Portfolio:
    .feasibleConstrainedMVPortfolio(Data, Spec, Constraints)
    
    # Feasible Portfolio:
    portfolio = feasiblePortfolio(Data, Spec, Constraints)
    
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
    head(Data)
    
    # Specification:
    Spec = portfolioSpec()
    Spec
    
    # Feasible Portfolio - Equals Tangency Portfolio:
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


test.tangencyConstrainedMVPortfolio = 
function()
{
    # Data:
    Data = as.timeSeries(data(smallcap.ts))
    Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
    head(Data)
    
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


test.minvarianceConstrainedMVPortfolio = 
function()
{
    # Data:
    Data = as.timeSeries(data(smallcap.ts))
    Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
    head(Data)
    
    # Specification:
    Spec = portfolioSpec()
    Spec
    
    # Minimum Variance Portfolio:
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
    head(Data)
    
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


test.efficientConstrainedMVPortfolio.twoAssets = 
function()
{
    # Data:
    Data = as.timeSeries(data(smallcap.ts))
    Data = Data[, c("BKE", "GG")]
    head(Data)
    
    # Specification:
    Spec = portfolioSpec()
    Spec
    
    # Modify Target Return:
    targetReturn = mean(Data@Data)
    targetReturn
    setTargetReturn(Spec) <- targetReturn
    Spec
    
    # Tangency Portfolio:
    .efficientConstrainedMVPortfolio(Data, Spec, "LongOnly")
    .DEBUG
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.efficientConstrainedMVPortfolio.RDonlp2 = 
function()
{
    # Data:
    Data = as.timeSeries(data(smallcap.ts))
    Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
    head(Data)
    
    # Specification:
    Spec = portfolioSpec()
    setSolver(Spec) = "RDonlp2"
    Spec
    
    # Modify Target Return:
    targetReturn = mean(colMeans(Data@Data))
    targetReturn
    setTargetReturn(Spec) <- targetReturn
    Spec
    
    # Tangency Portfolio:
    .efficientConstrainedMVPortfolio(Data, Spec, NULL)
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
    head(Data)
    
    # Specification:
    Spec = portfolioSpec()
    Spec
    
    # Constraints: 
    Constraints = NULL
    
    # Portfolio Frontier:
    Frontier = .portfolioConstrainedMVFrontier(Data, Spec, Constraints)
    Frontier
    plot(Frontier, which = c(1, 2, 3, 5))
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.portfolioConstrainedMVFrontier.RDonlp2 =
function()
{   
    # Data:
    Data = as.timeSeries(data(smallcap.ts))
    Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
    head(Data)
    
    # Specification:
    Spec = portfolioSpec()
    setSolver(Spec) = "RDonlp2"
    Spec
    
    # Constraints: 
    Constraints = NULL                        
    
    # Portfolio Frontier:
    Frontier = .portfolioConstrainedMVFrontier(Data, Spec, Constraints)
    Frontier
    plot(Frontier, which = c(1, 2, 3, 5))
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.portfolioConstrainedMVFrontier.RiskBudgets =
function()
{   
    # Data:
    Data = as.timeSeries(data(smallcap.ts))
    Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
    head(Data)
    
    # Specification:
    Spec = portfolioSpec()
    setTargetReturn(Spec)<- mean(colAvgs(Data))
    setSolver(Spec) = "RDonlp2"
    Spec
    
    # Weight Constraints Only:
    Constraints = "LongOnly"
    
    # Portfolio Frontier:
    Frontier = .efficientConstrainedMVPortfolio(Data, Spec, Constraints)
    Frontier
    
    # Add Risk Budgets:
    Constraints = c("minW[1:4]=0", "maxB[1:4]=0.3")
        
    # Portfolio Frontier:
    # Frontier = portfolioFrontier(Data, Spec, Constraints)
    # Frontier                                                       # CHECK !!!
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


if (FALSE) {
    require(RUnit)
    require(Rdonlp2)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fPortfolio/test/runit3B.R",
        rngKind = "Marsaglia-Multicarry", rngNormalKind = "Inversion")
    printTextProtocol(testResult)
}
   

################################################################################

