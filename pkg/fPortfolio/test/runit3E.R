
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
# FUNCTION:                              SINGLE PORTFOLIOS:
#  .feasibleConstrainedCVaRPortfolio      Returns a feasible M-CVaR PF
#  .cmlConstrainedCVaRPortfolio           Returns a CML M-CVaR PF
#  .tangencyConstrainedCVaRPortfolio      Returns the tangency M-CVaR PF
#  .minvarianceConstrainedCVaRPortfolio   Returns the minimum var M-CVaR PF
#  .efficientConstrainedCVaRPortfolio     Returns a frontier M-CVaR PF
# FUNCTION:                              PORTFOLIO FRONTIER:
#  .portfolioConstrainedCVaRFrontier      Returns the EF of a  M-CVaR PF
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


# ------------------------------------------------------------------------------


test.feasibleConstrainedCVaRPortfolio =
function()
{
    # Require lpSolve:
    require(lpSolve)
    
    # Data:
    Data = as.timeSeries(data(smallcap.ts))
    Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
    head(Data)
    
    # CVaR Specification:
    Spec = portfolioSpec()
    setType(Spec) = "CVaR"
    setTargetReturn(Spec) = mean(colAvgs(Data))
    setTargetAlpha(Spec) = 0.05
    setSolver(Spec) = "RlpSolve"
    Spec
    
    # Constraints:
    Constraints = NULL
    
    # Portfolio:
    Portfolio = NA
    Portfolio
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


.cmlConstrainedCVaRPortfolio
function()
{
    # Require lpSolve:
    require(lpSolve)
    
    # Data:
    Data = as.timeSeries(data(smallcap.ts))
    Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
    head(Data)
    
    # CVaR Specification:
    Spec = portfolioSpec()
    setType(Spec) = "CVaR"
    setTargetReturn(Spec) = mean(colAvgs(Data))
    setTargetAlpha(Spec) = 0.05
    setSolver(Spec) = "RlpSolve"
    Spec
    
    # Constraints:
    Constraints = NULL
    
    # Portfolio:
    Portfolio = NA
    Portfolio
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------

.tangencyConstrainedCVaRPortfolio
function()
{
    # Require lpSolve:
    require(lpSolve)
    
    # Data:
    Data = as.timeSeries(data(smallcap.ts))
    Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
    head(Data)
    
    # CVaR Specification:
    Spec = portfolioSpec()
    setType(Spec) = "CVaR"
    setTargetReturn(Spec) = mean(colAvgs(Data))
    setTargetAlpha(Spec) = 0.05
    setSolver(Spec) = "RlpSolve"
    Spec
    
    # Constraints:
    Constraints = NULL
    
    # Portfolio:
    Portfolio = NA
    Portfolio
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------

test.minvarianceConstrainedCVaRPortfolio =
function()
{
    # Require lpSolve:
    require(lpSolve)
    
    # Data:
    Data = as.timeSeries(data(smallcap.ts))
    Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
    head(Data)
    
    # CVaR Specification:
    Spec = portfolioSpec()
    setType(Spec) = "CVaR"
    setTargetReturn(Spec) = mean(colAvgs(Data))
    setTargetAlpha(Spec) = 0.05
    setSolver(Spec) = "RlpSolve"
    Spec
    
    # Constraints:
    Constraints = NULL
   
    # CVaR Portfolio Optimization:
    Portfolio = .minvarianceConstrainedCVaRPortfolio(Data, Spec, Constraints)
    Portfolio

    # Return Value:
    return()
}

# ------------------------------------------------------------------------------


test.efficientConstrainedCVaRPortfolio = 
function()
{  
    # Require lpSolve:
    require(lpSolve)
    
    # Data:
    Data = as.timeSeries(data(smallcap.ts))
    Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
    head(Data)
    
    # CVaR Specification:
    Spec = portfolioSpec()
    setType(Spec) = "CVaR"
    setTargetReturn(Spec) = mean(colAvgs(Data))
    setTargetAlpha(Spec) = 0.05
    setSolver(Spec) = "RlpSolve"
    Spec
    
    # Constraints
    constraints = NULL
    
    # CVaR Portfolio Optimization:
    ans = efficientPortfolio(Data, Spec, Constraints)
    weights = round(getWeights(ans), 3)
    weights
    
    # Compare with:
    Spec = portfolioSpec()
    setTargetReturn(Spec) = mean(colAvgs(Data))
    Spec
    ans = efficientPortfolio(Data, Spec)
    weights = round(getWeights(ans), 3)
    weights
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.portfolioCVaRFrontier = 
function()
{  
    # Require lpSolve:
    require(lpSolve)
    
    # Data:
    Data = as.timeSeries(data(smallcap.ts))
    Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
    set.seed(4711); Data@Data = matrix(rnorm(4*60), ncol = 4)
    head(Data)
    
    # CVaR Specification:
    Spec = portfolioSpec()
    setType(Spec) = "CVaR"
    setTargetReturn(Spec) = mean(colAvgs(Data))
    setTargetAlpha(Spec) = 0.05
    setSolver(Spec) = "RlpSolve"
    Spec
    
    # Constraints:
    Constraints = NULL
    
    # Graph Frame:
    par(mfrow = c(2, 2), cex = 0.7)
    
    # CVaR Portfolio Optimization:
    CVaR.Frontier = .portfolioConstrainedCVaRFrontier(Data, Spec, Constraints)
    CVaR.Frontier
    plot(getFrontier(CVaR.Frontier))
    
    # Compare with Default:
    Spec = portfolioSpec()
    MV.Frontier = portfolioFrontier(Data, Spec, NULL)
    MV.Frontier
    plot(getFrontier(MV.Frontier))
       
    # Compare Target Risks:
    plot(getTargetRisk(MV.Frontier), getTargetRisk(CVaR.Frontier))
    
    round(getWeights(CVaR.Frontier) - getWeights(MV.Frontier), 3)
     
    # Return Value:
    return()    
}
  

# ------------------------------------------------------------------------------


if (FALSE) {
    require(RUnit)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fPortfolio/test/runit3E.R",
        rngKind = "Marsaglia-Multicarry", rngNormalKind = "Inversion")
    printTextProtocol(testResult)
}
   

################################################################################



.CVaR = 
function (portfolio)
{
    data.mat = as.matrix(portfolio@data$series)
    weights.vec = getWeights(portfolio)
    
    x = as.vector(data.mat %*% weights.vec)
    alpha = getSpecification(portfolio)@portfolio$targetAlpha
    
    VaR = sort(x)[trunc(length(x)*alpha)] 
    CVaR = mean(sort(x)[1:trunc(length(x)*alpha)])
    TCL = mean(x[x<=VaR(x, alpha)]) 
    
    list(VaR = VaR, CVaR = CVaR, TCL = TCL)
}


.example =
function()
{
    data$series = -data$series
    ans = .portfolioConstrainedCVaRFrontier(data, spec, NULL)
    data.mat = as.matrix(ans@data$series)
    weights.mat = getWeights(ans)
    alpha = getSpecification(ans)@portfolio$targetAlpha
    targetRisk1 = targetRisk2 = targetRisk3 = NULL
    for (i in 1:50) {
        x = as.vector(data.mat %*% weights.mat[i,])
        VaR = -sort(x)[trunc(length(x)*alpha)] # VaR
        CVaR = -mean(sort(x)[1:trunc(length(x)*alpha)]) # CVaR
        TCL = -mean(x[x<=VaR(x, alpha)]) # TCL
        targetRisk1 = c(targetRisk1, VaR)
        targetRisk2 = c(targetRisk2, CVaR)
        targetRisk3 = c(targetRisk3, TCL)
    }
    targetReturn = getFrontier(ans)[, 2]
    par(mfrow = c(2,2), cex = 0.7)
    plot(getTargetRisk(ans), targetReturn, type = "o")
    plot(targetRisk1, targetReturn, type = "o", main = "VaR")
    plot(targetRisk2, targetReturn, type = "o", main = "CVaR")
    plot(targetRisk3, targetReturn, type = "o", main = "TCL")
    
    
    
    
}   


# Compare Weights:

# ------------------------------------------------------------------------------

