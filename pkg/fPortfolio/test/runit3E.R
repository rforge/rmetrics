
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
# FUNCTION:                            DESCRIPTION:
#  .varRisk                             Computes Value at Risk
#  .cvarRisk                            Computes Conditional Value at Risk
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


# ------------------------------------------------------------------------------


test.CVaRPortfolioSpec = 
function()
{
    # This Function may be added for conveniance ...
    .CVaRPortfolioSpec = 
    function()
    {
        Spec = portfolioSpec()
        setType(Spec) = "CVaR"
        setTargetAlpha(Spec) = 0.05
        setSolver(Spec) = "RlpSolve"
        Spec
    }  
    
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
    setWeights(Spec) = rep(1/4, 4)
    setTargetAlpha(Spec) = 0.05
    setSolver(Spec) = "RlpSolve"
    Spec
    
    # Constraints:
    Constraints = NULL
    Constraints
    
    # CVaR Portfolio:
    cvarPortfolio = feasiblePortfolio(Data, Spec, Constraints)
    cvarPortfolio
    
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
    
    # Constraints:
    Constraints = NULL
    Constraints
    
    # CVaR Portfolio Optimization:
    cvarPortfolio = efficientPortfolio(Data, Spec, Constraints)
    cvarPortfolio
    
    # Compare with Mean Variance Portfolio:
    Spec = portfolioSpec()
    setTargetReturn(Spec) = mean(colAvgs(Data))
    covPortfolio = efficientPortfolio(Data, Spec)
    covPortfolio
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------

   
test.efficientConstrainedCVaRPortfolio.2 = 
function()
{  
    # Second Example:
    Data = 100*as.timeSeries(data(LPP2005REC))[, 1:6]
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
    Constraints
    
    # CVaR Portfolio Optimization:
    cvarPortfolio = efficientPortfolio(Data, Spec, Constraints)
    cvarPortfolio
    weights = round(getWeights(cvarPortfolio), 3)
    weights
    par(mfrow = c(2, 2), cex = 0.7)
    weightsPie(cvarPortfolio)
    title(main = "CVaR Portfolio")
    attributesPie(cvarPortfolio)
    title(main = "CVaR Portfolio")
    
    # Compare with Mean Variance Portfolio:
    Spec = portfolioSpec()
    setTargetReturn(Spec) = mean(colAvgs(Data))
    covPortfolio = efficientPortfolio(Data, Spec)
    covPortfolio
    weights = round(getWeights(covPortfolio), 3)
    weights
    weightsPie(covPortfolio)
    title(main = "MV Portfolio")
    attributesPie(covPortfolio)
    title(main = "MV Portfolio")
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.cmlConstrainedCVaRPortfolio = 
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
    setRiskFreeRate(Spec) = mean(colAvgs(Data))/10
    Spec
    
    # Constraints:
    Constraints = NULL
    Constraints
    
    # Graph Frame:
    par(mfrow = c(2, 2), cex = 0.7)
    
    # CVaR Portfolio:
    cvarPortfolio = cmlPortfolio(Data, Spec, Constraints)
    cvarPortfolio
    weights = round(getWeights(cvarPortfolio), 3)
    weights
    weightsPie(cvarPortfolio)
    title(main = "CVaR Portfolio")
      
    # Compare with Mean Variance Portfolio:
    Spec = portfolioSpec()
    setTargetReturn(Spec) = mean(colAvgs(Data))
    covPortfolio = cmlPortfolio(Data, Spec)
    weights = round(getWeights(covPortfolio), 3)
    weights
    weightsPie(covPortfolio)
    title(main = "MV Portfolio")
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------


test.tangencyConstrainedCVaRPortfolio = 
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
    setTargetAlpha(Spec) = 0.10
    setSolver(Spec) = "RlpSolve"
    Spec
    
    # Constraints:
    Constraints = NULL
    Constraints
    
    # Portfolio:
    cvarPortfolio = tangencyPortfolio(Data, Spec, Constraints)
    cvarPortfolio
    
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
    setTargetAlpha(Spec) = 0.05
    setSolver(Spec) = "RlpSolve"
    Spec
    
    # Constraints:
    Constraints = NULL
    Constraints
    
    # CVaR Portfolio Optimization:
    cvarPortfolio = minvariancePortfolio(Data, Spec, Constraints)
    cvarPortfolio
    weights = round(getWeights(cvarPortfolio), 3)
    weights
    par(mfrow = c(2, 2), cex = 0.7)
    weightsPie(cvarPortfolio)
    title(main = "Minimum-CVaR Portfolio")
      
    # Compare with Mean Variance Portfolio:
    Spec = portfolioSpec()
    setTargetReturn(Spec) = mean(colAvgs(Data))
    covPortfolio = minvariancePortfolio(Data, Spec)
    covPortfolio
    weights = round(getWeights(covPortfolio), 3)
    weights
    weightsPie(covPortfolio)
    title(main = "Minimum-Variance Portfolio")
    
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
    head(Data)
    
    # CVaR Specification:
    Spec = portfolioSpec()
    setType(Spec) = "CVaR"
    setTargetAlpha(Spec) = 0.05
    setSolver(Spec) = "RlpSolve"
    Spec
    
    # Constraints:
    Constraints = NULL
    Constraints
    
    # Graph Frame:
    par(mfrow = c(2, 2), cex = 0.7)
    
    # CVaR Portfolio Optimization:
    cvarFrontier = portfolioFrontier(Data, Spec, Constraints)
    cvarFrontier
    weightsPlot(cvarFrontier)
    plot(cvarFrontier, which = 1:6)
    
    # Compare with Mean-Variance Portfolio:
    Spec = portfolioSpec()
    covFrontier = portfolioFrontier(Data, Spec)
    covFrontier
    weightsPlot(covFrontier)
    plot(covFrontier, which = 1:6)
     
    # Return Value:
    return()    
}


################################################################################


test.Scherer.1 =
function()
{
    .CVaR = 
    function(object, alpha = 0.05)
    {
        data = as.matrix(object@data$series)
        weights = getWeights(object)
        
        x = as.numeric(data %*% weights)       
        VaR = sort(x)[trunc(length(x)*alpha)] 
        CVaR = mean(sort(x)[1:trunc(length(x)*alpha)])
        TCL = mean(x[x <= VaR(x, alpha)]) 
        
        list(VaR = VaR, CVaR = CVaR, TCL = TCL)
    }
    
    Spec = portfolioSpec()
    covPortfolio = tangencyPortfolio(Data, Spec)
    covPortfolio
    
    .CVaR(covPortfolio, alpha = 0.10)
  
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.Scherer.2 =
function()
{
    # A VERY IMPORTANT CHECK!
    
    # Load Swiss SPI and Swiss Immofunds SII
    Data = 100 * as.timeSeries(data(LPP2005REC))[, c("SPI", "SII")]
    head(Data)
    
    # Create Equidistant Weights from 0 to 1:
    nWeights = 21
    W1 = seq(0, 1, length = nWeights)
    W2 = 1 - W1
  
    # Create Points where to calculate the VaR:
    nVaR = nWeights - 1
    Returns = as.numeric( cbind(W1, W2) %*% colMeans(Data@Data))
    sReturns = sort(Returns)
    VaR = sReturns[-nWeights] + diff(sReturns)/2

    # Set alpha and compute CVaR on "nWeights x nVaR" Grid:
    alpha = 0.20
    CVaR = matrix(rep(0, nWeights*nVaR), ncol = nVaR)
    for (i in 1:nWeights) {
        for (j in 1:nVaR) {  
            RET = W1[i] * Data@Data[, 1] + W2[i] * Data@Data[, 2]
            es = ( (-VaR[j] - RET) + abs(-VaR[j] - RET) ) / 2
            CVaR[i, j] = -VaR[j] - mean(es)/alpha
        }
    }
    rownames(CVaR) = substr(as.character(W1+0.001), 1, 4)
    colnames(CVaR) = substr(as.character(VaR+0.0000001), 1, 5)
    round(CVaR, 3)
    persp(W1, VaR, CVaR, theta = 120, phi = 30, col = "steelblue", 
        ps = 9, ticktype = "detailed", expand = 0.6, r = 500)
   
    # Compute Example (e.g. Case 7) for VaR and CVaR from Scratch:
    S = 7   
    x = W1[S] * Data@Data[, 1] + W2[S] * Data@Data[, 2]
    VaR = quantile(x, alpha, type = 1)
    CVaR = VaR - 0.5*mean(((VaR-x) + abs(VaR-x)))/alpha
      
    # CVaR Portfolio Specification:
    Spec = portfolioSpec()
    setType(Spec) = "CVaR"
    setTargetReturn(Spec) = mean(x)
    setTargetAlpha(Spec) = alpha
    setSolver(Spec) = "RlpSolve"
   
    # CVaR Portfolio Optimization:
    myPortfolio = efficientPortfolio(Data, Spec, NULL)
    optVaR = -.DEBUG$solution[1]
    optCVaR = .DEBUG$objval   
    
    # Compare Results from Scratch and Optimization:
    c(VaR, optVaR)
    c(CVaR, optCVaR)
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


if (FALSE) {
    require(RUnit)
    require(lpSolve)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fPortfolio/test/runit3E.R",
        rngKind = "Marsaglia-Multicarry", rngNormalKind = "Inversion")
    printTextProtocol(testResult)
}
   

################################################################################

    