
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


# ------------------------------------------------------------------------------


test.CVaRPortfolioSpec = 
function()
{
    CVaRPortfolioSpec = 
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
    
    # CVaR Portfolio:
    myPortfolio = feasiblePortfolio(Data, Spec, Constraints)
    myPortfolio
    
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
    
    colMeans(Data@Data)
    
    # CVaR Specification:
    Spec = portfolioSpec()
    setType(Spec) = "CVaR"
    setTargetReturn(Spec) = 0.035706 # mean(colAvgs(Data))
    setTargetAlpha(Spec) = 0.05
    setSolver(Spec) = "RlpSolve"
    Spec
    
    # Constraints:
    Constraints = NULL
    
    # CVaR Portfolio Optimization:
    cvarPortfolio = efficientPortfolio(Data, Spec, Constraints)
    cvarPortfolio
    
    # Compare with Mean Variance Portfolio:
    Spec = portfolioSpec()
    setTargetReturn(Spec) = mean(colAvgs(Data))
    Spec
    meanvarPortfolio = efficientPortfolio(Data, Spec)
    meanvarPortfolio
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------

   
test.efficientConstrainedCVaRPortfolio.2 = 
function()
{  
    # Second Example:
    Data = 100*as.timeSeries(data(LPP2005REC))[, 1:6]
    
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
    ans = efficientPortfolio(Data, Spec, Constraints)
    weights = round(getWeights(ans), 3)
    weights
    par(mfrow = c(2, 2), cex = 0.7)
    weightsPie(ans)
    title(main = "CVaR Portfolio")
    attributesPie(ans)
    title(main = "CVaR Portfolio")
    
    # Compare with Mean Variance Portfolio:
    Spec = portfolioSpec()
    setTargetReturn(Spec) = mean(colAvgs(Data))
    Spec
    ans = efficientPortfolio(Data, Spec)
    weights = round(getWeights(ans), 3)
    weights
    weightsPie(ans)
    title(main = "MV Portfolio")
    attributesPie(ans)
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
    
    # CVaR Portfolio:
    myPortfolio = .cmlConstrainedCVaRPortfolio(Data, Spec, Constraints)
    myPortfolio
    
    # CVaR Portfolio:
    myPortfolio = cmlPortfolio(Data, Spec, Constraints)
    myPortfolio
    weights = round(getWeights(myPortfolio), 3)
    weights
    par(mfrow = c(2, 2), cex = 0.7)
    weightsPie(myPortfolio)
    title(main = "CVaR Portfolio")
      
    # Compare with Mean Variance Portfolio:
    Spec = portfolioSpec()
    setTargetReturn(Spec) = mean(colAvgs(Data))
    Spec
    my = cmlPortfolio(Data, Spec)
    weights = round(getWeights(myPortfolio), 3)
    weights
    weightsPie(myPortfolio)
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
    
    # Portfolio:
    myPortfolio = .tangencyConstrainedCVaRPortfolio(Data, Spec, Constraints)
    myPortfolio
    
    # Portfolio:
    myPortfolio = tangencyPortfolio(Data, Spec, Constraints)
    myPortfolio
    
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
   
    # CVaR Portfolio Optimization:
    myPortfolio = .minvarianceConstrainedCVaRPortfolio(Data, Spec, Constraints)
    myPortfolio
    
    # CVaR Portfolio Optimization:
    myPortfolio = minvariancePortfolio(Data, Spec, Constraints)
    myPortfolio
    weights = round(getWeights(myPortfolio), 3)
    weights
    par(mfrow = c(2, 2), cex = 0.7)
    weightsPie(myPortfolio)
    title(main = "Minimum-CVaR Portfolio")
      
    # Compare with Mean Variance Portfolio:
    Spec = portfolioSpec()
    setTargetReturn(Spec) = mean(colAvgs(Data))
    Spec
    my = minvariancePortfolio(Data, Spec)
    weights = round(getWeights(myPortfolio), 3)
    weights
    weightsPie(myPortfolio)
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
    
    colMeans(Data@Data)
    
    # CVaR Specification:
    Spec = portfolioSpec()
    setType(Spec) = "CVaR"
    setTargetAlpha(Spec) = 0.05
    setSolver(Spec) = "RlpSolve"
    Spec
    
    # Constraints:
    Constraints = NULL
    
    # Graph Frame:
    par(mfrow = c(2, 2), cex = 0.7)
    
    # CVaR Portfolio Optimization:
    object = cvarFrontier = portfolioFrontier(Data, Spec, Constraints)
    cvarFrontier

    weightsPlot(cvarFrontier)
    plot(cvarFrontier, which = 1:6)
    
    # Compare with Mean-Variance Portfolio:
    Spec = portfolioSpec()
    meanvarFrontier = portfolioFrontier(Data, Spec)
    meanvarFrontier
    
    weightsPlot(meanvarFrontier)
    plot(meanvarFrontier, which = 1:6)
     
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



# ------------------------------------------------------------------------------


    # Data:
    Data = as.timeSeries(data(smallcap.ts))
    Data = 100*Data[, c("BKE", "GG", "GYMB", "KRON")]
    
    # Data = 100*as.timeSeries(data(LPP2005REC))[, 1:6]
    
    # CVaR Specification:
    Spec = portfolioSpec()
    setType(Spec) = "CVaR"
    setTargetReturn(Spec) = mean(colAvgs(Data))
    setTargetAlpha(Spec) = 0.20
    setSolver(Spec) = "RlpSolve"
    Spec
    
    # CVaR Portfolio Optimization:
    myPortfolio = efficientPortfolio(Data, Spec, NULL)
    result$solution
    result$objval
    
    
    # CVaR Frontier:
    myPortfolio = portfolioFrontier(Data, Spec, NULL)
    weightsPlot(myPortfolio)
    
    
    VaR = result$solution[1]
    VaR
    es = result$solution[2:61]
    es
    weights = result$solution[62:65]
    weights
    sum(weights)
    
    
    es = t(t(result$solution[2:61]))
    alpha = Spec@portfolio$targetAlpha
    alpha
    Returns = Data@Data %*% weights
    mean(Returns[Returns<VaR])
    
    x = Returns
    eVaR = sort(x)[trunc(length(x)*alpha)] 
    eVaR
    eCVaR = mean(sort(x)[1:trunc(length(x)*alpha)])
    eCVaR
    eTCL = -mean(x[x<=eVaR])  
    eTCL
    
    x = ((VaR-Returns) + abs(VaR-Returns)) / 2
    mean(x) 
    result$objval
    
    cbind(x, es)
    
    round(sort(Returns), 3) 
    mean(Returns[Returns<0])
    
    
    
    x = matrix(rmvnorm(60, mean = runif(4, 2, 8)/100,
        sigma = diag(runif(4))), ncol = 4)
    positions = timeSequence(from = "1970-01-01", length.out = length(x[, 1]))
    Data = 100 * timeSeries(data = x, charvec = positions)


# ------------------------------------------------------------------------------   
    
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
    optVaR = -result$solution[1]
    optCVaR = result$objval   
    
    # Compare Results from Scratch and Optimization:
    c(VaR, optVaR)
    c(CVaR, optCVaR)
    
    
################################################################################


    Data = as.timeSeries(data(LPP2005REC))[, 1:6]
    
    Spec = portfolioSpec()
    setType(Spec) = "CVaR"
    setWeights(Spec) = rep(1/6, 6)
    setTargetAlpha(Spec) = 0.10
    setSolver(Spec) = "RlpSolve" 
    
    Constraints = NULL
    
    
    data = Data
    spec = Spec
    constraints = Constraints
    
    
    
    
    
    