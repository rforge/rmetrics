
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

    
################################################################################
# FUNCTION:        DESCRIPTION:
#  feasibleHull     Returns the convex Hull of the feasible set
#  feasibleSet      Computes feasible portfolios on a Risk/Return grid
#  .weightsLocator  Locates points from the feasible set and returns weights
#  feasibleSetPlot  Underlies a frontier plot with an image and contour plot   
################################################################################

    
feasibleHull <- 
function(data, spec = portfolioSpec(), constraints = "LongOnly", 
    trace = TRUE, doplot = TRUE)
{
    # Description
    #   Returns the convex Hull of the feasible set of a long only 
    #   Markowitz portfolio
    
    # Arguments:
    #   data - a multivariate object of class 'timeSeries'
    #   spec - the portfolio specification
    #   constraints - a character string with the constraints
    #   trace - should the pairwise frontier calculations be traced?
    #   doplot - should a plot of the convex hull be generated
    
    # Value:
    #   a three-column matrix with target returns, as well as minimum
    #   and maximum target risks
    
    # Note:
    #   This function is restricted to long only mean variance portfolios
    
    # Example:
    #   require(fPortfolioAdvanced)
    #   data = 100*LPP2005.RET[, 1:6]; hull = feasibleHull(data); hull
    
    # FUNCTION:
    
    # Check Spec and Constraints:
    stopifnot(is.timeSeries(data))
    stopifnot(getType(spec) == "MV")
    stopifnot(constraints == "LongOnly")
    
    # Compute the Frontier:
    frontier = portfolioFrontier(data, spec)
    
    # Get Target Risks and Returns:
    risks = Risks = frontierPoints(frontier)[, 1]
    Returns = frontierPoints(frontier)[, 2]
    
    # Compute all Pairweise Frontiers:
    if (trace) cat("Compute pairwise frontier for ...\n")
    N = ncol(data)
    for (i in 1:(N-1))
    for (j in (i+1):N)
    {
        if (trace) cat(paste("  Pair:", i, j, "\n"))
        Data = data[, c(i, j)]
        ans = portfolioFrontier(Data, spec)
        coord = frontierPoints(ans)
        nextFrontier = approx(coord[,2], coord[,1], xout = Returns)$y
        naIndex = which(is.na(nextFrontier))
        nextFrontier[naIndex] = Risks[naIndex]
        risks = rbind(risks, nextFrontier) 
    }
    
    # Risk Range:
    maxRisks = colMaxs(risks)
    minRisks = Risks
    
    # Optional Plot:
    if (doplot) {
        tailoredFrontierPlot(frontier)
        points(maxRisks, Returns)
        lines(maxRisks, Returns)
        lines(minRisks, Returns)
    }
    
    # Return Value:
    ans = cbind(
        targetReturn = Returns, 
        minTargetRisk = minRisks,
        maxTargetRisk = maxRisks)
}

    
# ------------------------------------------------------------------------------   


feasibleSet <-
function(data, spec = portfolioSpec(), constraints = "LongOnly",  
    trace = TRUE, doplot = TRUE, locator = FALSE)
{   
    # Description:
    #   Computes all feasible portfolios on a Risk/Return grid
    
    # Arguments:
    #   data - a multivariate object of class 'timeSeries'
    #   spec - the portfolio specification
    #   constraints - a character string with the constraints
    #   trace - should the pairwise frontier calculations be traced?
    #   doplot - should a plot of the convex hull be generated
    
    # Example:
    #   require(fPortfolioAdvanced)
    #   data = 100*LPP2005.RET[, 1:6]; set = feasibleSet(data)
    
    # FUNCTION:
    
    # Check Spec and Constraints:
    stopifnot(is.timeSeries(data))
    stopifnot(getType(spec) == "MV")
    stopifnot(constraints == "LongOnly")
    
    # Hull:
    hull = feasibleHull(data, spec, constraints, trace, doplot)
    targetReturns = hull[, 1]
    minRisks = hull[, 2]
    maxRisks = hull[, 3]
    minRisk = min(minRisks)
    maxRisk = max(maxRisks)
    targetRisks = seq(minRisk, maxRisk, length = length(targetReturns)) 
    
    # Covariance and Mean:
    COV = cov(data)
    MEAN = colMeans(data)
   
    # Rdonlp2 Solver Setup:
    N = ncol(data)
    par = rep(1/N, times = N)
    fun = function(x) ( t(x) %*% COV %*% x)
    par.lower = rep(0, times = N)
    par.upper = rep(1, times = N)
    eqA = rbind(rep(1, times = N), MEAN)
    eqFun = list( function(x) sqrt(t(x) %*% COV %*% x ) )
    
    # Grid Portfolios from the feasible Set:
    x = y = Weights = Coords = Positions = NULL
    for (i in 1:length(targetReturns)) 
    for (j in 1:length(targetRisks))
    {
        targetReturn = targetReturns[i]
        targetRisk = targetRisks[j]
        
        if (targetRisk >= minRisks[i] & targetRisk <= maxRisks[i]) 
        {
            eqA.bound = c(1, targetReturn)
            eqFun.bound = targetRisk
            
            ans = donlp2NLP(
                par = par, 
                fun = fun, par.lower = par.lower, par.upper = par.upper,
                eqA = eqA, eqA.bound = eqA.bound,
                ineqA = NULL, ineqA.lower = NULL, ineqA.upper = NULL,
                eqFun = eqFun, eqFun.bound = eqFun.bound,
                ineqFun = list(), ineqFun.lower = NULL, ineqFun.upper = NULL)
            
            weights = signif(ans$par, digits = 3)
            invest = signif(sum(ans$par), digits = 3) 
    
            Coords = rbind(Coords, c(j, i))
            Positions = rbind(Positions, c(targetRisk, targetReturn))
            Weights = rbind(Weights, ans$par)
            if (doplot) 
                points(targetRisk, targetReturn, pch = 19, col = "steelblue")
            x = c(x, targetRisk)
            y = c(y, targetReturn)
            if (trace) {
                cat(paste(signif(targetRisk, 4), "\t", 
                    signif(targetReturn, 4), "\t"))
                cat(paste("\t", weights))
                cat(paste("\t:", invest))
                cat(paste("\t", ans$message, "\n"))
            }
        }   
    }
    
    # Final Overlay Plot:
    if (doplot) {  
        object = portfolioFrontier(data, spec, constraints)
        offset = 0.1
        xmax = max(sqrt(diag(getCov(object))))
        xlim = c(0, xmax)
        Xlim = c(xlim[1] - diff(xlim)*offset, xlim[2] + diff(xlim)*offset)
        ylim = range(getMean(object))
        Ylim = c(ylim[1] - diff(ylim)*offset, ylim[2] + diff(ylim)*offset)
        return = "mean"
        risk = "Cov"
        frontierPlot(object, return = return, risk = risk, auto = FALSE, 
            xlim = Xlim, ylim = Ylim, pch = 19, add = TRUE)
        abline(h = 0, col = "grey")
        abline(v = 0, col = "grey")
        data = getData(object)
        spec = getSpec(object)
        constraints = getConstraints(object)
        mvPortfolio = minvariancePortfolio(data, spec, constraints)
        minvariancePoints(object, return = return, risk = risk, 
            auto = FALSE, pch = 19, col = "red")
        tangencyPoints(object, return = return, risk = risk, 
            auto = FALSE, pch = 19, col = "blue")
        tangencyLines(object, return = return, risk = risk, 
            auto = FALSE, col = "blue")
        xy = equalWeightsPoints(object, return = return, risk = risk, 
            auto = FALSE, pch = 15, col = "grey")
        text(xy[, 1] + diff(xlim)/20, xy[, 2] + diff(ylim)/20, "EWP", 
            font = 2, cex = 0.7)
        col = rainbow(getNAssets(object))
        xy = singleAssetPoints(object, return = return, risk = risk, 
            auto = FALSE, cex = 1.5, col = col, lwd = 2)
        text(xy[, 1] + diff(xlim)/20, xy[, 2] + diff(ylim)/20, 
            rownames(xy), font = 2, cex = 0.7)
        sharpeRatioLines(object, return = return, risk = risk, 
            auto = FALSE, col = "orange", lwd = 2)  
        points(maxRisks, targetReturns, lwd = 2)
        lines(maxRisks, targetReturns)
        lines(minRisks, targetReturns)
    }
    
    # Compose Result:
    colnames(Weights) = colnames(data)
    ans = list(
        data = data,
        weights  = Weights, 
        coords = Coords, 
        positions = Positions,
        targetReturns = targetReturns, 
        targetRisks = targetRisks,
        hull = hull)
    
    # Use Locator?
    while (locator) .weightsLocator(Weights, Positions)
        
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.weightsLocator <- 
function(Weights, Positions)
{
    # Description:
    #   Locates a point from the feasible set and returns the corresping weights
    
    # Arguments:
    #   Weights - weights matrix
    
    # FUNCTION:
    
    # Locate:
    Names = colnames(Weights)
    while (TRUE){
        z = locator(1)
        distance = (Positions[,1]-z$x)^2 + (Positions[,2]-z$y)^2
        Index = which.min( distance )
        weights = Weights[Index, ]
        names(weights) = Names
        print(sort(round(weights, 3)))
    }
    
    # Return Value:
    invisible()
}
    
      
# ------------------------------------------------------------------------------


feasibleSetPlot <-
function(object, FUN = cvarRisk, ...)
{
    # Description:
    #   Underlies a frontier plot wit an image and contour plot
    
    # Example:
    #   require(fPortfolio); require(Rdonlp2)
    #   data = 100*LPP2005.RET[, 1:6]; set = feasibleSet(data, locator = FALSE)
    #   feasibleSetPlot(set, FUN = cvarRisk, alpha = 0.05)
    
    # FUN(data, weights, ...)
    
    # Settings:
    data = object$data
    weights  = object$weights
    coords = object$coords
    positions = object$positions
    targetReturns = object$targetReturns
    targetRisks = object$targetRisks
    hull = object$hull
    fun = match.fun(FUN)
    n = length(targetRisks)
    N = nrow(weights)
    x = targetRisks
    y = targetReturns
    
    # Image and Contour Plot:
    offset = 0.1
    xmax = max(sqrt(diag(getCov(data))))
    xlim = c(0, xmax)
    Xlim = c(xlim[1] - diff(xlim) * offset, xlim[2] + diff(xlim) * offset)
    Xlab = "Risk"
    ylim = range(getMean(data))
    Ylim = c(ylim[1] - diff(ylim) * offset, ylim[2] + diff(ylim) * offset)
    Ylab = "Return"
    z = matrix(rep(NA, times = n*n), ncol = n)
    for (i in 1:N) 
        z[coords[i, 1], coords[i, 2]] = 
        fun(getSeries(data), weights = weights[i, ], ...)
    image(x, y, z, xlim = Xlim, ylim = Ylim, xlab = Xlab, ylab = Ylab)
    contour(x, y, z, add = TRUE, nlevels = 20, labcex = 1)
    
    # Plot the Convex Hull of the Fesible Set:
    lines(hull[,2], hull[,1])
    lines(hull[,3], hull[,1])
    
    # Add a Box Around the Plot:
    box()

    # What to plot?
    return = "mean"
    risk = "Cov"
    col = 1:getNAssets(data)
    
    # Add Decoration to Plot:
    frontier = portfolioFrontier(data)
    minvariancePoints(frontier, return = return, risk = risk, auto = FALSE, 
        pch = 19, col = "red")
    tangencyPoints(frontier, return = return, risk = risk, auto = FALSE, 
        pch = 19, col = "blue")
    tangencyLines(frontier, return = return, risk = risk, auto = FALSE, 
        col = "blue")
    xy = equalWeightsPoints(frontier, return = return, risk = risk, 
        auto = FALSE, pch = 15, col = "grey")
    text(xy[, 1] + diff(xlim)/20, xy[, 2] + diff(ylim)/20, "EWP", 
        font = 2, cex = 0.7)
    xy = singleAssetPoints(frontier, return = return, risk = risk, 
        auto = FALSE, cex = 1.5, col = col, lwd = 2)
    text(xy[, 1] + diff(xlim)/20, xy[, 2] + diff(ylim)/20, rownames(xy), 
        font = 2, cex = 0.7)
    grid()
    
    # Return Value:
    invisible()
}

    
# ------------------------------------------------------------------------------


if(FALSE) 
{

    # Some more Exmples ...
    # ... should be moved to unit tests!
    
    
    require(fPortfolio) 
    require(Rdonlp2)
    
    
    data = 100*LPP2005.RET[, 1:6]
    set = feasibleSet(data)
    
    
    # Example 1:
    FUN = function(data, weights, ...) sd(weights)
    feasibleSetPlot(set, FUN)
    
    
    # Example 2:
    feasibleSetPlot(set, varRisk)
    
    
    # Example 3:
    feasibleSetPlot(set, cvarRisk)
    
    
    # Example 4 - Maximum Drawdowns:
    maxDrawdown = function(data, weights) {
        data = data/100
        X = data 
        series(X) = data %*% as.vector(weights)
        min(drawdowns(X)) }
    feasibleSetPlot(set, maxDrawdown)
    
    
    # Example 5: - Maximum RiskBudget
    maxRiskBudget <- function(data, weights)
        max(riskBudgets(data, weights))
    feasibleSetPlot(set, maxRiskBudget)
    
    
    # Example 6: - Maximum RiskContribution
    maxRiskContribution <- function(data, weights)
        max(riskContributions(data, weights))
    feasibleSetPlot(set, maxRiskContribution)
    
    
    # Example 7: - Maximum MCR
    maxMCR <- function(data, weights)
        max(mcr(data, weights))
    feasibleSetPlot(set, maxMCR)
    
    
    # Example 8: - Sharpe Ratio
    sharpeRatio <- function(data, weights) {
        return = mean( data %*% weights )
        risk = sqrt( t(weights) %*% cov(data) %*% weights )[[1]] 
        return / risk }
    feasibleSetPlot(set, sharpeRatio)
    
 
}


################################################################################
  
    