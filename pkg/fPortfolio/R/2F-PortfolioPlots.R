# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR Description. See the 
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General 
# Public License along with this library; if not, write to the 
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
# MA 02111-1307 USA

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
# FUNCTION                     EFFICIENT FRONTIER PLOT AND ADDONS:  
#  frontierPlot                  Plots efficient Frontier
#   .sharpeRatioPlot             Adds Sharpe Ratio
#   .minvariancePlot             Adds Minimum Variance point
#   .cmlPlot                     Adds Market Portfolio and Capital Market Line
#   .tangencyPlot                Adds Tangency Portfolio point and line
#   .equalWeightsPlot            Adds point of equal weights portfolio
#   .singleAssetPlot             Adds points of single asset portfolios
#   .twoAssetsPlot               Adds EF for all combinations of two assets
#   .wheelPiePlot                Adds pie chart of weights on EF
#   .monteCarloPlot              Adds randomly produced feasible portfolios
#   .notStackedWeightsPlot       Plots the not stacked weights of potfolio
#   .addlegend                   Adds legend to sliders
# FUNCTION                     DESCRIPTION:                  
#  weightsPlot                  Plots staggered weights
#  weightsPie                   Plots staggered weights
#  attributesPlot               Plots weighted means
# FUNCTION                     DESCRIPTION:
#  covEllipsesPlot              Plots covariance ellipses
################################################################################


frontierPlot =
function(object, frontier = c("both", "lower", "upper"),
    col = c("black", "grey"), add = FALSE, ...)
{   # A function implemented by Rmetrics

    # Description:
    #   Plots the efficient frontier
    
    # FUNCTION:
    
    # Check Colors:
    stopifnot(length(col) == 2)
  
    # Settings:
    frontier = match.arg(frontier)
    
    # Frontier:
    fullFrontier = getFrontier(object, frontier = "both")
    upperFrontier = getFrontier(object, frontier = "upper")
    lowerFrontier = getFrontier(object, frontier = "lower")
    
    # Check for 'xlim' Argument:
    Arg <- match.call(expand.dots = TRUE)
    m <- match(c("xlim", "ylim"), names(Arg), Arg)
    xArg <- as.character(Arg[c(1, m)])[2]
    yArg <- as.character(Arg[c(1, m)])[3]

    # Plot:
    if(xArg == "NULL" & yArg == "NULL") {
        # Use default, if xlim and ylim is not specified ...
        mu = object@data$statistics$mu
        Sigma = object@data$statistics$Sigma      
        yLim = range(mu) + 0.25*c(-diff(range(mu)), diff(range(mu)))
        # First, take care that all assets appear on the plot ...
        sqrtSig = sqrt(diag(Sigma))
        xLimAssets = c(min(sqrtSig), max(sqrtSig))+
             c(-0.4*diff(range(sqrtSig)), 0.1*diff(range(sqrtSig)))
        # ... second take care that the whole frontier appears on the plot:
        xLimFrontier = range(fullFrontier[, 1])
        xLim = range(c(xLimAssets, xLimFrontier))
        # Plot:
        if(!add){
            if(frontier == "upper" | frontier == "both") {
                plot(upperFrontier, col = col[1], xlim = xLim, ylim = yLim, ...)
            } else {
                if( frontier == "both") {
                    points(bothFrontier, col = col[2], xlim = xLim,
                        ylim = yLim, ...)
                }
                if(frontier == "lower" ) {
                    plot(lowerFrontier, col = col[2], xlim = xLim, ylim = yLim,
                         ...)
                }
            }
        }
        if(frontier == "upper" | frontier == "both") {
            points(upperFrontier, col = col[1], ...)
        }
        if(frontier == "lower" | frontier == "both") {
            points(lowerFrontier, col = col[2], ...)
        }
    } else if (xArg != "NULL" & yArg == "NULL") {
        # In this only xlim is specified in the argument list 
        mu = object@data$statistics$mu
        Sigma = object@data$statistics$Sigma      
        yLim = range(mu) + 0.25*c(-diff(range(mu)), diff(range(mu)))
        # Plot:
        if(!add){
            if(frontier == "upper" | frontier == "both") {
                plot(upperFrontier, col = col[1], ylim = yLim, ...)
            } else {
                if( frontier == "both") {
                    points(bothFrontier, col = col[2], ylim = yLim, ...)
                }
                if(frontier == "lower" ) {
                    plot(lowerFrontier, col = col[2], ylim = yLim, ...)
                }
            }
        }
        if(frontier == "upper" | frontier == "both") {
            points(upperFrontier, col = col[1], ...)
        }
        if(frontier == "lower" | frontier == "both") {
            points(lowerFrontier, col = col[2], ...)
        }   
    } else if(xArg == "NULL" & yArg != "NULL") {
        # In this only ylim is specified in the argument list 
        Sigma = object@data$statistics$Sigma      
        # First, take care that all assets appear on the plot ...
        sqrtSig = sqrt(diag(Sigma))
        xLimAssets = c(min(sqrtSig), max(sqrtSig))+
             c(-0.4*diff(range(sqrtSig)), 0.1*diff(range(sqrtSig)))
        # ... second take care that the whole frontier appears on the plot:
        xLimFrontier = range(fullFrontier[, 1])
        xLim = range(c(xLimAssets, xLimFrontier))
        # Plot:
        if(!add){
            if(frontier == "upper" | frontier == "both") {
                plot(upperFrontier, col = col[1], xlim = xLim, ...)
            } else {
                if( frontier == "both") {
                    points(bothFrontier, col = col[2], xlim = xLim, ...)
                }
                if(frontier == "lower" ) {
                    plot(lowerFrontier, col = col[2], xlim = xLim, ...)
                }
            }
        }
        if(frontier == "upper" | frontier == "both") {
            points(upperFrontier, col = col[1], ...)
        }
        if(frontier == "lower" | frontier == "both") {
            points(lowerFrontier, col = col[2], ...)
        }
    } else if (xArg != "NULL" & yArg != "NULL"){
        #  If both xlim and ylim are not defined in argument list ...
        if(!add){
            if(frontier == "upper" | frontier == "both") {
                plot(fullFrontier, type = "n", ...)
                points(upperFrontier, col = col[1], ...)
            }
            if(frontier == "both") {
                points(lowerFrontier, col = col[2], ...)
            }
            if(frontier == "lower") {
                plot(lowerFrontier, col = col[2], ...)
            }
        } else{    
            if(frontier == "upper" | frontier == "both") {
                points(upperFrontier, col = col[1], ...)
            }
            if(frontier == "lower" | frontier == "both") {
                points(lowerFrontier, col = col[2], ...)
            }
        }
    }    
      
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------

  
.sharpeRatioPlot = 
function(object, ...)
{   # A function implemented by Rmetrics

    # Description:
    #   Adds Sharpe Ratio
    
    # FUNCTION:
    
    # Get Portfolio Slots:
    Data = object@data$statistics
    Spec = object@specification
    Constraints = object@constraints
    
    # Efficient Frontier:
    portfolio = object@portfolio
    x = portfolio$targetRisk
    y = portfolio$targetReturn
    
    # Tangency Portfolio:
    tangenyPortfolio = tangencyPortfolio(Data, Spec, Constraints)
    x.tg = tangenyPortfolio@portfolio$targetReturn 
     
    # Normalization to fit in EF Plot:
    norm = x.tg / max(y/x) 
    index = 2:length(x) 
    index = index[diff(x) > 0]
    x = x[index]
    y = y[index]
    points(x, (y/x*norm), ...)
        
    # Add Tailored Labels -  2 may be a good Number ...
    x.tg = x.tg[index]
    norm2 = x.tg / max(y)
    Range = range(y/x * norm) 
    
    # Take a reasonable number of significant digits to plot, e.g. 2 ...
    nPrecision = 3
    Labels = signif(Range, nPrecision)
    axis(4, at = Range, labels = c(" ", " "))
    axis(4, at = mean(Range), labels = paste(Labels[1], "   ", Labels[2]))
    
    # Add Axis Labels and Title:
    mtext("Sharpe Ratio", side = 4, line = 2)
    
    # return Value:
    invisible()
}


# ------------------------------------------------------------------------------


.minvariancePlot = 
function(object, ...)
{   # A function implemented by Rmetrics

    # Description:
    #   Adds the minimum variance point to a portfolio plot
    
    # FUNCTION:
     
    # Get Portfolio Slots:
    Data = object@data$statistics
    Spec = object@specification
    Constraints = object@constraints
    
    # Add Minimum Variance Point:
    points(getFrontier(minvariancePortfolio(Data, Spec, Constraints)), ...)
    
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


.tangencyPlot = 
function(object, ...)
{   # A function implemented by Rmetrics

    # Description:
    #   Adds the tangency point and line to a portfolio plot
    
    # FUNCTION:
    
    # Get Portfolio Slots:
    Data = object@data$statistics
    Spec = object@specification
    Constraints = object@constraints
    
    # Compute Tangency Portfolio:
    tangencyPortfolio = tangencyPortfolio(Data, Spec, Constraints)
    
    # Add Tangency Point:
    points(getFrontier(tangencyPortfolio), ...)
    
    # Add Tangency Line:
    slope = getTargetReturn(tangencyPortfolio)/getTargetRisk(tangencyPortfolio)
    abline(0, slope, ...)
   
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


.cmlPlot = 
function(object, ...)
{   # A function implemented by Rmetrics

    # Description:
    #   Adds the capital market line to a portfolio plot
    
    # FUNCTION:
    
    # Get Portfolio Statistics:
    Data = object@data$statistics
    Spec = object@specification
    Constraints = object@constraints
    
    # Compute Tangency Portfolio:
    cmlPortfolio = cmlPortfolio(Data, Spec, Constraints)
    
    # Add Tangency Point:
    points(getFrontier(cmlPortfolio), ...)
    
    # Add Tangency Line:
    riskFreeRate = cmlPortfolio@specification@portfolio$riskFreeRate
    slope = ((getTargetReturn(cmlPortfolio) - riskFreeRate)
        /getTargetRisk(cmlPortfolio))
    abline(b = slope, a = riskFreeRate, ...)
    
    # Return Value:
    invisible(object)
}


# ------------------------------------------------------------------------------  


.singleAssetPlot =
function(object, ...)
{   # A function implemented by Rmetrics

    # Description:
    #   Adds all single assets returns and risks to a portfolio plot
    
    # FUNCTION:
     
    # Get Portfolio Statistics:
    Statistics = object@data$statistics
    
    # Compose Assets:
    assets = cbind(sigma = sqrt(diag(Statistics$Sigma)), mu = Statistics$mu)
    
    # Add Asset Points:
    points(assets, ...)
    
    # Return Value:
    invisible()  
}


# ------------------------------------------------------------------------------  


.equalWeightsPlot =
function(object, ...)
{   # A function implemented by Rmetrics

    # Description:
    #   Adds equal weights portfolio to a portfolio plot
    
    # FUNCTION:
    
    # Get Portfolio Statistics: 
    Data = object@data$statistics
    Spec = object@specification
    Constraints = object@constraints
    
    # Compute Equal Weights Portfolio:
    equalWeightsPortfolio = feasiblePortfolio(Data, Spec, Constraints)
   
    # Add Equal Weights Point:
    points(getFrontier(equalWeightsPortfolio), ...)
    
    # Return Value:   
    invisible()    
}


# ------------------------------------------------------------------------------


.twoAssetsPlot =
function(object, ...)
{   # A function implemented by Rmetrics

    # Description:
    #   Adds efficient long-only frontier of all portfolio pairs
    
    # FUNCTION:
    
    # Get Portfolio Statistics: 
    Data = object@data$statistics
    Spec = object@specification
    Constraints = object@constraints
    # Add Froniters for all Two-Assets Portfolios:
    N = length(Data$mu)
     
    # Plotting only the tenth part of the original frontier porints
    nFP = object@specification@portfolio$nFrontierPoints
    object@specification@portfolio$nFrontierPoints = nFP / 10
    for ( i in 1:(N-1) ) {
        for (j in (i+1):N ) {
            index = c(i, j) 
            statistics = list(mu = Data$mu[index],
                Sigma = Data$Sigma[index, index])
            statistics = list(statistics = statistics)
            ans = .portfolioShortMVFrontier(data = statistics, Spec,
                Constraints)
            lines(getFrontier(ans), ...)
        }
    }
    # Restetting to the original # frontier points
    object@specification@portfolio$nFrontierPoints = nFP * 10

    # Return Value:
    invisible()   
}


# ------------------------------------------------------------------------------


.wheelPiePlot = 
function(object, piePos = NULL, pieR = NULL, pieOffset = NULL, ...)
{   # A function implemented by Rmetrics

    # Description:
    #   Adds a pie plot of the weights
    
    # Details:
    #   The default settings are:
    #   piePos - Position of tangency Portfolio
    #   pieR - 10% of the Risk Range: diff(range(targetRisk(object)))/10 
    
    # FUNCTION:
    

    # Extraction coordinates    
    p = par()$usr/15
    dx = p[2]-p[1]
    dy = p[4]-p[3]

    
    if(is.null(piePos)) {
        Data = object@data$statistics
        Spec = object@specification
        Constraints = object@constraints
        tg = getTargetReturn(tangencyPortfolio(Data, Spec, Constraints))
        ef = getTargetReturn(object)
        piePos = which(diff(sign(ef-tg)) > 0) 
    }
    if(is.null(pieR)) { 
        pieR = c(1, 1)
    }
    if(is.null(pieOffset)) { 
        pieOffset = c(-2*dx, 0)
    }
    # Plot Circle:
    weights = getWeights(object)[piePos, ]
    nWeights = length(weights)
    Sign = rep("+", nWeights)
    Sign[(1:nWeights)[weights < 0]] = "-"
    x = getTargetRisk(object)[piePos]
    y = getTargetReturn(object)[piePos]
    phi =  seq(0, 2*pi, length = 360)
    X = x + pieOffset[1] + pieR[1] * sin(phi) * dx
    Y = y + pieOffset[2] + pieR[2] * cos(phi) * dy
    lines(X, Y)
    
    # Add Center Point:
    points(x, y, col = "red", pch = 19, cex = 1.5)
    
    # Add Arrow:
    lines(c(x, x+pieOffset[1]), c(y, y+pieOffset[2]))
    
    # Add Color Wheel:
    psi = 2*pi*c(0, cumsum(abs(weights)/sum(abs(weights))))
    for (i in 1 : length(weights) ) {
        # Plotting Only Pie pieces with Weights > 5%
        if(psi[i+1]-psi[i] > 0.05 * 2*pi) {
            Psi = psi[i] + (0:100) * (psi[i+1]-psi[i])/100
            polyX = x + pieOffset[1] + pieR[1]*c(0, sin(Psi), 0) * dx
            polyY = y + pieOffset[2] + pieR[2]*c(0, cos(Psi), 0) * dy
            polygon(polyX, polyY, col = rainbow(nWeights)[i])
            # Adding the Asset Signs:
            text(x + pieOffset[1] + 0.75*pieR[1]* sin(Psi[51]) * dx,
                y + pieOffset[2] + 0.75*pieR[2]* cos(Psi[51]) * dy,
                col = "white", Sign[i])
         }
    }
    
    # Return Value:
    invisible()
}

# ------------------------------------------------------------------------------


.attPiePlot = 
function(object, piePos = NULL, pieR = NULL, pieOffset = NULL, ...)
{   # A function implemented by Rmetrics

    # Description:
    #   Adds a pie plot of the weights
    
    # Details:
    #   The default settings are:
    #   piePos - Position of tangency Portfolio
    #   pieR - 10% of the Risk Range: diff(range(targetRisk(object)))/10 
    
    # FUNCTION:
    

    # Extraction coordinates    
    p = par()$usr/15
    dx = p[2]-p[1]
    dy = p[4]-p[3]


    
    if(is.null(piePos)) {
        Data = object@data$statistics
        Spec = object@specification
        Constraints = object@constraints
        tg = getTargetReturn(tangencyPortfolio(Data, Spec, Constraints))
        ef = getTargetReturn(object)
        piePos = which(diff(sign(ef-tg)) > 0) 
    }
    if(is.null(pieR)) { 
        pieR = c(1, 1)
    }
    if(is.null(pieOffset)) { 
        pieOffset = c(2*dx, 0)
    }
    # Plot Circle:
    # Get weighted Returns:
    weights = getWeights(object)
    dim = dim(weights)
 
    returns = object@data$statistics$mu
    weightedReturns = NULL
    for(i in 1:dim[2]){
        nextWeightedReturns = weights[,i]*returns[i]
        weightedReturns = cbind(weightedReturns, nextWeightedReturns)
    }
    colnames(weightedReturns) = colnames(weights)
    weightedReturns = weightedReturns[piePos, ]
    nWeights = length(weightedReturns)
    Sign = rep("+", times =  nWeights)
    Sign[(1:nWeights)[weightedReturns < 0]] = "-" 
    x = getTargetRisk(object)[piePos]
    y = getTargetReturn(object)[piePos]
    phi =  seq(0, 2*pi, length = 360)
    X = x + pieOffset[1] + pieR[1] * sin(phi) * dx
    Y = y + pieOffset[2] + pieR[2] * cos(phi) * dy
    lines(X, Y)
    
    # Add Center Point:
    points(x, y, col = "red", pch = 19, cex = 1.5)
    
    # Add Arrow:
    lines(c(x, x+pieOffset[1]), c(y, y+pieOffset[2]))

    # Add Color Wheel:
    psi = 2*pi*c(0, cumsum(abs(weightedReturns)/sum(abs(weightedReturns))))
    for (i in 1 : nWeights) {
        # Plotting Only Pie pieces with Weights > 5%
        if(psi[i+1]-psi[i] > 0.05 * 2*pi) {
            Psi = psi[i] + (0:100) * (psi[i+1]-psi[i])/100
            polyX = x + pieOffset[1] + pieR[1]*c(0, sin(Psi), 0) * dx
            polyY = y + pieOffset[2] + pieR[2]*c(0, cos(Psi), 0) * dy
            polygon(polyX, polyY, col = rainbow(nWeights)[i])
            # Adding the Asset Signs:
            text(x + pieOffset[1] + 0.75*pieR[1]* sin(Psi[51]) * dx,
                y + pieOffset[2] + 0.75*pieR[2]* cos(Psi[51]) * dy,
                col = "white", Sign[i])
         }
    }
    
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


.monteCarloPlot =
function(object, mcSteps, ...)
{   # A function implemented by Rmetrics

    # Description:
    #   Adds randomly feasible portfolios to a plot
    
    # FUNCTION:
    
    # Get Portfolio Statistics: 
    Data = object@data$statistics
    mu = Data$mu
    Sigma = Data$Sigma
    N = length(mu)  
    # Monte Carlo Loop:
    for (k in 1:mcSteps) {  
        s = sign(rnorm(N, mean = rnorm(1)))
        weights = s * abs(rcauchy(N))        
        weights = weights / sum(weights)
        Return = as.numeric(mu %*% weights)
        Risk = sqrt( as.numeric( t(weights) %*% Sigma %*% (weights) ) )
        points(Risk, Return, ...)
    }
    
    # Return Value:
    invisible()
}

#-------------------------------------------------------------------------------


.notStackedWeightsPlot =
function(object, col = NULL)
{   # A function implemented by Rmetrics

    # Description:
    
    # Arguments:
    #   object - an object of class 'fPORTFOLIO'
    #   col - a color palette, by default the rainbow palette
    
    # FUNCTION:
    
    weights = getWeights(object)
    N = ncol(weights)
    targetRisk = getTargetRisk(object)
    targetReturn = getTargetReturn(object)
    nSigma = length(targetRisk)
    
    # Select Colors if not specified ...
    if (is.null(col)) col = rainbow(N)
    
    # Plot first asset ...    
    plot(weights[, 1], col = col[1], type = "l", ylim = c(min(weights),
        max(weights)), xaxt = "n", xlab = "", ylab = "")

    # Add vertical Line at minimum risk:
    names(targetRisk) <- as.character(seq(1, nSigma, 1))
    minRisk = min(targetRisk)
    for(i in 1: nSigma){
          if(minRisk == targetRisk[i]) minRisk = targetRisk[i]
    }
    minRisk = as.numeric(names(minRisk))
        
    # Big Point at minimum risk for first asset ...
    points(y = weights[minRisk, 1], x = minRisk, col = col[1], pch = 19,
        xaxt = "n", yaxt = "n", cex = 2)
    
    # ... and all other  
    for(i in 1:(N-1)){
        points(weights[, i+1], col = col[i+1], type = "l", xaxt = "n",
        yaxt = "n")
        points(y = weights[minRisk, i+1], x = minRisk, col = col[i+1], pch = 19,
            xaxt = "n", yaxt = "n", cex = 2)

    }
    grid()
    abline(h = 0, col = "grey", lty = 3)
    abline(v = minRisk, col = "black", lty = 3)


    # Add Tailored Labels -  6 may be a good Number ...
    nLabels = 6
    M = c(0, ( 1: (nSigma %/% nLabels) ) ) *nLabels + 1
    
    # Take a reasonable number of significant digits to plot, e.g. 2 ...
    nPrecision = 3
    axis(1, at = M, labels = signif(targetRisk[M], nPrecision))
    axis(3, at = M, labels = signif(targetReturn[M], nPrecision))
    
     
    # Add Axis Labels and Title:
    mtext("Risk", side = 1, line = 2, cex = .7)
    mtext("Return", side = 3, line = 2, cex = .7)
    mtext("Weight", side = 2, line = 2, cex = .7)
    
    # Return Value:
    invisible()   
}


#-------------------------------------------------------------------------------


.addlegend = 
function(object, control = list())
{   # A function implemented by Rmetrics

    # Description: 
    #   Adds a perdefined legend to sliders
    
    # Arguments:
    #   object - an object of class 'fPORTFOLIO'
    #   control - control list for colors and symbols
    
    # FUNCTION:
    
    dim = getNumberOfAssets(object)
    namesSingleAsset = names(object@data$statistics$mu)
    # Check if polt is used for forntierSlider...
    if(control$sliderFlag == "frontier"){
        legendtext = c("Efficient Frontier", "Sharpe Ratio", "Minimum Variance",
            "Tangency Portfolio", "Market Portfolio", "Equal Weights",
            namesSingleAsset)
        color = c("black", control$sharpeRatio.col, control$minvariance.col,
            control$tangency.col, control$cml.col, control$equalWeights.col,
            control$singleAsset.col)
        sym = c(19, 19, control$minvariance.pch, control$tangency.pch,
            control$cml.pch, control$equalWeights.pch,
            rep(control$singleAsset.pch, times = dim))
    # ... else is the weightsSlider case
    } else {
            legendtext = c("Efficient Frontier", "Minimum Variance",
            "Tangency Portfolio", namesSingleAsset)
        color = c("black", control$minvariance.col,
            control$tangency.col, control$singleAsset.col)
        sym = c(19, control$minvariance.pch, control$tangency.pch,
            rep(control$singleAsset.pch, times = dim))
    }
 
    # Adding legend:
    legend("topleft", legend = legendtext, col = color, pch = sym, cex = .8,
        bty = "n")
        
    # Return Value:
    invisible()

}
 

################################################################################


weightsPlot =
function(object, col = NULL, legend = FALSE)
{   # A function implemented by Rmetrics

    # Description:
    
    # Arguments:
    #   object - an object of class 'fPORTFOLIO'
    #   col - a color palette, by the rainbow palette
    
    # FUNCTION:
    
    # Select Colors if not specified ...
    if (is.null(col)) col = rainbow(ncol(object@portfolio$weights))
    
    # Get Weights:
    weights = getWeights(object)
    pos.weights = +0.5 * (abs(weights) + weights)
    neg.weights = -0.5 * (abs(weights) - weights)
    
    # Define Plot Range:
    ymax = max(rowSums(pos.weights))
    ymin = min(rowSums(neg.weights))
    range = ymax - ymin
    ymax = ymax + 0.005 * range
    ymin = ymin - 0.005 * range
    dim = dim(weights)
    range = dim[1]
    xmin = 0
    xmax = range + .2 * range
    
    # Create Bar Plots:
    if(!legend){
        barplot(t(pos.weights), space = 0, ylab = "",
            ylim = c(ymin, ymax), col = col, border = "grey")
    } else {
        legendtext = names(object@data$statistics$mu)
        if(is.null(legendtext)){
            for(i in 1:dim[2]){legendtext[i] = paste("Asset", i, sep = " ")}
        }
        barplot(t(pos.weights), space = 0, ylab = "", xlim = c(xmin, xmax),
            ylim = c(ymin, ymax), col = col, border = "grey")
        legend("topright", legend = legendtext, bty = "n", cex = 0.8,
            fill = col)
    }
    barplot(t(neg.weights), space = 0, add = TRUE, col = col, border = "grey") 
    
    # Add Tailored Labels -  6 may be a good Number ...
    targetRisk = getTargetRisk(object)
    targetReturn = getTargetReturn(object)
    nSigma = length(targetRisk)
    nLabels = 6
    M = c(0, ( 1: (nSigma %/% nLabels) ) ) *nLabels + 1
    # Take a reasonable number of significant digits to plot, e.g. 2 ...
    nPrecision = 3
    axis(1, at = M, labels = signif(targetRisk[M], nPrecision))
    axis(3, at = M, labels = signif(targetReturn[M], nPrecision))
    
    # Add Axis Labels and Title:
    mtext("Target Risk", side = 1, line = 2, cex = .7)
    mtext("Target Return", side = 3, line = 2, cex = .7)
    mtext("Weight", side = 2, line = 2, cex = .7)
      
    # Add Weights 0 and 1 Reference Lines
    lines(x = c(0, nSigma), c(1, 1), col = "grey", lty = 3) 
    lines(x = c(0, nSigma), c(0, 0), col = "grey", lty = 3)   
    
    # Add vertical Line at minimum risk:
    names(targetRisk) <- as.character(seq(1, nSigma, 1))
    minRisk = min(targetRisk)
    for(i in 1: nSigma){
          if(minRisk == targetRisk[i]) minRisk = targetRisk[i]
    }
    minRisk = as.numeric(names(minRisk))
    abline(v = minRisk, col = "black", lty = 3)

    
    # Complete to draw box ...
    box()
    
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


weightsPie = 
function(object, col = NULL, box = TRUE, ...)
{   # A function implemented by Rmetrics

    # Description:
    #   Adds a pie plot of the weights
        
    # Example:
    #   weightsPie(tangencyPortfolio(dutchPortfolioData(), portfolioSpec()))
    #   title(main = "Tangency Portfolio Weights")
    
    # FUNCTION:
    
    # Plot Circle:
    weights = getWeights(object)
    nWeights = length(weights)
    Sign = rep("+", nWeights)
    Sign[(1:nWeights)[weights < 0]] = "-"
    
    # Color Palette:
    if (is.null(col)) col = rainbow(nWeights)
    
    # Pie Chart:
    pie(abs(weights), labels = paste(1:nWeights, Sign), col = col, ...)
    if (box) box()
    
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


attributesPlot =
function(object, col = NULL, legend = FALSE)
{   # A function implemented by Rmetrics

    # Description:
    
    # Arguments:
    #   object - an object of class 'fPORTFOLIO'
    #   col - a color palette, by the rainbow palette
    
    # FUNCTION:
    
    # Select Colors if not specified ...
    if (is.null(col)) col = rainbow(ncol(object@portfolio$weights))
    
    # Get weighted Returns:
    weights = getWeights(object)
    dim = dim(weights)
    returns = object@data$statistics$mu
    weightedReturns = NULL
    for(i in 1:dim[2]){
        nextWeightedReturns = weights[,i]*returns[i]
        weightedReturns = cbind(weightedReturns, nextWeightedReturns)
    }
    colnames(weightedReturns) = colnames(weights)
    pos.weightedReturns = +0.5 * (abs(weightedReturns) + weightedReturns)
    neg.weightedReturns = -0.5 * (abs(weightedReturns) - weightedReturns)
    
    # Define Plot Range:
    ymax = max(rowSums(pos.weightedReturns))
    ymin = min(rowSums(neg.weightedReturns))
    range = ymax - ymin
    ymax = ymax + 0.005 * range
    ymin = ymin - 0.005 * range
    range = dim[1]
    xmin = 0
    xmax = range + .2 * range

    # Create Bar Plots:
    if(!legend){
        barplot(t(pos.weightedReturns), space = 0, ylab = "",
            ylim = c(ymin, ymax), col = col, border = "grey")
    } else {
        legendtext = names(object@data$statistics$mu)
        if(is.null(legendtext)){
            for(i in 1:dim[2]){legendtext[i] = paste("Asset", i, sep = " ")}
        }
        barplot(t(pos.weightedReturns), space = 0, ylab = "",
            xlim = c(xmin, xmax), ylim = c(ymin, ymax), col = col,
            border = "grey")
        legend("topright", legend = legendtext, bty = "n", cex = 0.8,
            fill = col)
    }
    barplot(t(neg.weightedReturns), space = 0, add = TRUE, col = col,
        border = "grey") 
    
    # Add Tailored Labels -  6 may be a good Number ...
    targetRisk = getTargetRisk(object)
    targetReturn = getTargetReturn(object)
    nSigma = length(targetRisk)
    nLabels = 6
    M = c(0, ( 1: (nSigma %/% nLabels) ) ) *nLabels + 1
    # Take a reasonable number of significant digits to plot, e.g. 2 ...
    nPrecision = 3
    axis(1, at = M, labels = signif(targetRisk[M], nPrecision))
    axis(3, at = M, labels = signif(targetReturn[M], nPrecision))
    
    # Add Axis Labels and Title:
    mtext("Target Risk", side = 1, line = 2, cex = .7)
    mtext("Target Return", side = 3, line = 2, cex = .7)
    mtext("Return", side = 2, line = 2, cex = .7)
      
    # Add Weights 0 and 1 Reference Lines
    lines(x = c(0, nSigma), c(1, 1), col = "grey", lty = 3) 
    lines(x = c(0, nSigma), c(0, 0), col = "grey", lty = 3)   
    
    # Add vertical Line at minimum risk:
    names(targetRisk) <- as.character(seq(1, nSigma, 1))
    minRisk = min(targetRisk)
    for(i in 1: nSigma){
          if(minRisk == targetRisk[i]) minRisk = targetRisk[i]
    }
    minRisk = as.numeric(names(minRisk))
    abline(v = minRisk, col = "black", lty = 3)
   
    # Complete to draw box ...
    box()
    
    # Return Value:
    invisible()
}


################################################################################


covEllipsesPlot = 
function(x = list(), ...)
{
    # Description:
    #   Plots covariance ellipses
    
    # Source:
    #   Partly based on function covfmEllipsesPlot() from
    #   Package: robust 0.2-2, 2006-03-24
    #   Maintainer: Kjell Konis <konis@stats.ox.ac.uk>
    #   Description: A package of robust methods.
    #   License: Insightful Robust Library License (see license.txt)
    
    # FUNCTION:
    
    # Settings:
    if (length(x) == 0) 
        stop("Input must be a list of at least 2 covariance matrices!")
    nModels = length(x)
    p = dim(x[[1]])[1]

    # Graphics Frame:
    plot(0, 0, xlim = c(0, p+1), ylim = c(0, p+1), type = "n",
         axes = FALSE, xlab = "", ylab = "", ...)
    box()

    # Correlation Ellipses:
    for(k in 1:nModels) {
        s = sqrt(diag(x[[k]]))
        X = x[[k]] / (s %o% s)
        xCenters = matrix(rep(1:p, p), byrow = TRUE, ncol = p)
        yCenters = matrix(rep(p:1, p), ncol = p)
        points = rep((c(0:180, NA) * pi)/90, (p^2 - p) / 2)
        cors = as.vector(rbind(matrix(X[row(X) < col(X)], nrow = 181, 
            ncol = (p^2 - p)/2, byrow = TRUE), rep(NA, (p^2 - p)/2)))
        xs = 0.475 * cos(points + acos(cors)/2) +
            rep(xCenters[row(xCenters) < col(xCenters)], each = 182)
        ys = 0.475 * cos(points - acos(cors)/2) +
            rep(yCenters[row(xCenters) < col(xCenters)], each = 182)   
        polygon(x = xs, y = ys, density = 0, col = k)
        shift = max(0.2, (p - 8)/88 + 0.2)
        xs = xCenters[row(xCenters) > col(xCenters)]
        ys = yCenters[row(yCenters) > col(yCenters)]
        cors = X[row(X) > col(X)]
        text(xs, ys + (((shift*(nModels - 1))/2) - shift*(k - 1)),
            labels = round(cors, digits = max(1, floor(20/p))),
            col = k, cex = min(1, 90/(p^2)))
    }

    # Diagonal Line:
    lines(c(0.5, p+0.5), c(p+0.5, 0.5), lwd = 2)

    # Correlation - Text:
    text(x = cbind(1:p, rep(p + 0.7, p)), 
        labels = dimnames(X)[[2]], cex = 1, adj = 0)
    text(x = cbind(rep(0.5, p), p:1), 
        labels = dimnames(X)[[1]], cex = 1, adj = 1)
    legend(x = (p+1)/2, y = 0, legend = unlist(paste("-", names(x), "-")), 
        xjust = 0.5, yjust = 0, text.col = 1:nModels, bty = "n")

    # Return Value:
    invisible()
}


################################################################################

