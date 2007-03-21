
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
# FUNCTION:                     PORTFOLIO CLASS:
#  'fPORTFOLIO'                  S4 Portfolio Class
# FUNCTION:                     SINGLE PORTFOLIOS:
#  feasiblePortfolio             Returns a feasible portfolio
#  cmlPortfolio                  Returns capital market line
#  tangencyPortfolio             Returns the tangency portfolio
#  minvariancePortfolio          Returns the minimum variance portfolio
#  efficientPortfolio            Returns a frontier portfolio
# FUNCTION:                     PORTFOLIO FRONTIER:
#  portfolioFrontier             Returns the efficient frontier of a portfolio
# FUNCTION:                     PRINT AND PLOT METHODS:        
#  show.fPORTFOLIO               S4 Print method for 'fPPORTFOLIO' objects   
#  plot.fPORTFOLIO               S3 Plot method for 'fPORTFOLIO' objects   
# FUNCTION:                     EDUCATIONAL PORTFOLIO SLIDERS: 
#  weightsSlider                 Weights Slider           
#  frontierSlider                Efficient Frontier Slider
################################################################################


setClass("fPORTFOLIO", 
    representation(
        call = "call",
        data = "list",
        specification = "fPFOLIOSPEC",
        constraints = "character",
        portfolio = "list",
        title = "character",
        description = "character")  
)


# ------------------------------------------------------------------------------


feasiblePortfolio =
function(data, spec = portfolioSpec(), constraints = NULL)
{   # A function implemented by Rmetrics
    # Description:
    #   Computes Risk and Return for a feasible portfolio
    
    # Arguments:
    #   data - a rectangular object of assets
    #   spec - an object of class 'fPFOLIOSPEC'
    
    # FUNCTION:
    
    # Check Data: 
    if (is.list(data)) {
        series = NA
        mu = data$mu
        Sigma = data$Sigma
        statistics = list(mu = mu, Sigma = Sigma)
    } else {
        series = data
        statistics = portfolioStatistics(data, spec)
        mu = statistics$mu
        Sigma = statistics$Sigma
    }
    data = list(series = series, statistics = statistics)
    
    # Compose Function:
    if(is.null(constraints) | length(constraints) == 0) {
        Model = "Constrained"
        nAssets = length(mu)
        constraintsString = paste("minW[1:", nAssets, "]=0", sep = "")
    } else if (constraints[1] == "short") {
        Model = "Short"
    }      
    Type = spec@model$type
    fun = match.fun(paste(".feasible", Model, Type, "Portfolio", sep = ""))
    
    # Compute Portfolio
    ans = fun(data, spec, constraints)
    
    # Reset Call:
    ans@call = match.call()
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


cmlPortfolio =
function(data, spec = portfolioSpec(), constraints = NULL)
{   # A function implemented by Rmetrics

    # Description:
    #   Computes Capital Market Line
    
    # Arguments:
    #   data - a rectangular object of assets
    #   spec - an object of class 'fPFOLIOSPEC'
     
    # FUNCTION:
    
    # Check Data: 
    if (is.list(data)) {
        series = NA
        mu = data$mu
        Sigma = data$Sigma
        statistics = list(mu = mu, Sigma = Sigma)
    } else {
        series = data
        statistics = portfolioStatistics(data, spec)
        mu = statistics$mu
        Sigma = statistics$Sigma
    }
    data = list(series = series, statistics = statistics)
    
    # Compose Function:
    if(is.null(constraints) | length(constraints) == 0) {
        Model = "Constrained"
        nAssets = length(mu)
        constraintsString = paste("minW[1:", nAssets, "]=0", sep = "")
    } else if (constraints[1] == "short") {
        Model = "Short"
    } else {
        Model = "Constrained"
    }        
    Type = spec@model$type
    fun = match.fun(paste(".cml", Model, Type, "Portfolio", sep = ""))
    
    # Compute Portfolio
    ans = fun(data, spec, constraints)
    
    # Reset Call:
    ans@call = match.call()
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


tangencyPortfolio =
function(data, spec = portfolioSpec(), constraints = NULL)
{   # A function implemented by Rmetrics

    # Description:
    #   Computes target risk and weights for the tangency portfolio
    
    # Arguments:
    #   data - a rectangular object of assets
    #   spec - an object of class 'fPFOLIOSPEC'
     
    # FUNCTION:
    
    # Check Data: 
    if (is.list(data)) {
        series = NA
        mu = data$mu
        Sigma = data$Sigma
        statistics = list(mu = mu, Sigma = Sigma)
    } else {
        series = data
        statistics = portfolioStatistics(data, spec)
        mu = statistics$mu
        Sigma = statistics$Sigma
    }
    data = list(series = series, statistics = statistics)
    
    # Compose Function:
    if(is.null(constraints) | length(constraints) == 0) {
        Model = "Constrained"
        nAssets = length(mu)
        constraintsString = paste("minW[1:", nAssets, "]=0", sep = "")
    } else if (constraints[1] == "short") {
        Model = "Short"
    } else {
        Model = "Constrained"
    }        
    Type = spec@model$type
    fun = match.fun(paste(".tangency", Model, Type, "Portfolio", sep = ""))
    
    # Compute Portfolio:
    ans = fun(data, spec, constraints)
    
    # Reset Call:
    ans@call = match.call() 

    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


minvariancePortfolio =
function(data, spec = portfolioSpec(), constraints = NULL)
{   # A function implemented by Rmetrics

    # Description:
    #   Computes minimum variance portfolio
    
    # Arguments:
    #   data - a rectangular object of assets
    #   spec - an object of class 'fPFOLIOSPEC'
    
    # FUNCTION:
    
    # Check Data: 
    if (is.list(data)) {
        series = NA
        mu = data$mu
        Sigma = data$Sigma
        statistics = list(mu = mu, Sigma = Sigma)
    } else {
        series = data
        statistics = portfolioStatistics(data, spec)
        mu = statistics$mu
        Sigma = statistics$Sigma
    }
    data = list(series = series, statistics = statistics)
    
    # Compose Function:
    if(is.null(constraints) | length(constraints) == 0) {
        Model = "Constrained"
        nAssets = length(mu)
        constraintsString = paste("minW[1:", nAssets, "]=0", sep = "")
    } else if (constraints[1] == "short") {
        Model = "Short"
    } else {
        Model = "Constrained"
    }        
    Type = spec@model$type
    fun = match.fun(paste(".minvariance", Model, Type, "Portfolio", sep = ""))
    
    # Compute Portfolio:
    ans = fun(data, spec, constraints)
    
    # Reset Call:
    ans@call = match.call() 
    
    # Return Value:
    ans   
}


# ------------------------------------------------------------------------------


efficientPortfolio =
function(data, spec = portfolioSpec(), constraints = NULL)
{   # A function implemented by Rmetrics

    # Description:
    #   Computes target risk and weights for an efficient portfolio
    
    # Arguments:
    #   data - a rectangular object of assets
    #   spec - an object of class 'fPFOLIOSPEC'
    
    # FUNCTION:
    
    # Check Data: 
    if (is.list(data)) {
        series = NA
        mu = data$mu
        Sigma = data$Sigma
        statistics = list(mu = mu, Sigma = Sigma)
    } else {
        series = data
        statistics = portfolioStatistics(data, spec)
        mu = statistics$mu
        Sigma = statistics$Sigma
    }
    data = list(series = series, statistics = statistics)
    
    # Compose Function:
    if(is.null(constraints) | length(constraints) == 0) {
        Model = "Constrained"
        nAssets = length(mu)
        constraintsString = paste("minW[1:", nAssets, "]=0", sep = "")
    } else if (constraints[1] == "short") {
        Model = "Short"
    } else {
        Model = "Constrained"
    }        
    Type = spec@model$type
    fun = match.fun(paste(".efficient", Model, Type, "Portfolio", sep = ""))
    
    # Compute Portfolio:
    ans = fun(data, spec, constraints)
    
    # Reset Call:
    ans@call = match.call() 
    
    # Return Value:
    ans   
}


# ------------------------------------------------------------------------------


portfolioFrontier =
function(data, spec = portfolioSpec(), constraints = NULL, 
title = NULL, description = NULL)
{   # A function implemented by Rmetrics

    # Description:
    #   Computes the efficient frontier of a portfolio
    
    # Arguments:
    #   data - a rectangular object of assets
    #   spec - an object of class 'fPFOLIOSPEC'
    
    # FUNCTION:
    
    # Check Data: 
    if (is.list(data)) {
        series = NA
        mu = data$mu
        Sigma = data$Sigma
        statistics = list(mu = mu, Sigma = Sigma)
    } else {
        series = data
        statistics = portfolioStatistics(data, spec)
        mu = statistics$mu
        Sigma = statistics$Sigma
    }
    data = list(series = series, statistics = statistics)
    
    # Compose Function:
    if(is.null(constraints) | length(constraints) == 0) {
        Model = "Constrained"
        nAssets = length(mu)
        constraintsString = paste("minW[1:", nAssets, "]=0", sep = "")
    } else if (constraints[1] == "short") {
        Model = "Short"
    } else {
        Model = "Constrained"
    }   
    Type = spec@model$type
    fun = match.fun(paste(".portfolio", Model, Type, "Frontier", sep = ""))
    
    # Compute Portfolio:
    ans = fun(data, spec, constraints)
    
    # Reset Call:
    ans@call = match.call() 
    
    # Return Value:
    ans   
}

  
################################################################################


show.fPORTFOLIO =
function(object)
{   # A function implemented by Rmetrics

    # Description:
    #   S4 Print Method for an object of class "fPORTFOLIO"
    
    # Arguments:
    #   object - an object of class "fPORTFOLIO"
    
    # FUNCTION:
     
    # Title:
    cat("\nTitle:\n ")
    cat(getTitle(object), "\n")
    
    # Call:
    cat("\nCall:\n ")
    print.default(getCall(object))
    
    # Target Weights:
    cat("\nPortfolio Weight(s):\n")
    weights = getWeights(object)
    if (length(weights) == 1) {
        cat(" ", weights, "\n")
    } else {
        print(weights)
    }

    # Target Returns:   
    cat("\nTarget Return(s):\n")
    targetReturn = getTargetReturn(object)
    if (length(targetReturn) == 1) {
        cat(" ", targetReturn, "\n")
    } else {
        print(targetReturn)
    }
    
    # Target Risk:
    cat("\nTarget Risk(s):\n")   
    targetRisk = getTargetRisk(object) 
    if (length(targetRisk) == 1) {
        cat(" ", targetRisk, "\n")
    } else {
        print(targetRisk)
    }
    
    # Target Stdev:
    cat("\nTarget Standard Deviation(s):\n")   
    targetStdev = getTargetStdev(object) 
    if (length(targetStdev) == 1) {
        cat(" ", targetStdev, "\n")
    } else {
        print(targetStdev)
    }
       
    # Description:
    cat("\nDescription:\n ")
    cat(getDescription(object), "\n")
        
    # Return Value: 
    invisible(object)
}


# ------------------------------------------------------------------------------


setMethod("show", "fPORTFOLIO", show.fPORTFOLIO)


# ------------------------------------------------------------------------------


plot.fPORTFOLIO =
function(x, which = "ask", control = list(), ...)
{   # A function implemented by Rmetrics

    # Description:
    #   Plot method for an object of class 'fGARCH'
    
    # Note:
    #   This method can also be used for plotting graphs fitted by 
    #   the function 'garch' from the contributed R package 'tseries'.
    
    # FUNCTION:
    
    # Control Parameters:
    N = length(x@data$statistics$mu)
         
    # Use default, if xlim and ylim is not specified ...
    mu = x@data$statistics$mu
    Sigma = x@data$statistics$Sigma      
    yLim = range(mu) + 0.25*c(-diff(range(mu)), diff(range(mu)))
    # First, take care that all assets appear on the plot ...
    sqrtSig = sqrt(diag(Sigma))
    xLimAssets = c(min(sqrtSig), max(sqrtSig))+
         c(-0.4*diff(range(sqrtSig)), 0.1*diff(range(sqrtSig)))
    # ... second take care that the whole frontier appears on the plot:
    fullFrontier = getFrontier(x)
    xLimFrontier = range(fullFrontier[, 1])
    xLim = range(c(xLimAssets, xLimFrontier))

    # Control List:
    con <<- list(
        sharpeRatio.col = "black",
        minvariance.col = "red",
        tangency.col = "steelblue",
        cml.col = "green",
        equalWeights.col = "blue",
        singleAsset.col = rainbow(N),
        twoAssets.col = "grey",
        monteCarlo.col = "black",
        
        sharpeRatio.cex = 0.1,
        minvariance.cex = 1,
        tangency.cex = 1.25,
        cml.cex = 1.25,
        equalWeights.cex = 0.8,
        singleAsset.cex = 1,
        twoAssets.cex = 0.01,
        monteCarlo.cex = 0.01,
        
        xlim.frontier = xLim,
        ylim.frontier = yLim,
        
        mcSteps = 5000,
        
        pieR = NULL, 
        piePos = NULL, 
        pieOffset = NULL
        )    
    con[(Names <- names(control))] <- control
    
    par(mar = c(5, 4, 4, 3) + 0.1)


    # Plot Function and Addons:
    plot.1 <<- function(x, ...) {
        frontierPlot(object = x, xlim = con$xlim.frontier,
            ylim = con$ylim.frontier)
    }       
    plot.2 <<- function(x, ...) {
        .sharpeRatioPlot(object = x, type = "l", 
            col = con$sharpeRatio.col, cex = con$sharpeRatio.cex, 
            lty = 3)
    } 
    plot.3 <<- function(x, ...) {
        .minvariancePlot(object = x, 
            col = con$minvariance.col, cex = con$minvariance.cex, 
            pch = 19)
    }       
    plot.4 <<- function(x, ...) {
        .tangencyPlot(object = x, 
            col = con$tangency.col, cex = con$tangency.cex, 
            pch = 17)
    }
    plot.5 <<- function(x, ...) {
        .singleAssetPlot(object =x , 
            col = con$singleAsset.col, cex = con$singleAsset.cex, 
            pch = 18)
    }       
    plot.6 <<- function(x, ...) {
        .equalWeightsPlot(object = x, 
            col = con$equalWeights.col, cex = con$equalWeights.cex, 
            pch = 15)
    }  
    plot.7 <<- function(x, ...) {
        .twoAssetsPlot(object = x, col = con$twoAssets.col) 
    }       
    plot.8 <<- function(x, ...) {
        .wheelPiePlot(object = x,
            piePos = con$PiePos, pieR = con$pieR, pieOffset = con$pieOffset)
    }  
    plot.9 <<- function(x, ...) {
        .monteCarloPlot(object = x, 
            col = con$monteCarlo.col, cex = con$monteCarlo.cex, 
            mcSteps = con$mcSteps) 
    }       
   
    # Plot:
    interactivePlot(
        x,
        choices = c(
            "Plot Efficient Frontier",
            "Add Sharpe Ratio",
            "Add minvariance Portfolio",
            "Add Tangency Portfolio",
            "Add Single Assets",
            "Add Equal Weights Portfolio",
            "Add Two Asset Frontiers",
            "Add Wheel Pie",
            "Add Monte Carlo Portfolios"),
        plotFUN = c(
            "plot.1", "plot.2", "plot.3", "plot.4", "plot.5",  
            "plot.6", "plot.7", "plot.8", "plot.9"),
        which = which) 
            
    # Return Value:
    invisible(x)
} 


################################################################################


weightsSlider =     
function(object, control = list(), ...)
{   # A function implemented by Rmetrics

    # Description:
    
    # FUNCTION:
     
    # Global Variables:
    object <<- object
    nFrontierPoints <<- length(getTargetRisk(object))
    dim = dim(getWeights(object))[2]
        
    # Use default, if xlim and ylim is not specified ...
    mu = object@data$statistics$mu
    Sigma = object@data$statistics$Sigma      
    yLim = range(mu) + 0.25*c(-diff(range(mu)), diff(range(mu)))
    # First, take care that all assets appear on the plot ...
    sqrtSig = sqrt(diag(Sigma))
    xLimAssets = c(min(sqrtSig), max(sqrtSig))+
         c(-0.4*diff(range(sqrtSig)), 0.1*diff(range(sqrtSig)))
    # ... second take care that the whole frontier appears on the plot:
    fullFrontier = getFrontier(object)
    xLimFrontier = range(fullFrontier[, 1])
    xLim = range(c(xLimAssets, xLimFrontier))

    # Control Parameters:
    con <<- list(
        sliderResolution = ceiling(nFrontierPoints/10),
        
        sliderFlag = "weights",
    
        runningPoint.col  = "red",
        minvariance.col = "red",
        tangency.col = "steelblue",
        singleAsset.col = rainbow(dim),

        minvariance.pch = 19,
        singleAsset.pch = 19,
        
        runningPoint.cex = 1.5,
        minvariance.cex = 1,
        tangency.cex = 1.25,
        singleAsset.cex = 1,
        
        xlim.frontier = xLim,
        ylim.frontier = yLim

        )    
    con[(Names <- names(control))] <- control
    
    # Internal Function:
    refresh.code = function(...)
    {
        # Sliders:
        N = .sliderMenu(no = 1)
        
        # Reset Frame:
        par(mfrow = c(2, 2), cex = 0.7)
        
        # Plot 1 - Weights Plot: 
        weightsPlot(object)
        title(main = "Weights", line = 3)
        # Weights Plot Pointer:
        abline(v = N, col = "black")
        
        # Plot 2 - Single Weights Plot:
        .notStackedWeightsPlot(object)
        # Weights Plot Pointer for not stacked:
        abline(v = N, col = "black")
        title(main = "Not Stacked Weights", line = 3)

        # Plot 3 - Frontier Plot:
        frontier = getFrontier(object)
        fPoint = frontier[N, ]
        frontierPlot(object, xlim = con$xlim.frontier, ylim = con$ylim.frontier,
            xlab = "", ylab = "")
        mtext("Risk", side = 1, line = 2, cex = .7)
        mtext("Weight", side = 2, line = 2, cex = .7)
        points(fPoint[1], fPoint[2], col = con$runningPoint.col, pch = 19,
            cex = con$runningPoint.cex)
        .tangencyPlot(object, col = con$tangency.col)
        .singleAssetPlot(object, col = con$singleAsset.col,
            cex = con$singleAsset.cex, pch = con$singleAsset.pch)
        .minvariancePlot(object, col = con$minvariance.col,
            cex = con$minvariancePlot.cex, pch = con$minvariance.pch)
        Title = paste(
            "Return =", signif(fPoint[1], 2), "|", 
            "Risk = ", signif(fPoint[2], 2))
        .addlegend(object = object, control = con)
        title(main = Title)
        grid()
        
        # Weights Pie:
        Object = object
        Object@portfolio$weights = getWeights(object)[N, ]
        weightsPie(Object)
        title(main = "Portfolio Pie")   
    }
  
    # Open Slider Menu:
    .sliderMenu(refresh.code,
       names =       c(                 "N"),
       minima =      c(                   1),
       maxima =      c(     nFrontierPoints),
       resolutions = c(con$sliderResolution),
       starts =      c(                   1))
    
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------
 

frontierSlider =     
function(object, control = list(), ...)
{   # A function implemented by Rmetrics

    # Description:
    
    # FUNCTION:
    
    # Global Variables:
    object <<- object
    nFrontierPoints <<- nrow(getWeights(object))
    dim = dim(getWeights(object))[2]
       
    # Use default, if xlim and ylim is not specified ...
    mu = object@data$statistics$mu
    Sigma = object@data$statistics$Sigma      
    yLim = range(mu) + 0.25*c(-diff(range(mu)), diff(range(mu)))
    # First, take care that all assets appear on the plot ...
    sqrtSig = sqrt(diag(Sigma))
    xLimAssets = c(min(sqrtSig), max(sqrtSig))+
         c(-0.4*diff(range(sqrtSig)), 0.1*diff(range(sqrtSig)))
    # ... second take care that the whole frontier appears on the plot:
    fullFrontier = getFrontier(object)
    xLimFrontier = range(fullFrontier[, 1])
    xLim = range(c(xLimAssets, xLimFrontier))

    # Control list:
    con <<- list(
        sliderFlag = "frontier",
    
        sharpeRatio.col = "black",
        minvariance.col = "red",
        tangency.col = "steelblue",
        cml.col = "green",
        equalWeights.col = "blue",
        singleAsset.col = rainbow(dim),
        twoAssets.col = "grey",
        monteCarlo.col = "black",
        
        minvariance.pch = 17,
        tangency.pch = 17,
        cml.pch = 17,
        equalWeights.pch = 15,
        singleAsset.pch = 18,
        
        sharpeRatio.cex = 0.1,
        minvariance.cex = 1,
        tangency.cex = 1.25,
        cml.cex = 1.25,
        equalWeights.cex = 0.8,
        singleAsset.cex = 1,
        twoAssets.cex = 0.01,
        monteCarlo.cex = 0.01,
        
        mcSteps = 5000,
        
        pieR = NULL, 
        piePos = NULL, 
        pieOffset = NULL,
        
        xlim.frontier = xLim,
        ylim.frontier = yLim
        )    
    con[(Names <- names(control))] <- control
     
    # Set and Reset 'mar': 
    oldmar = par()$mar
    on.exit(par(oldmar))  
    par(mar = c(5, 4, 4, 3) + 0.1)
    
    # Internal Function:
    refresh.code = function(...)
    {
        # Sliders:  
        N                = .sliderMenu(no =  1)
        pieFlag          = .sliderMenu(no =  2)
        attributePieFlag = .sliderMenu(no =  3)
        legendFlag       = .sliderMenu(no =  4)
        minvarianceFlag  = .sliderMenu(no =  5)
        tangencyFlag     = .sliderMenu(no =  6)
        cmlFlag          = .sliderMenu(no =  7)
        riskFreeRate     = .sliderMenu(no =  8)
        sharpeRatioFlag  = .sliderMenu(no =  9)
        equalWeightsFlag = .sliderMenu(no = 10)
        singleAssetFlag  = .sliderMenu(no = 11)
        twoAssetsFlag    = .sliderMenu(no = 12)
        mcFlag           = .sliderMenu(no = 13)
        mcSteps          = .sliderMenu(no = 14)

        # Reset Frame:
        par(mfrow = c(1, 1))
        
        # Plots and Addons:
        frontierPlot(object = object, pch = 19, ylim = con$ylim.frontier,
            xlim = con$xlim.frontier)
        ef = getFrontier(object)
        points(ef[N, 1], ef[N, 2], col = "red", pch = 19, cex = 1.5)
        if (sharpeRatioFlag) {
            .sharpeRatioPlot(object = object, type = "l", 
                col = con$sharpeRatio.col, cex = con$sharpeRatio.cex, 
                lty = 3)
        }
        if (minvarianceFlag) {
            .minvariancePlot(object = object, 
                col = con$minvariance.col, cex = con$minvariance.cex, 
                pch = con$minvariance.pch)
        }
        if (cmlFlag) {
            object@specification@portfolio$riskFreeRate = riskFreeRate
            data = object@data$statistics
            spec = object@specification
            constraintsString = object@constraints
            setRiskFreeRate(spec = object@specification,
                riskFreeRate = riskFreeRate)
            cml = cmlPortfolio(data = data, spec = spec,
               constraintsString = constraintsString)
            .cmlPlot(object = cml, 
                col = con$cml.col, cex = con$cml.cex, pch = con$cml.pch)
        }
        
        if (tangencyFlag) {
            .tangencyPlot(object = object, 
                col = con$tangency.col, cex = con$tangency.cex,
                pch = con$tangency.pch)
        }
        if (singleAssetFlag) {
            .singleAssetPlot(object = object, 
                col = con$singleAsset.col, cex = con$singleAsset.cex, 
                pch = con$singleAsset.pch)
        }
        if (equalWeightsFlag) {
            .equalWeightsPlot(object = object, 
                col = con$equalWeights.col, cex = con$equalWeights.cex, 
                pch = con$equalWeights.pch)
        }
        if (twoAssetsFlag) {
            .twoAssetsPlot(object = object, col = con$twoAssets.col) 
        }
        if (pieFlag) {
            .wheelPiePlot(object = object,
                piePos = N, pieR = con$PieR, pieOffset = con$pieOffset)
        }
        if (mcFlag) {
            .monteCarloPlot(object = object, 
                col = con$monteCarlo.col, cex = con$monteCarlo.cex, 
                mcSteps = mcSteps) 
        }
        if (legendFlag) {
            .addlegend(object = object, control = con)
        } 
        if (attributePieFlag) {
            .attPiePlot(object = object,
                piePos = N, pieR = con$PieR, pieOffset = con$pieOffset)
            }
        fPoint = ef[N, ] 
        Title = paste(
            "Return =", signif(fPoint[1], 2), "|", 
            "Risk = ", signif(fPoint[2], 2))
        title(main = Title)            
    }
  
    # Open Slider Menu:
    nFP = nFrontierPoints
    # #RF
    maxRF = max(getTargetReturn(object))
    resRF = maxRF/100
    .sliderMenu(refresh.code,
        names =       c(  "Select Frontier Point  ",
                          "Add Weights Pie        ",
                          "Add Attribute Pie      ",
                          "Add Legend             ",
                          "Add Min Variance PF    ", 
                          "Add Tangency PF        ",
                          "Add Capital Market Line",
                          "Risk Free Rate         ",
                          "Add Sharpe Ratio       ",
                          "Add Equal Weights PF   ",
                          "Add Single Assets      ",
                          "Add Two Assets EFs     ",
                          "Add Monte Carlo PFs    ",
                          "# of MC Steps          "),
                          
            #  frontierPoints Pie Pie L mv tg cml    #RF SR EW SA TA MC   #MC 
                    #       1   2   2 3  4  5   7      8  6  9 10 11 12    13 
        minima =      c(    1,  0,  0,0, 0, 0,  0,     0, 0, 0, 0, 0, 0,    0),
        maxima =      c(  nFP,  1,  1,1, 1, 1,  1, maxRF, 1, 1, 1, 1, 1,25000),
        resolutions = c(    1,  1,  1,1, 1, 1,  1, resRF, 1, 1, 1, 1, 1, 1000),
        starts =      c(    1,  1,  0,1, 1, 1,  1,     0, 0, 0, 1, 0, 0, 1000))
      
    # Return Value:                                                 
    invisible()
}


################################################################################

