
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
#  portfolioFrontier             Returns the efficient frontier of a portfolio
# FUNCTION:                     SINGLE PORTFOLIOS:
#  feasiblePortfolio             Returns a feasible portfolio
#  efficientPortfolio            Returns a frontier portfolio
#  cmlPortfolio                  Returns capital market line
#  tangencyPortfolio             Returns the tangency portfolio
#  minvariancePortfolio          Returns the minimum variance portfolio
# FUNCTION:                     PRINT AND PLOT METHODS:           
#  show.fPORTFOLIO               S4 Print method for 'fPPORTFOLIO' objects
#  plot.fPORTFOLIO               S3 Plot method for 'fPORTFOLIO' objects   
#  summary.fPORTFOLIO            S3 Summary method for 'fPORTFOLIO' objects
# FUNCTION:                     EDUCATIONAL PORTFOLIO SLIDERS: 
#  weightsSlider                 Weights Slider           
#  frontierSlider                Efficient Frontier Slider
################################################################################


setClass("fPORTFOLIO", 
    representation(
        call = "call",
        data = "list",
        spec = "list",
        constraints = "character",
        portfolio = "list",
        title = "character",
        description = "character")  
)


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
    #   constraints - a character vector or NULL
    
    # FUNCTION:
    
    # Compose Portfolio Data: 
    data = portfolioData(data, spec)
    
    # Compose Optimization Function:
    if(is.null(constraints) | length(constraints) == 0) {
        Model = c("Constrained", "LongOnly")
        nAssets = getNumberOfAssets(data)
        constraints = paste("minW[1:", nAssets, "]=0", sep = "")
    } else if (constraints[1] == "Short") {
        Model = "Short"
    } else {
        Model = "Constrained"
    }         
    Type = getType(spec)
    fun = match.fun(paste(".portfolio", Model[1], Type[1], "Frontier", 
        sep = ""))
    attr(constraints, "model") = Model
    
    # Compute Portfolio:
    ans = fun(data, spec, constraints)
    attr(ans@constraints, "model") = Model
    
    # Reset Call:
    ans@call = match.call() 
    
    # Return Value:
    ans   
}


################################################################################


feasiblePortfolio =
function(data, spec = portfolioSpec(), constraints = NULL)
{   # A function implemented by Rmetrics
    # Description:
    #   Computes Risk and Return for a feasible portfolio
    
    # Arguments:
    #   data - a rectangular object of assets
    #   spec - an object of class 'fPFOLIOSPEC'
    #   constraints - a character vector or NULL
    
    # FUNCTION:
    
    # Compose Portfolio Data: 
    data = portfolioData(data, spec)
    
    # Constraints:
    # .checkPortfolioConstraints
    
    # Compose Optimization Function:
    if(is.null(constraints) | length(constraints) == 0) {
        Model = c("Constrained", "LongOnly")
        nAssets = getNumberOfAssets(data)
        constraints = paste("minW[1:", nAssets, "]=0", sep = "")
    } else if (constraints[1] == "Short") {
        Model = "Short"
    } else {
        Model = "Constrained"
    }         
    Type = getType(spec)
    fun = match.fun(paste(".feasible", Model[1], Type[1], "Portfolio", 
        sep = ""))
    attr(constraints, "model") = Model
    
    # Compute Portfolio:
    ans = fun(data, spec, constraints)
    attr(ans@constraints, "model") = Model
    
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
    #   constraints - a character vector or NULL
     
    # FUNCTION:
    
    # Compose Portfolio Data: 
    data = portfolioData(data, spec)
    
    # Compose Optimization Function:
    if(is.null(constraints) | length(constraints) == 0) {
        Model = c("Constrained", "LongOnly")
        nAssets = getNumberOfAssets(data)
        constraints = paste("minW[1:", nAssets, "]=0", sep = "")
    } else if (constraints[1] == "Short") {
        Model = "Short"
    } else {
        Model = "Constrained"
    }         
    Type = getType(spec)
    fun = match.fun(paste(".cml", Model[1], Type[1], "Portfolio", 
        sep = ""))
    attr(constraints, "model") = Model
    
    # Compute Portfolio
    ans = fun(data, spec, constraints)
    attr(ans@constraints, "model") = Model
    
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
    #   constraints - a character vector or NULL
     
    # FUNCTION:
    
    # Compose Portfolio Data: 
    data = portfolioData(data, spec)
    
    # Compose Optimization Function:
    if(is.null(constraints) | length(constraints) == 0) {
        Model = c("Constrained", "LongOnly")
        nAssets = getNumberOfAssets(data)
        constraints = paste("minW[1:", nAssets, "]=0", sep = "")
    } else if (constraints[1] == "Short") {
        Model = "Short"
    } else {
        Model = "Constrained"
    }        
    Type = getType(spec)
    fun = match.fun(paste(".tangency", Model[1], Type[1], "Portfolio", 
        sep = ""))
    attr(constraints, "model") = Model
    
    # Compute Portfolio:
    ans = fun(data, spec, constraints)
    attr(ans@constraints, "model") = Model
    
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
    #   constraints - a character vector or NULL
    
    # FUNCTION:
    
    # Compose Portfolio Data: 
    data = portfolioData(data, spec)
    
    # Compose Optimization Function:
    if(is.null(constraints) | length(constraints) == 0) {
        Model = c("Constrained", "LongOnly")
        nAssets = getNumberOfAssets(data)
        constraints = paste("minW[1:", nAssets, "]=0", sep = "")
    } else if (constraints[1] == "Short") {
        Model = "Short"
    } else {
        Model = "Constrained"
    }       
    Type = getType(spec)
    fun = match.fun(paste(".minvariance", Model[1], Type[1], "Portfolio", 
        sep = ""))
    attr(constraints, "model") = Model
    
    # Compute Portfolio:
    ans = fun(data, spec, constraints)
    attr(ans@constraints, "model") = Model
    
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
    #   constraints - a character vector or NULL
    
    # FUNCTION:
    
    # Compose Portfolio Data: 
    data = portfolioData(data, spec)
    
    # Compose Optimization Function:
    if(is.null(constraints) | length(constraints) == 0) {
        Model = c("Constrained", "LongOnly")
        nAssets = getNumberOfAssets(data)
        constraints = paste("minW[1:", nAssets, "]=0", sep = "")
    } else if (constraints[1] == "Short") {
        Model = "Short"
    } else {
        Model = "Constrained"
    }         
    Type = getType(spec)
    fun = match.fun(paste(".efficient", Model[1], Type[1], "Portfolio", 
        sep = ""))
    attr(constraints, "model") = Model
    
    # Compute Portfolio:
    ans = fun(data, spec, constraints)
    attr(ans@constraints, "model") = Model
    
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
    weights = round(getWeights(object), digits = 4)
    if (length(weights) == 1) {
        cat(" ", weights, "\n")
    } else {
        print.table(weights)
    }
    
    # Covariance Risk Budgets:
    cat("\nRiskBudget(s):\n")
    riskBudgets = round(getCovRiskBudgets(object), digits = 4)
    if (length(riskBudgets) == 1) {
        cat(" ", riskBudgets, "\n")
    } else {
        print.table(riskBudgets)
    }
    
    # Tail Risk Budgets:
    if (FALSE) {
        if (!is.na(getTailRiskBudgets(object))) {
             cat("\nRiskBudget(s):\n")
            riskBudgets = round(getTailRiskBudgets(object), digits = 4)
            if (length(riskBudgets) == 1) {
                cat(" ", riskBudgets, "\n")
            } else {
                print.table(riskBudgets)
            }   
        }  
    }
  
    # Target Returns:   
    # cat("\nTarget Return(s):\n")
    targetReturn = object@portfolio$targetReturn # getTargetReturn(object)
    # print(targetReturn)
 
    # Target Risk:
    # cat("\nTarget Risk(s):\n")
    targetRisk = object@portfolio$targetRisk # getTargetRisk(object) 
    # print(targetRisk)
    
    ##
    spec = getSpec(object)
    cat("\nTarget Risk(s) and Return(s):\n")
    if (is.null(dim(targetReturn))) {
        targetReturn = matrix(targetReturn, nrow = 1)
        colnames(targetReturn) = getEstimator(spec)[1]
    }
    if (is.null(dim(targetRisk))) {
        targetRisk = matrix(targetRisk, nrow = 1)
        colnames(targetRisk) = getEstimator(spec)[2]
    }
    target = cbind(targetReturn, targetRisk)
    colnames(target) = c(colnames(targetReturn), colnames(targetRisk))    
    if (nrow(target) == 1) {
        print(target[1, ])
    } else {
        print(target)
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
    #   Plot method for an object of class 'fPORTFOLIO'
    
    # Note:
    #   This method can also be used for plotting graphs fitted by 
    #   the function 'garch' from the contributed R package 'tseries'.
    
    # FUNCTION:
    
    # Control Parameters:
    Statistics = getStatistics(x)
         
    # Use default, if xlim and ylim is not specified ...
    mu = Statistics$mu
    Sigma = Statistics$Sigma   
    N = length(mu)   
    yLim = range(mu) + 0.25*c(-diff(range(mu)), diff(range(mu)))
    
    # First, take care that all assets appear on the plot ...
    # sqrtSig = sqrt(diag(Sigma))
    # xLimAssets = c(
    #    min(sqrtSig), 
    #    max(sqrtSig))+ c(-0.4*diff(range(sqrtSig)), 0.1*diff(range(sqrtSig)))
    xRange = range(getFrontier(x)[, 1])    
    xDiff = diff(xRange)   
    xLimAssets = c(xRange[1] - 2.5*xDiff/10, xRange[2] + xDiff/10)
      
    # ... second take care that the whole frontier appears on the plot:
    fullFrontier = getFrontier(x)
    xLimFrontier = range(fullFrontier[, 1])
    xLim = range(c(xLimAssets, xLimFrontier))

    # Control List:
    con <<- list(
        sharpeRatio.col = "blue",
        minvariance.col = "red",
        tangency.col = "steelblue",
        cml.col = "green",
        equalWeights.col = "blue",
        singleAsset.col = rainbow(N),
        twoAssets.col = "grey",
        monteCarlo.col = "black",
        sharpeRatio.cex = 0.1,
        # Point Sizes:
        minvariance.cex = 1.25,
        tangency.cex = 1.25,
        cml.cex = 1.25,
        equalWeights.cex = 1.25,
        singleAsset.cex = 1.25,
        twoAssets.cex = 0.01,
        monteCarlo.cex = 0.01,
        # Frontier Limits:
        xlim = xLim,
        ylim = yLim,
        # Monte Carlo Steps:
        mcSteps = 5000,
        # Pie Settings:
        pieR = NULL, 
        piePos = NULL, 
        pieOffset = NULL
        )    
    con[(Names <- names(control))] <- control
    
    par(mar = c(5, 4, 4, 3) + 0.1)

   
    # Plot:
    interactivePlot(
        x,
        choices = c(
            "Plot Efficient Frontier",
            "Add Minimum Risk Portfolio",
            "Add Tangency Portfolio",
            "Add Risk/Return of Single Assets",
            "Add Equal Weights Portfolio",
            "Add Two Asset Frontiers [0-1 PF Only]",
            "Add Wheel Pie of Weights",
            "Add Monte Carlo Portfolios",
            "Add Sharpe Ratio [MV PF Only]"),
        plotFUN = c(
            ".fportfolio.plot.1", ".fportfolio.plot.2", ".fportfolio.plot.3", 
            ".fportfolio.plot.4", ".fportfolio.plot.5", ".fportfolio.plot.6", 
            ".fportfolio.plot.7", ".fportfolio.plot.8", ".fportfolio.plot.9"),
        which = which) 
            
    # Return Value:
    invisible(x)
} 


# Plot Function and Addons:


.fportfolio.plot.1 <- 
function(x, ...) 
{
    Type = getType(x)
    if (Type == "MV") {
        xLab = "Mean-Var Target Risk"
    } else if (Type == "CVaR") {
        xLab = "-CVaR Target Risk"
    }
    frontierPlot(object = x, xlim = con$xlim,
        ylim = con$ylim, main = "Efficient Frontier",
        xlab = xLab, ylab = "Target Return", 
        pch = 19, cex = 0.75)
} 

      
.fportfolio.plot.2 <- 
function(x, ...) 
{
    .minvariancePlot(object = x, 
        col = con$minvariance.col, cex = con$minvariance.cex, 
        pch = 19)
} 

      
.fportfolio.plot.3 <-
function(x, ...) 
{
    .tangencyPlot(object = x, 
        col = con$tangency.col, cex = con$tangency.cex, 
        pch = 17)
}


.fportfolio.plot.4 <- 
function(x, ...) 
{
    .singleAssetPlot(object =x , 
        col = con$singleAsset.col, cex = con$singleAsset.cex, 
        pch = 18)
}       


.fportfolio.plot.5 <- 
function(x, ...) 
{
    .equalWeightsPlot(object = x, 
        col = con$equalWeights.col, cex = con$equalWeights.cex, 
        pch = 15)
}  


.fportfolio.plot.6 <- 
function(x, ...) 
{
    .singleAssetPlot(object = x , 
        col = con$singleAsset.col, cex = con$singleAsset.cex, 
        pch = 18)
    lines(getFrontier(object = x), col = "grey")
    .twoAssetsPlot(object = x, col = con$twoAssets.col) 
}       


.fportfolio.plot.7 <- 
function(x, ...) 
{
    .weightsWheel(object = x,
        piePos = con$PiePos, pieR = con$pieR, pieOffset = con$pieOffset)
}  


.fportfolio.plot.8 <- 
function(x, ...) 
{
    .monteCarloPlot(object = x, 
        col = con$monteCarlo.col, cex = con$monteCarlo.cex, 
        mcSteps = con$mcSteps) 
}


.fportfolio.plot.9 <- 
function(x, ...) 
{
    .sharpeRatioPlot(object = x, type = "l", 
        col = con$sharpeRatio.col, cex = con$sharpeRatio.cex, 
        lty = 3)
}       


# ------------------------------------------------------------------------------


summary.fPORTFOLIO =
function(object, ...)
{   # A function implemented by Rmetrics

    # Description:
    #   Plot method for an object of class 'fPORTFOLIO'
    
    # Note:
    #   This method can also be used for plotting graphs fitted by 
    #   the function 'garch' from the contributed R package 'tseries'.
    
    # FUNCTION:

    # Summary:
    print(object)
    funCalled = as.character(object@call[1])
    if (funCalled == "portfolioFrontier") {      
        weightsPlot(object)
        attributesPlot(object)
        riskBudgetsPlot(object)
        # Plot Frontier:
        plot(object, which = 1)
    } else {
        weightsPie(object)
        attributesPie(object)
        riskBudgetsPie(object)
    }
          
    # Return Value:
    invisible(object)
} 



################################################################################


weightsSlider =     
function(object, control = list(), ...)
{   # A function implemented by Rmetrics

    # Description:
    #    Interactive view of Portfolio Weights
    
    # FUNCTION:
     
    # Global Variables:
    object <<- object
    nFrontierPoints <<- length(getTargetRisk(object)[ ,1])
    dim = dim(getWeights(object))[2]
        
    # Use default, if xlim and ylim is not specified ...
    mu = getStatistics(object)$mu
    Sigma = getStatistics(object)$Sigma      
    yLim = range(mu) + 0.25*c(-diff(range(mu)), diff(range(mu)))
    
    # First, take care that all assets appear on the plot ...
    sqrtSig = sqrt(diag(Sigma))
    xLimAssets = c(min(sqrtSig), max(sqrtSig))+
         c(-0.4*diff(range(sqrtSig)), 0.1*diff(range(sqrtSig)))
    
    # ... second take care that the whole frontier appears on the plot:
    fullFrontier = getFrontier(object)
    xLimFrontier = range(fullFrontier[, 1])
    xLim = range(c(xLimAssets, xLimFrontier))
    xLim[1] = xLim[1]-diff(xLim)/5
    
    # Control Parameters:
    con <<- list(
        sliderResolution = 1,     
        sliderFlag = "weights",
        runningPoint.col  = "red",
        minvariance.col = "red",
        tangency.col = "steelblue",
        singleAsset.col = rainbow(dim),
        minvariance.pch = 19,
        singleAsset.pch = 19,
        tangency.pch = 17,
        runningPoint.cex = 1.5,
        minvariance.cex = 1,
        tangency.cex = 1.25,
        singleAsset.cex = 1,
        xlim = xLim,
        ylim = yLim
        )    
    con[(Names <- names(control))] <- control
    
    # Internal Function:
    refresh.code = function(...)
    {
        # Startup Counter:
        .counter <<- .counter + 1
        if (.counter < 1) return ()
        
        # Sliders:
        N = .sliderMenu(no = 1)
        
        # Reset Frame:
        par(mfrow = c(2, 2), cex = 0.7)
        
        # Plot 1 - Weights Plot: 
        weightsPlot(object)
        abline(v = N, col = "black")
        
        # Plot 2 - Single Weights Plot:
        .notStackedWeightsPlot(object)
        abline(v = N, col = "black")

        # Plot 3 - Frontier Plot:
        frontier = getFrontier(object)
        fPoint = frontier[N, ]
        frontierPlot(object, xlim = con$xlim, ylim = con$ylim,
            xlab = "", ylab = "")
        mtext("Target Risk", side = 1, line = 2, cex = 0.7)
        mtext("Target Return", side = 2, line = 2, cex = 0.7)
        points(fPoint[1], fPoint[2], col = con$runningPoint.col, pch = 19,
            cex = con$runningPoint.cex)
        .tangencyPlot(object, col = con$tangency.col, pch = con$tangency.pch)
        .singleAssetPlot(object, col = con$singleAsset.col,
            cex = con$singleAsset.cex, pch = con$singleAsset.pch)
        .minvariancePlot(object, col = con$minvariance.col,
            cex = con$minvariancePlot.cex, pch = con$minvariance.pch)
        Title = paste(
            "Return =", signif(fPoint[2], 2), "|", 
            "Risk = ", signif(fPoint[1], 2))
        .addlegend(object = object, control = con)
        title(main = Title)
        grid()
        
        # Plot 4 - Weights Pie:
        Object = object
        Object@portfolio$weights = getWeights(object)[N, ]
        weightsPie(Object)
        targetReturn = signif(getTargetReturn(object)[N], 3)
        targetRisk = signif(getTargetRisk(object)[N], 3)
        Text = paste(
            "Target Return =", targetReturn, " | ", 
            "Target Risk =", targetRisk)
        mtext(Text, side = 1, line = 0, adj = 0, cex = 0.7)

    }
  
    # Open Slider Menu:
    .counter <<- 0
    Start <<- which.min(getTargetRisk(object)[ , 1])
    .sliderMenu(refresh.code, title = "Weights Slider",
       names =       c(                 "N"),
       minima =      c(                   1),
       maxima =      c(     nFrontierPoints),
       resolutions = c(con$sliderResolution),
       starts =      c(               Start))
    
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------
 

.frontierSlider.Add = NA


frontierSlider =     
function(object, control = list(), ...)
{   # A function implemented by Rmetrics

    # Description:
    #   Views interactively frontier and related plots
    
    # FUNCTION:
    
    # Global Variables:
    object <<- object
    nFrontierPoints <<- nrow(getWeights(object))
    dim = dim(getWeights(object))[2]
       
    # Use default, if xlim and ylim is not specified ...
    mu = getStatistics(object)$mu
    Sigma = getStatistics(object)$Sigma      
    yLim = range(mu) + 0.25*c(-diff(range(mu)), diff(range(mu)))
    
    # First, take care that all assets appear on the plot ...
    sqrtSig = sqrt(diag(Sigma))
    xLimAssets = c(min(sqrtSig), max(sqrtSig))+
         c(-0.4*diff(range(sqrtSig)), 0.1*diff(range(sqrtSig)))

    # ... second take care that the whole frontier appears on the plot:
    fullFrontier = getFrontier(object)
    xLimFrontier = range(fullFrontier[, 1])
    xLim = range(c(xLimAssets, xLimFrontier))
    xLim[1] = xLim[1]-diff(xLim)/5
    
    # Initial setting of the pies:
    Data = getSeries(object)
    Spec = getSpec(object)
    Constraints = getConstraints(object)
    tg = getTargetReturn(tangencyPortfolio(Data, Spec, Constraints))
    ef = getTargetReturn(object)
    piePos = which(diff(sign(as.vector(ef)-as.vector(tg))) > 0) 

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
        weightsPieR = NULL, 
        weightsPieOffset = NULL,
        attributesPieR = NULL, 
        attributesPieOffset = NULL,
        xlim = xLim,
        ylim = yLim
        )    
    con[(Names <- names(control))] <- control
     
    # Set and Reset 'mar': 
    oldmar = par()$mar
    on.exit(par(oldmar))  
    par(mar = c(5, 4, 4, 3) + 0.1)
    frontierPlot(object = object, pch = 19, xlim = con$xlim, ylim = con$ylim)
    
    # Internal Function:
    refresh.code = function(...)
    {
        
        # Sliders:  
        N = FrontierPoint = .tdSlider(no =  1)
        AddRemove         = .tdSlider(no =  2)
        riskFreeRate      = .tdSlider(no =  3)
        mcSteps           = .tdSlider(no =  4)
        
        type = as.integer(.tdSlider(obj.name = "type"))
        Add[type] <<- AddRemove
        
        print(type)
        print(Add)

        # Reset Frame:
        par(mfrow = c(1, 1))
        
        # Plots and Addons:
        frontierPlot(object = object, pch = 19, 
            xlim = con$xlim, ylim = con$ylim)
        ef = getFrontier(object)
        points(ef[N, 1], ef[N, 2], col = "red", pch = 19, cex = 1.5)
        
        
        if (Add[1] == 1) {
            .weightsWheel(object = object,
                piePos = N, pieR = con$weightsPieR,
                pieOffset = con$weightsPieOffset)
        }
        
        if (Add[2] == 1) {
            .attributesWheel(object = object,
                piePos = N, 
                pieR = con$attributesPieR,
                pieOffset = con$attributesPieOffset)
        }
            
        if (Add[3] == 1) {
            .addlegend(object = object, 
                control = con)
        }   
        
        if (Add[4] == 1) {
            .minvariancePlot(object = object, 
                col = con$minvariance.col, 
                cex = con$minvariance.cex, 
                pch = con$minvariance.pch)
        } 
        
        if (Add[5] == 1) {
            .tangencyPlot(object = object, 
                col = con$tangency.col, 
                cex = con$tangency.cex,
                pch = con$tangency.pch)
        }
            
        if (Add[6] == 1) {
            object@spec$spec@portfolio$riskFreeRate = riskFreeRate
            .cmlPlot(object, 
                col = con$cml.col, 
                cex = con$cml.cex, 
                pch = con$cml.pch)
        }
        
        if (Add[7] == 1) {
            .sharpeRatioPlot(object = object, 
                type = "l", 
                col = con$sharpeRatio.col, 
                cex = con$sharpeRatio.cex, 
                lty = 3)
        }
           
        if (Add[8] == 1) {
            .equalWeightsPlot(object = object, 
                col = con$equalWeights.col, 
                cex = con$equalWeights.cex, 
                pch = con$equalWeights.pch)
        }
        
        if (Add[9] == 1) {
            .singleAssetPlot(object = object, 
                col = con$singleAsset.col, 
                cex = con$singleAsset.cex, 
                pch = con$singleAsset.pch)
        }
        
        if (Add[10] == 1) {
            .twoAssetsPlot(object = object, 
                col = con$twoAssets.col) 
        }
        
        
        if (Add[11] == 1) {
            .monteCarloPlot(object = object, 
                col = con$monteCarlo.col, 
                cex = con$monteCarlo.cex, 
                mcSteps = mcSteps) 
        }
        
        fPoint = ef[N, ] 
        Title = paste(
            "Return =", signif(fPoint[2], 2), "|", 
            "Risk = ", signif(fPoint[1], 2))
        title(main = Title) 
        
        grid()           
    }
  
    nFP = nFrontierPoints
    maxRF = max(getTargetReturn(object))
    resRF = maxRF/100
    .frontierSlider.Add <<- rep(0, times = 11)
    
    .tdSlider(
        refresh.code,
        
        sl.names    = c(    "Frontier Point", 
                        "Remove | Add", 
                                "Risk Free Rate",
                                            "Monte Carlo Steps"),
        sl.mins     = c(      0,    0,         0,         1000),
        sl.maxs     = c(    nFP,    1,     maxRF,        25000),
        sl.deltas   = c(      1,    1,     resRF,         5000),
        sl.defaults = c( piePos,    1,         0,         1000),
        
        but.functions = list(
            function(...){
                .tdSlider(obj.name = "type", obj.value =  "1"); refresh.code()},
            function(...){
                .tdSlider(obj.name = "type", obj.value =  "2"); refresh.code()},
            function(...){
                .tdSlider(obj.name = "type", obj.value =  "3"); refresh.code()},
            function(...){
                .tdSlider(obj.name = "type", obj.value =  "4"); refresh.code()},
            function(...){
                .tdSlider(obj.name = "type", obj.value =  "5"); refresh.code()},
            function(...){
                .tdSlider(obj.name = "type", obj.value =  "6"); refresh.code()},
            function(...){
                .tdSlider(obj.name = "type", obj.value =  "7"); refresh.code()},
            function(...){
                .tdSlider(obj.name = "type", obj.value =  "8"); refresh.code()},
            function(...){
                .tdSlider(obj.name = "type", obj.value =  "9"); refresh.code()},               
            function(...){
                .tdSlider(obj.name = "type", obj.value = "10"); refresh.code()},         
            function(...){
                .tdSlider(obj.name = "type", obj.value = "11"); refresh.code()}
        ),
        
        but.names = c(
            "Remove | Add:  Weights Pie        ",
            "Remove | Add:  Attribute Pie      ",
            "Remove | Add:  Legend             ",
            "Remove | Add:  Min Variance PF    ", 
            "Remove | Add:  Tangency PF        ",
            "Remove | Add:  Capital Market Line",
            "Remove | Add:  Sharpe Ratio       ",
            "Remove | Add:  Equal Weights PF   ",
            "Remove | Add:  Single Assets      ",
            "Remove | Add:  Two Assets EFs     ",
            "Remove | Add:  Monte Carlo PFs    "),
            
        title = "Frontier Slider"
        )        
            
   .tdSlider(obj.name = "type", obj.value = "1", no = 1)
}


################################################################################

