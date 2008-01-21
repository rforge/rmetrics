
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
# FUNCTION:                     PRINT AND PLOT METHODS:           
#  plot.fPORTFOLIO               S3 Plot method for 'fPORTFOLIO' objects   
################################################################################


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
    con <- list(
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
    
    ## par(mar = c(5, 4, 4, 3) + 0.1) ##

    attr(x, "control") <- con
   
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


# ------------------------------------------------------------------------------


.fportfolio.plot.1 <- 
function(x) 
{
    # FUNCTION:
    
    # Control:
    con = attr(x, "control")
    Type = getType(x)
    if (Type == "MV") {
        xLab = "Mean-Var Target Risk"
    } else if (Type == "CVaR") {
        xLab = "-CVaR Target Risk"
    }
    
    # Plot:
    frontierPlot(
        object = x, 
        xlim = con$xlim,
        ylim = con$ylim, 
        main = "Efficient Frontier",
        xlab = xLab, 
        ylab = "Target Return", 
        pch = 19, 
        cex = 0.75)
} 


# ------------------------------------------------------------------------------

      
.fportfolio.plot.2 <- 
function(x) 
{
    # FUNCTION:
    
    # Control:
    con = attr(x, "control")
    
    # Plot:
    .minvariancePlot(
        object = x, 
        col = con$minvariance.col, 
        cex = con$minvariance.cex, 
        pch = 19)
} 


# ------------------------------------------------------------------------------

      
.fportfolio.plot.3 <-
function(x) 
{
    # FUNCTION:
    
    # Control:
    con = attr(x, "control")
    
    # Plot:
    .tangencyPlot(
        object = x, 
        col = con$tangency.col, 
        cex = con$tangency.cex, 
        pch = 17)
}


# ------------------------------------------------------------------------------


.fportfolio.plot.4 <- 
function(x) 
{
    # FUNCTION:
    
    # Control:
    con = attr(x, "control")
    
    # Plot:
    .singleAssetPlot(
        object =x , 
        col = con$singleAsset.col, 
        cex = con$singleAsset.cex, 
        pch = 18)
}       


# ------------------------------------------------------------------------------


.fportfolio.plot.5 <- 
function(x) 
{
    # FUNCTION:
    
    # Control:
    con = attr(x, "control")
    
    # Plot:
    .equalWeightsPlot(
        object = x, 
        col = con$equalWeights.col, 
        cex = con$equalWeights.cex, 
        pch = 15)
}  


# ------------------------------------------------------------------------------


.fportfolio.plot.6 <- 
function(x) 
{
    # FUNCTION:
    
    # Control:
    con = attr(x, "control")
    
    # Plot:
    .singleAssetPlot(
        object = x , 
        col = con$singleAsset.col, 
        cex = con$singleAsset.cex, 
        pch = 18)
    lines(getFrontier(object = x), col = "grey")
    .twoAssetsPlot(object = x, col = con$twoAssets.col) 
}       


# ------------------------------------------------------------------------------


.fportfolio.plot.7 <- 
function(x) 
{
    # FUNCTION:
    
    # Control:
    con = attr(x, "control")
    
    # Plot:
    .weightsWheel(
        object = x,
        piePos = con$PiePos, 
        pieR = con$pieR, 
        pieOffset = con$pieOffset)
}  


# ------------------------------------------------------------------------------


.fportfolio.plot.8 <- 
function(x) 
{
    # FUNCTION:
    
    # Control:
    con = attr(x, "control")
    
    # Plot:
    .monteCarloPlot(
        object = x, 
        col = con$monteCarlo.col, 
        cex = con$monteCarlo.cex, 
        mcSteps = con$mcSteps) 
}


# ------------------------------------------------------------------------------


.fportfolio.plot.9 <- 
function(x) 
{
    # FUNCTION:
    
    # Control:
    con = attr(x, "control")
    
    # Plot:
    .sharpeRatioPlot(
        object = x, 
        type = "l", 
        col = con$sharpeRatio.col, 
        cex = con$sharpeRatio.cex, 
        lty = 3)
}       


################################################################################

