
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
# FUNCTION:                     EDUCATIONAL PORTFOLIO SLIDERS: 
#  weightsSlider                 Weights Slider           
################################################################################


.counterWeightsSlider = NA


weightsSlider =     
function(object, control = list(), ...)
{   # A function implemented by Rmetrics

    # Description:
    #    Interactive view of Portfolio Weights
    
    # FUNCTION:
     
    # Global Variables:
    object <<- object
    nFrontierPoints <- length(getTargetRisk(object)[ ,1])
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
        .counterWeightsSlider <<- .counterWeightsSlider + 1
        if (.counterWeightsSlider < 1) return ()
        
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
    .counterWeightsSlider <<- 0
    Start <- which.min(getTargetRisk(object)[ , 1])
    .sliderMenu(refresh.code, title = "Weights Slider",
       names =       c(                 "N"),
       minima =      c(                   1),
       maxima =      c(     nFrontierPoints),
       resolutions = c(con$sliderResolution),
       starts =      c(               Start))
    
    # Return Value:
    invisible()
}


################################################################################

