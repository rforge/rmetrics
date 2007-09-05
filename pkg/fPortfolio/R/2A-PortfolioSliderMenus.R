
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
#  frontierSlider                Efficient Frontier Slider
################################################################################


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


# ------------------------------------------------------------------------------

 
.Add = NA
.type = NA


frontierSlider =     
function(object, control = list(), ...)
{   # A function implemented by Rmetrics

    # Description:
    #   Views interactively frontier and related plots
    
    # FUNCTION:
    
    # Rev 1729
    
    # Global Variables:
    object <<- object
    nFrontierPoints <- nrow(getWeights(object))
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
        N = FrontierPoint = .tdSliderMenu(no =  1)
        AddRemove         = .tdSliderMenu(no =  2)
        riskFreeRate      = .tdSliderMenu(no =  3)
        mcSteps           = .tdSliderMenu(no =  4)
        
        .type <<- as.integer(.tdSliderMenu(obj.name = "type"))
        .Add[.type] <<- AddRemove
        
        cat("\nType:")
        print(.type)
        cat("\nType:")
        print(.Add)

        # Reset Frame:
        par(mfrow = c(1, 1))
        
        # Plots and Addons:
        frontierPlot(object = object, pch = 19, 
            xlim = con$xlim, ylim = con$ylim)
        ef = getFrontier(object)
        points(ef[N, 1], ef[N, 2], col = "red", pch = 19, cex = 1.5)
        
        
        if (.Add[1] == 1) {
            .weightsWheel(object = object,
                piePos = N, pieR = con$weightsPieR,
                pieOffset = con$weightsPieOffset)
        }
        
        if (.Add[2] == 1) {
            .attributesWheel(object = object,
                piePos = N, 
                pieR = con$attributesPieR,
                pieOffset = con$attributesPieOffset)
        }
            
        if (.Add[3] == 1) {
            .addlegend(object = object, 
                control = con)
        }   
        
        if (.Add[4] == 1) {
            .minvariancePlot(object = object, 
                col = con$minvariance.col, 
                cex = con$minvariance.cex, 
                pch = con$minvariance.pch)
        } 
        
        if (.Add[5] == 1) {
            .tangencyPlot(object = object, 
                col = con$tangency.col, 
                cex = con$tangency.cex,
                pch = con$tangency.pch)
        }
            
        if (.Add[6] == 1) {
            object@spec$spec@portfolio$riskFreeRate = riskFreeRate
            .cmlPlot(object, 
                col = con$cml.col, 
                cex = con$cml.cex, 
                pch = con$cml.pch)
        }
        
        if (.Add[7] == 1) {
            .sharpeRatioPlot(object = object, 
                type = "l", 
                col = con$sharpeRatio.col, 
                cex = con$sharpeRatio.cex, 
                lty = 3)
        }
           
        if (.Add[8] == 1) {
            .equalWeightsPlot(object = object, 
                col = con$equalWeights.col, 
                cex = con$equalWeights.cex, 
                pch = con$equalWeights.pch)
        }
        
        if (.Add[9] == 1) {
            .singleAssetPlot(object = object, 
                col = con$singleAsset.col, 
                cex = con$singleAsset.cex, 
                pch = con$singleAsset.pch)
        }
        
        if (.Add[10] == 1) {
            .twoAssetsPlot(object = object, 
                col = con$twoAssets.col) 
        }
        
        
        if (.Add[11] == 1) {
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
    .Add <<- rep(0, times = 11)
    
    .tdSliderMenu(
        refresh.code,
        
        names       = c(    "Frontier Point", 
                        "Remove | Add", 
                                "Risk Free Rate",
                                            "Monte Carlo Steps"),
        minima      = c(      0,    0,         0,         1000),
        maxima      = c(    nFP,    1,     maxRF,        25000),
        resolutions = c(      1,    1,     resRF,         5000),
        starts      = c( piePos,    1,         0,         1000),
        
        but.functions = list(
            function(...){
                .tdSliderMenu(obj.name = "type", obj.value =  "1"); refresh.code()},
            function(...){
                .tdSliderMenu(obj.name = "type", obj.value =  "2"); refresh.code()},
            function(...){
                .tdSliderMenu(obj.name = "type", obj.value =  "3"); refresh.code()},
            function(...){
                .tdSliderMenu(obj.name = "type", obj.value =  "4"); refresh.code()},
            function(...){
                .tdSliderMenu(obj.name = "type", obj.value =  "5"); refresh.code()},
            function(...){
                .tdSliderMenu(obj.name = "type", obj.value =  "6"); refresh.code()},
            function(...){
                .tdSliderMenu(obj.name = "type", obj.value =  "7"); refresh.code()},
            function(...){
                .tdSliderMenu(obj.name = "type", obj.value =  "8"); refresh.code()},
            function(...){
                .tdSliderMenu(obj.name = "type", obj.value =  "9"); refresh.code()},               
            function(...){
                .tdSliderMenu(obj.name = "type", obj.value = "10"); refresh.code()},         
            function(...){
                .tdSliderMenu(obj.name = "type", obj.value = "11"); refresh.code()}
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
            
   .tdSliderMenu(obj.name = "type", obj.value = "1", no = 1)
}


################################################################################

