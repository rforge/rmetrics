
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# A copy of the GNU General Public License is available via WWW at
# http://www.gnu.org/copyleft/gpl.html.  You can also obtain it by
# writing to the Free Software Foundation, Inc., 59 Temple Place,
# Suite 330, Boston, MA  02111-1307  USA. 

# Copyrights (C)
# for this R-port: 
#   1999 - 2004, Diethelm Wuertz, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:
################################################################################


# ******************************************************************************
# A2-ExtremesPlots


.fExtremes.A2Cmd.1  = function() .fExtremes.A2Cmd(1)
.fExtremes.A2Cmd.2  = function() .fExtremes.A2Cmd(2)
.fExtremes.A2Cmd.3  = function() .fExtremes.A2Cmd(3)
.fExtremes.A2Cmd.4  = function() .fExtremes.A2Cmd(4)
.fExtremes.A2Cmd.5  = function() .fExtremes.A2Cmd(5)
.fExtremes.A2Cmd.6  = function() .fExtremes.A2Cmd(6)
.fExtremes.A2Cmd.7  = function() .fExtremes.A2Cmd(7)
.fExtremes.A2Cmd.8  = function() .fExtremes.A2Cmd(8)
.fExtremes.A2Cmd.9  = function() .fExtremes.A2Cmd(9)
.fExtremes.A2Cmd.10 = function() .fExtremes.A2Cmd(10)
.fExtremes.A2Cmd.11 = function() .fExtremes.A2Cmd(11)
.fExtremes.A2Cmd.12 = function() .fExtremes.A2Cmd(12)
.fExtremes.A2Cmd.13 = function() .fExtremes.A2Cmd(13)


.fExtremes.A2Cmd <-
function(choice)
{
    # Description:
    #   Extremes Plots
    
    # Menu:
    What = c(
        "Example timeSeries: x = NYSE Returns",
        "Empirical Distribution Function",
        "Quantile Quantile Plot",
        "QQ-Plot with 95% Intervals",
       #"Exploratory QQ Plot for EV Analysis",
        "Sample Mean Excess Plot",
        "... Mean Excess Function Plot",
        "... Mean Residual Life Plot",
        "Records Development",
        "... Development of Subsamples",
        "Ratio of Maximums and Sums",
        "ACF Exceedences of Heights",
        "... of Distances")
        
    # Functions:
    fun = c(
        "",
        "emdPlot",
        "qqPlot",
        "qqbayesPlot",
        "qplot",
        "mxfPlot",
        "mrlPlot",
        "mePlot",
        "recordsPlot",
        "ssrecordsPlot",
        "msratioPlot",
        ".xacfHeightsPlot",
        ".xacfDistancesPlot",)
            
    # Choices:    
    if (choice == 1) {
        # Example timeSeries:
        data(nyse)
        X = as.timeSeries(nyse)
        X = returnSeries(X)
        X = X[X@Data < 1] # Correct for OLD/NEW Index
        x <<- .saveAs(X, "NYSE Returns")
        print(start(x))
        print(tail(x))
    } else {    
        par(mfrow = c(1,1), cex = 0.7)
        FUN = match.fun(fun[choice])
        ans = FUN(x)
    }
    
}


# ******************************************************************************
# A3-ExtremesPreprocessing


.fExtremes.A3Cmd.1 = function(...) .fExtremes.A3Cmd(1)
.fExtremes.A3Cmd.2 = function(...) .fExtremes.A3Cmd(2)
.fExtremes.A3Cmd.3 = function(...) .fExtremes.A3Cmd(3)
.fExtremes.A3Cmd.4 = function(...) .fExtremes.A3Cmd(4)
.fExtremes.A3Cmd.5 = function(...) .fExtremes.A3Cmd(5)
.fExtremes.A3Cmd.6 = function(...) .fExtremes.A3Cmd(6)


.fExtremes.A3Cmd = 
function(choice)
{
    # Description:
    #   Extremes Preprocessing
    
    # Menu:
    What = c(
        "Example timeSeries: x = NYSE Returns",
        "5% Series Points below Threshold Value",
        "5% Series Points above Threshold Value",
        "Monthly Block Minima Series Points",
        "Monthly Block Maxima Series Points")
      
    # Choices:  
    if(choice == 1) {
        # Example timeSeries:
        .dataSet("nyse")
    }       
    if(choice == 2) {
        # 5% Series Points below Threshold Value"
        .fun = function(series, object2x) {
            x = eval(parse(text = series))
            if (!is.timeSeries(x)) 
                stop("x must be a timeSeries object!")
            threshold = findThreshold(x)
            object <<- x[x@Data < threshold]
            if (object2x) 
                x <<- .saveAs(
                    data = object,
                    infoName = "Points Below Threshold") 
            plot(object, type = "h", ylab = "Below")
            object }
        .objectMenu(
            params = list(
                series = "x", 
                object2x = TRUE),
            infoName = "Points below Threshold",
            tkoutput = FALSE,
            console = "print(head(object))" )     
    }   
    if(choice == 3) {
        # 5% Series Points above Threshold Value
        .fun = function(series, object2x) {
            x = eval(parse(text = series))
            if (!is.timeSeries(x)) 
                stop("x must be a timeSeries object!")
            threshold = findThreshold(x)
            object <<- x[x@Data > threshold]
            if (object2x) 
                x <<- .saveAs(
                    data = object,
                    infoName = "Points Above Threshold") 
            plot(object, type = "h", ylab = "Above")
            object }
        .objectMenu(
            params = list(
                series = "x", 
                object2x = FALSE),
            infoName = "Points Above Threshold",
            tkoutput = FALSE,
            console = "print(head(object))" )     
    }   
    if(choice == 4) {
        # Monthly Block Minima Series Points
        .fun = function(series, object2x) {
            x = eval(parse(text = series))
            blockValues = -as.matrix(blockMaxima(-x, "month"))
            units = paste(x@units, ".BLOCK", sep = "")
            object <<- timeSeries(blockValues[,1], names(blockValues[, 1]), 
                units = units, FinCenter = "GMT")
            if (object2x) 
                x <<- .saveAs(
                    data = object,
                    infoName = "Block Minima") 
            plot(object, type = "h", ylab = "Minima")
            title(main = "Block Minima")
            title(main = paste("\n\n", x@units))
            object }
        .objectMenu(
            params = list(
                series = "x", 
                object2x = FALSE),
            infoName = "Monthly Block Minima",
            tkoutput = FALSE,
            console = "print(head(object))" )            
    }   
    if(choice == 5) {
        # Monthly Block Maxima Series Points
        .fun = function(series, object2x) {
            x = eval(parse(text = series))
            blockValues = as.matrix(blockMaxima(x, "month"))
            units = paste(x@units, ".BLOCK", sep = "")
            object <<- timeSeries(blockValues[,1], names(blockValues[, 1]), 
                units = units, FinCenter = "GMT")
            if (object2x) 
                x <<- .saveAs(
                    data = object,
                    infoName = "Monthly Block Maxima") 
            plot(object, type = "h", ylab = "Maxima")
            title(main = "Block Maxima")
            title(main = paste("\n\n", x@units))
            object }
        .objectMenu(
            params = list(
                series = "x", 
                object2x = FALSE),
            infoName = "Monthly Block Maxima",
            tkoutput = FALSE,
            console = "print(head(object))" )               
    }  
}


# ******************************************************************************
# B1-GevDistribution


.fExtremes.B1Cmd.1 = function() .fExtremes.B1Cmd(1)
.fExtremes.B1Cmd.2 = function() .fExtremes.B1Cmd(2)


.fExtremes.B1Cmd = 
function(choice)
{
    # Description:
    #   GEV Distribution
    
    # Menu:
    What = c(
        "Generate GEV Random Numbers",
        "GEV Distribution Slider")
        
    # Choices:
    if (choice == 1) {
        # Random Number Generation"
        .gevSlider(GenerateRandomNumbers = TRUE)
    }
    if (choice == 2) {
        # GEV Slider:
        .gevSlider()
    }
}


# ******************************************************************************
# B2-GevFit

.fExtremes.B2Cmd.1 = function(...) .fExtremes.B2Cmd(1)
.fExtremes.B2Cmd.2 = function(...) .fExtremes.B2Cmd(2)
.fExtremes.B2Cmd.3 = function(...) .fExtremes.B2Cmd(3)
.fExtremes.B2Cmd.4 = function(...) .fExtremes.B2Cmd(4)
.fExtremes.B2Cmd.5 = function(...) .fExtremes.B2Cmd(5)
.fExtremes.B2Cmd.6 = function(...) .fExtremes.B2Cmd(6)

.fExtremes.B2Cmd = 
function(choice)
{
    # Description:
    #   GEV Fit
    
    # Menu:
    What = c(
        "BMW Monthly Max Losses",
        "Simulate GEV Series", 
        "Fit GEV Parameters",
        "... Print Summary Report",
        "... Fitted Values",
        "... Residual Values",
        "GEV Return Level Plot")
    
    # Choices:
    if (choice == 1) {
        .dataSet(bmwmax)
    }
    if (choice == 2) {
        # Simulate GEV Series:
        .fun <<- function(n, shape, location, scale, object2x) {
            object <<- gevSim(model = list(shape = shape, location = location, 
                scale = scale), n = n) 
            if (object2x) 
                x <<- .saveAs(
                    data = object,
                    infoName = "Simulated GEV Series") 
            object }
        .objectMenu(
            params = list(
                n = 1000, 
                shape = 0.25, 
                location = 0, 
                scale = 1,
                object2x = FALSE),
            infoName = "Simulated GEV Series",
            tkoutput = FALSE,
            console = "print(head(object))" )
    }
    if (choice == 3) {
        # Fit GEV Parameters:
        .fun <<- function(series, type, gumbel) { 
            x = eval(parse(text = series))
            object <<- gevFit(x = x, type = type, gumbel = gumbel)
            fit <<- .saveAs(
                data = object,
                infoName = "GEV Parameter Fit",
                what = "fitted") 
            object }
        .objectMenu(
            params = list(
                series = "x",
                type = "mle", 
                gumbel = FALSE),
            infoName = "GEV Parameter Fit",
            tkoutput = TRUE)
    }
    if (choice == 4) {
        # Summary Report:
        par(mfrow = c(2, 2), cex = 0.7)
        .tkOutput(capture.output(summary(fit)))
    }
    if (choice == 5) {
        # Fitted Values:        
        x <<- .saveAs(
            data = fit$fit$data -fit$fit$residuals, 
            infoName = "Vector of GEV Fitted Values",
            console = "cat('\nVector of GEV Fitted Values:\n', data[1:5]) ",
            what = "x" )
    }   
    if (choice == 6) {
        # Residual Values:
        x <<- .saveAs(
            data = fit$fit$residuals, 
            infoName = "Vector of GEV Residuals",
            console = "cat('\nVector of GEV Residuals:\n', data[1:5]) ",
            what = "x" )
    }
    if (choice == 7) {
        .fun <<- function(fit, k.blocks, add) { 
            fit = eval(parse(text = fit))
            object <<- function(object = fit, k.blocks = k.blocks, add = add)
            object }
        .objectMenu(
            params = list(
                object = "fit",
                k.blocks = 20, 
                add = FALSE),
            infoName = "GEV Return Level ",
            tkoutput = TRUE)
    }
}


# ******************************************************************************
# B3-GevGlmFit


.fExtremes.B3Cmd.1 = function() .fExtremes.B3Cmd(1)


.fExtremes.B3Cmd <-
function(choice)
{
    # Description:
    #   Gev Glm Fit 
    
    # Menu:
    What = c(
        "NYI: GEV GLM Fit")
        
        
    # Choices:
    if (choice == 1) {
        tkinsert(txt, "end", "\n\nSorry, not yet implemented!\n")
    }
}


# ******************************************************************************
# B4-MdaPlots


.fExtremes.B4Cmd.1 = function() .fExtremes.B4Cmd(1)
.fExtremes.B4Cmd.2 = function() .fExtremes.B4Cmd(2)
.fExtremes.B4Cmd.3 = function() .fExtremes.B4Cmd(3)


.fExtremes.B4Cmd <-
function(choice)
{
    # Description:
    #   MDA Plots
    
    # Menu:
    What = c(
        "Example timeSeries: x = BMW Log Returns",
        "Hill Plot",
        "Shape Parameter Plots")
        
    # Choices:
    if (choice == 1) {
        # Example timeSeries: x = BMW Log Returns
        .dataSet("bmw")
    }
    if (choice == 2) {
        # Hill Plot:
        .fun <<- function(series, option, start, end, reverse, p, ci) {
            x = eval(parse(text = series))
            par(mfcol = c(1, 1), cex = 0.7)
            object <<- hillPlot(x = x, option, start, end, reverse, p, ci, 
                autoscale = TRUE, labels = TRUE)
            object }
        .objectMenu(
            params = list(
                series = "x",
                option = "alpha", 
                start = 15, 
                end = NA, 
                reverse = FALSE, 
                p = NA, 
                ci = 0.95),
            infoName = "Hill Plot",
            tkoutput = FALSE )
    }
    if (choice == 3) {
        # Shape Parameter Plots:
        .fun <<- function(series, revert, standardize) {
             x = eval(parse(text = series))
             par(mfcol = c(3, 2), cex = 0.7)
             object <<- shaparmPlot(x = x, revert, standardize, 
                tails = 0.01*(1:10), doplot = c(FALSE, FALSE, FALSE, 
                    FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE), 
                which = c(TRUE, TRUE, TRUE), doprint = FALSE, both.tails = TRUE, 
                xi.range = c(-0.5, 1.5), alpha.range = c(0, 10))
             object }
        .objectMenu(
            params = list(
                series = "x",
                revert = FALSE, 
                standardize = FALSE),
            infoName = "Shape Parameter Plots",
            tkoutput = FALSE )
    }   
}


# ******************************************************************************
# C1-GpdDistribution


.fExtremes.C1Cmd.1 = function() .fExtremes.C1Cmd(1)
.fExtremes.C1Cmd.2 = function() .fExtremes.C1Cmd(2)


.fExtremes.C1Cmd <-
function(choice)
{
    # Description:
    #   GEV Distribution
    
    # Menu:
    What = c(
        "Generate GPD Random Numbers",
        "GPD Distribution Slider")
        
    # Choices:
    if (choice == 1) {
        # Random Number Generation"
        .gpdSlider(GenerateRandomNumbers = TRUE)
    }
    if (choice == 2) {
        # GEV Slider:
        .gpdSlider()
    }
}


# ******************************************************************************
# C3 - GpdFit


.fExtremes.C2Cmd.1 = function() .fExtremes.C2Cmd(1)
.fExtremes.C2Cmd.2 = function() .fExtremes.C2Cmd(2)
.fExtremes.C2Cmd.3 = function() .fExtremes.C2Cmd(3)


.fExtremes.C2Cmd <-
function(choice)
{
    # Description:
    #   Gpd Fit
    
    # menu:
    What = c(
        "Example timeSerie: x = Danish Fire Losses",
        "Simulate GPD Series", 
        "Fit GPD Parameters",
        "... Print Summary Report",
        "... Fitted Values",
        "... Residual Values")
            
    if (choice == 1) {
        # Example Data: x = Danish Fire Losses
        .dataSet("danish")
    }
    if (choice == 2) {
        # GPD Simulation:
        .fun <<- function(n, shape, location, scale, object2x) {
            object <<- gpdSim(model = list(shape = shape, location = location, 
                scale = scale), n = n) 
            if (object2x) 
                x <<- .saveAs(
                    data = object,
                    infoName = "Simulated GPD Series") 
            object }
        .objectMenu(
            params = list(
                n = 1000, 
                shape = 0.25, 
                location = 0, 
                scale = 1,
                object2x = FALSE),
            infoName = "Simulated GPD Series",
            tkoutput = FALSE,
            console = "print(head(object))" )
    }
    if (choice == 3) {
        # Fit GPD Parameters:
        .fun <<- function(series, threshold, nextremes, type, information) { 
            x = eval(parse(text = series))
            object <<- gpdFit(x = x, threshold = threshold, 
                nextremes = nextremes, type = type, information = information)
            fit <<- .saveAs(
                data = object,
                infoName = "GPD Parameter Fit",
                what = "fitted") 
            object }
        .objectMenu(
            params = list(
                series = "x",
                threshold = NA, 
                nextremes = NA, 
                type = "mle",
                information = "observed" ),
            infoName = "GPD Parameter Fit",
            tkoutput = TRUE)
    }
    if (choice == 4) {
        # Summary Report:
        par(mfrow = c(2, 2), cex = 0.7)
        .tkOutput(capture.output(summary(fit)))
    }
    if (choice == 5) {
        # Fitted Values:        
        object <<- .saveAs(
            data = fit$fitted.values, 
            infoName = "Vector of GEV Fitted Values",
            console = "cat('\nVector of GEV Fitted Values:\n', data[1:5]) ",
            what = "object" )
    }   
    if (choice == 6) {
        # Residual Values:
        object <<- .saveAs(
            data = fit$residuals, 
            infoName = "Vector of GEV Residuals",
            console = "cat('\nVector of GEV Residuals:\n', data[1:5]) ",
            what = "object" )
    }
    
    
    if (choice == 0) {
        .fun <<- function(fittedObject, pp, ci.type, ci.p, like.num) {
            x = gpdtailPlot(fit)
            object <<- gpdqPlot(x = x, pp = pp, ci.type = ci.type, 
                ci.p = 0.95, like.num = 50)
            title(main = "GPD q-Plot")
            object }
        .objectMenu(
            params = list(
                fittedObject = "fit",
                pp = 0.99,
                ci.type = "likelihood",
                ci.p = 0.95,
                like.num = 50),
            infoName = "GPD Risk Measures",
            tkoutput = FALSE)
    }
    
    if (choice == 0) {
        .fun <<- function(series, p, models, start, end, reverse, ci) {
            x = eval(parse(text = series))
            object <<- gpdquantPlot(x = x, p = p, models = models, 
                start = start, end = end, reverse = reverse, ci = ci,
                autoscale = TRUE, labels = TRUE)
            object }
        .objectMenu(
            params = list(
                series = "x",
                p = 0.99,
                models = 30,
                start = 15,
                end = 500,
                reverse = TRUE,
                ci = 0.95),
            infoName = "",
            tkoutput = FALSE)
    }
    
    
        
    if (choice == 0) {
        .fun <<- function(fittedObject, plevel) {
            x = eval(parse(text = fittedObject))
            plevel = eval(parse(text = plevel))
            object <<- gpdriskmeasures(x = x, plevels = plevel)
            .tkTitle("GPD Risk Measures")
            .tkOutput(capture.output(object))
            .tkDescription(date())
            object }
        .objectMenu(
            params = list(
                fittedObject = "fit",
                plevel = "c(0.99, 0.995, 0.999, 0.9995, 0.9999)" ),
            infoName = "GPD Risk Measures",
            tkoutput = FALSE)
    }
    
    
    if (choice == 0) {
        .fun <<- function(series, pp, ci.p, like.num) {
            x = eval(parse(text = series))
            object <<- gpdsfallPlot(x = x, pp = pp, ci.p = ci.p, 
                like.num = like.num)
            object }
        .objectMenu(
            params = list(
                series = "x",
                pp = 0.99,
                ci.p = 0.95,
                like.num = 50),
            infoName = "GPDS Fall Plot",
            tkoutput = FALSE)
    }
    
    
    if (choice == 0) {
        .fun <<- function(series, models, start, end, reverse, ci,
            lables) {
            x = eval(parse(text = series))
            object <<- gpdshapePlot(x = x, models = models, start = start, 
                end = end, reverse = reverse, ci = ci, autoscale = TRUE, 
                labels = labels) 
            object }
        .objectMenu(
            params = list(
                series = "x",
                models = 30,
                start = 15,
                end = 500,
                reverse = TRUE,
                ci = 0.95,
                labels = TRUE),
            infoName = "GPD Shape Plot",
            tkoutput = FALSE)
    }
    
    
    if (choice == 0) {
        .fun <<- function(fittedObject, extend, labels) {
            fit = eval(parse(text = fittedObject))
            par(mfrow = c(1, 1))
            object <<- gpdtailPlot(fit = fit, extend = extend, labels = labels)
            title(main = "GPD Tail Plot")
            object }
        .objectMenu(
            params = list(
                fittedObject = "fit",
                optlog = NA,
                extend = 1.5,
                labels = TRUE),
            infoName = "GPD Tail Plot",
            tkoutput = FALSE)
    }

    
}


# ******************************************************************************
# C3-GpdGlmFit


.fExtremes.C3Cmd.1 = function() .fExtremes.C3Cmd(1)


.fExtremes.C3Cmd <-
function(choice)
{
    # Description:
    #   Gpd Glm Fit
    
    # Menu:
    What = c(
        "NYI: GPD GLM Fit")
        
    # Choices:
    if (choice == 1) {
        tkinsert(txt, "end", "\n\nSorry, not yet implemented!\n")
    }
}


# ******************************************************************************
# C4-PotFit


.fExtremes.C4Cmd.1 = function() .fExtremes.C4Cmd(1)
.fExtremes.C4Cmd.2 = function() .fExtremes.C4Cmd(2)
.fExtremes.C4Cmd.3 = function() .fExtremes.C4Cmd(3)
.fExtremes.C4Cmd.4 = function() .fExtremes.C4Cmd(4)
.fExtremes.C4Cmd.5 = function() .fExtremes.C4Cmd(5)


.fExtremes.C4Cmd <-
function(choice)
{
    # Description:
    #   Pot Fit
    
    # Menu:
    What = c(
        "Example timeSeries: x = Danish Fire Losses",
        "POT Simulation",
        "POT Fit",
        "... Summary Report")
        
    # Choices:
    if (choice == 1) {
        # Example timeSeries: x = BMW Log Returns
        .dataSet("danish")
    }
    if (choice == 2) {
        # POT Simulation:
        .fun <<- function(start, plottype) {
            par(mfcol = c(1, 1))
            object }
        .objectMenu(
            params = list(
                start = 5, 
                plottype = "thresh"),
            infoName = "Extremal Index - Lower Tail",
            tkoutput = TRUE )
    }
    if (choice == 3) {
        # POT Fit:
        .fun <<- function(threshold, nextremes) 
            potFit(x = x, threshold = threshold, nextremes = nextremes,
                run = NA)
        par(mfcol = c(3, 3))
        .objectMenu(
            fun = ".fun", 
            params = list(threshold = NA, nextremes = NA),
            infoName = "POT Fit",
            tkoutput = TRUE )
    }
    if (choice == 4) {
        # Summary Report:
        par(mfrow = c(3, 2))
        .tkOutput(capture.output(summary(object)))
    }
    if (choice == 5) {
       NA
    }
}


# ******************************************************************************
# C5-PPFit


.fExtremes.C5Cmd.1 = function() .fExtremes.C5Cmd(1)


.fExtremes.C5Cmd <-
function(choice)
{
    # Description:
    #   PP Fit
    
    # Menu:
    
    # Choices:
    if (choice == 1) {
        tkinsert(txt, "end", "\n\nSorry, not yet implemented!\n")
    }
}


# ******************************************************************************
# C6-rlargFit


.fExtremes.C6Cmd.1 = function() .fExtremes.C6Cmd(1)


.fExtremes.C6Cmd <-
function(choice)
{
    # Description:
    #   rlarg Fit
    
    # Menu:
    
    # Choices:
    if (choice == 1) {
        tkinsert(txt, "end", "\n\nSorry, not yet implemented!\n")
    }
}


# ******************************************************************************
# D1-ExtremalIndexPlot

.fExtremes.D1Cmd.1 = function() .fExtremes.D1Cmd(1)
.fExtremes.D1Cmd.2 = function() .fExtremes.D1Cmd(2)
.fExtremes.D1Cmd.3 = function() .fExtremes.D1Cmd(3)
.fExtremes.D1Cmd.4 = function() .fExtremes.D1Cmd(4)
.fExtremes.D1Cmd.5 = function() .fExtremes.D1Cmd(5)


.fExtremes.D1Cmd <-
function(choice)
{
    # Description:
    #   Extremal Indexes Plot
    
    # Menu:
    What = c(
        "Example timeSeries: x = BMW log Returns",
        "Monthly Extremal Index - Lower Tail", 
        "N-Days Extremal Index - Lower Tail",
        "Monthly Extremal Index - Upper Tail", 
        "N-Days Extremal Index - Lower Tail")
    
   # Choices:
   if (choice == 1) {
        # Example timeSeries: x = BMW Log Returns
        .dataSet("bmw")
   }
   if (choice == 2) {
        # Extremal Indexes Plot - Lower Tail:
        .fun <<- function(series, start, plottype) {
            x = eval(parse(text = series))
            par(mfcol = c(1, 1))
            object <<- exindexPlot(x = -x, block = "month", 
                start = start, end = NA, plottype = plottype, 
                labels = TRUE, autoscale = TRUE)
            .tkTitle("Extremal Index - Lower Tail") 
            .tkOutput(capture.output(object))
            .tkdescription(date())
            object }
        .objectMenu(
            params = list(
                series = "x",
                start = 5, 
                plottype = "thresh"),
            infoName = "Extremal Index - Lower Tail",
            tkoutput = FALSE )
   }
   if (choice == 3) {
       # 20 Days Extremal Index - Lower Tail:
       .fun <<- function(series, block) {
            x = eval(parse(text = series))
            par(mfcol = c(1, 1)) 
            object <<- exindexesPlot(x = -x, block = block, 
                quantiles = seq(0.99, 0.999, 0.001), doplot = TRUE)
            .tkTitle("Extremal Index - Lower Tail")
            .tkOutput(capture.output(object))
            .tkdescription(date())
            object }
        .objectMenu(
            params = list(
                series = "x",
                block = 20),
            infoName = "Extremal Index - Lower Tail",
            tkoutput = FALSE )
   }
   if (choice == 4) {
       # Extremal Indexes Plot - Upper Tail:
        .fun <<- function(series, start, plottype) {
            x = eval(parse(text = series))
            par(mfcol = c(1, 1))
            object <<- exindexPlot(x = x, block = "month", start = start, 
                end = NA, plottype = plottype, labels = TRUE, autoscale = TRUE)
            .tkTitle("Extremal Index - Upper Tail")
            .tkOutput(capture.output(object))
            .tkdescription(date())
            object }
        .objectMenu(
            params = list(
                series = "x",
                start = 5, 
                plottype = "thresh" ),
            infoName = "Extremal Index - Upper Tail",
            tkoutput = TRUE )
   }
   if (choice == 5) {
       # 20 Days Extremal Index - Upper Tail:
       .fun <<- function(series, block) {
            x = eval(parse(text = series))
            par(mfcol = c(1, 1))
            object <<- exindexesPlot(x = x, block = block, 
                quantiles = seq(0.99, 0.999, 0.001), doplot = TRUE)
            .tkTitle("Extremal Index - Upper Tail")
            .tkOutput(capture.output(object))
            .tkdescription(date())
            object }
        .objectMenu(
            fun = ".fun", 
            params = list(
                series = "x",
                block = 20),
            infoName = "Extremal Index - Upper Tail",
            tkoutput = TRUE )
   }
}


################################################################################
    
