
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
# Extremes Plots


.fExtremes.ExtremesPlots.emd = 
function()
{   # A function implemented by Diethelm Wuertz

    # Empirical Distribution Function:
    helpTopic <<- ""
    myFunction = function(series, par, object2x, report) {
        x = tkEval(series)
        eval(parse(text = par))
        object <<- emdPlot(x, doplot = TRUE, plottype = "", labels = TRUE,
            pch = 19, col = "steelblue")
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            par = "par(mfrow=c(1,1))",
            object2x = FALSE,
            report = FALSE ),
        infoName = "Plot of Empirical Distribution" )   
}


# ------------------------------------------------------------------------------


.fExtremes.ExtremesPlots.qq = 
function()
{   # A function implemented by Diethelm Wuertz

    # Normal QQ-Plot with 95% Intervals:
    helpTopic <<- ""
    myFunction = function(series, cf, par, object2x, report) {
        x = tkEval(series)
        tkEval(par)
        if (cf) {
            object <<- qqbayesPlot(x = x, doplot = TRUE, labels = TRUE,
                pch = 19, col = "steelblue")
        } else {
            object <<- qqPlot(x = x, doplot = TRUE, labels = TRUE,
                pch = 19, col = "steelblue")
        }
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            cf = FALSE,
            par = "par(mfrow=c(1,1))",
            object2x = FALSE,
            report = FALSE ),
        infoName = "Normal QQ Plot" )       
}
               

# ------------------------------------------------------------------------------


.fExtremes.ExtremesPlots.q = 
function()
{   # A function implemented by Diethelm Wuertz

    # Exponential/Pareto quantile plot:
    helpTopic <<- ""
    myFunction = function(series, xi, par, object2x, report) {
        x = tkEval(series)
        eval(parse(text = par))
        object <<- qPlot(x = x, xi = xi, trim = NA, threshold = NA, 
            doplot = TRUE, labels = TRUE, pch = 19, col = "steelblue")
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            xi = 0,
            par = "par(mfrow=c(1,1))",
            object2x = FALSE,
            report = FALSE ),
        infoName = "Plot of Pareto Quantiles" )       
}


# ------------------------------------------------------------------------------


.fExtremes.ExtremesPlots.me = 
function()
{   # A function implemented by Diethelm Wuertz

    # Mean Excess Function Plot
    helpTopic <<- ""
    myFunction = function(series, par, object2x, report) {
        x = tkEval(series)
        eval(parse(text = par))
        object <<- mePlot(x = x, doplot = TRUE, labels = TRUE, pch = 19, 
            col = "steelblue")
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            par = "par(mfrow=c(1,1))",
            object2x = FALSE,
            report = FALSE ),
        infoName = "Mean Excess Function Plot" )       
}


# ------------------------------------------------------------------------------


.fExtremes.ExtremesPlots.mrl = 
function()
{   # A function implemented by Diethelm Wuertz

    # Mean Residual Life Plot:
    helpTopic <<- ""
    myFunction = function(series, conf, par, object2x, report) {
        x = tkEval(series)
        eval(parse(text = par))
        object <<- mrlPlot(x = x, conf = 0.95, umin = NA, umax = NA, 
            nint = 100, doplot = TRUE, plottype = c("autoscale", ""), 
            labels = TRUE, col = "steelblue")
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            conf = 0.95,
            par = "par(mfrow=c(1,1))",
            object2x = FALSE,
            report = FALSE ),
        infoName = "Mean Residual Life Plot" )       
}


# ------------------------------------------------------------------------------


.fExtremes.ExtremesPlots.mxf = 
function()
{   # A function implemented by Diethelm Wuertz

    # Mean Excess Function Plot:
    helpTopic <<- ""
    myFunction = function(series, tail, par, object2x, report) {
        x = tkEval(series)
        eval(parse(text = par))
        object <<- mxfPlot(x = x, tail = 0.05, doplot = TRUE, labels = TRUE,
            pch = 19, col = "steelblue")
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            tail = 0.05,
            par = "par(mfrow=c(1,1))",
            object2x = FALSE,
            report = FALSE ),
        infoName = "Plot of Mean Excesses" )       
}


# ------------------------------------------------------------------------------


.fExtremes.ExtremesPlots.msratio = 
function()
{   # A function implemented by Diethelm Wuertz

    # Plot of the ratio of maximum and sum:
    helpTopic <<- ""
    myFunction = function(series, p, par, object2x, report) {
        x = tkEval(series)
        eval(parse(text = par))
        object <<- msratioPlot(x, p = 1:4, doplot = TRUE,
            plottype = c("autoscale", ""), labels = TRUE)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            par = "par(mfrow=c(1,1))",
            object2x = FALSE,
            report = FALSE ),
        infoName = "Plot of Maximum/Sum Ratio" )       
}


# ------------------------------------------------------------------------------


.fExtremes.ExtremesPlots.records = 
function()
{   # A function implemented by Diethelm Wuertz

    # Records Plot:
    helpTopic <<- ""
    myFunction = function(series, subsamples, plottype, par, object2x, report) {
        x = tkEval(series)
        plottype = tkSplit(plottype)
        eval(parse(text = par))
        if (subsamples == 1) {
            object <<- recordsPlot(x, conf = 0.95, doplot = TRUE, 
                labels = TRUE, pch = 19, col = "steelblue")
        } else {
            object <<- ssrecordsPlot(x = x, subsamples = 10, doplot = TRUE, 
                plottype = plottype, labels = TRUE)
        }
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            subsamples = 1,
            plottype = "lin & log",
            par = "par(mfrow=c(1,1))",
            object2x = FALSE,
            report = FALSE ),
        infoName = "Plot of Subsample Records" )       
}



# ------------------------------------------------------------------------------


.fExtremes.ExtremesPlots.xacf = 
function()
{   # A function implemented by Diethelm Wuertz

    # ACF of exceedences over a threshold:
    helpTopic <<- ""
    myFunction = function(series, threshold, lag.max, doplot, par, 
        object2x, report) {
        x = tkEval(series)
        if (report) tkTitle("ACF of Exceedences Over a Threshold")
        if (doplot) tkEval(par)
        object <<- xacfPlot(x, threshold, lag.max, doplot)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            threshold = 0.95,
            lag.max = 15,
            doplot = TRUE,
            par = "par(mfrow=c(2,2))",
            object2x = FALSE,
            report = FALSE ),
        infoName = "Plot of ACF of Exceedences" )       
}


################################################################################
# Data Preprocessing


.fExtremes.DataPreprocessing.findThreshold = 
function()
{   # A function implemented by Diethelm Wuertz

    # Simulate GEV Series:
    helpTopic <<- ""
    myFunction = function(series, n, object2x, report) {
        x = tkEval(series)
        if (is.character(n)) n = eval(parse(text = n))
        ans = data.frame(cbind(length(as.vector(x)), n, findThreshold(x, n))) 
        rownames(ans) = "Value"
        colnames(ans) = c("Observations", "Exceedences", "Threshold")
        object <<- ans
        if (report) tkTitle("Threshold Value")
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            n = "floor(0.05*length(as.vector(x)))", 
            object2x = FALSE,
            report = TRUE ),
        infoName = "Threshold Value" )
}


################################################################################
# Generalized Extreme Value


.fExtremes.GEV.gevSlider = 
function()
{   # A function implemented by Diethelm Wuertz

    # GEV Distribution Slider:
    helpTopic <<- "dgev"
    .gevSlider()
}


# ------------------------------------------------------------------------------


.fExtremes.GEV.sim = 
function()
{   # A function implemented by Diethelm Wuertz

    # Simulate GEV Series:
    helpTopic <<- ""
    myFunction = function(n, shape, location, scale, object2x, report) {
        object <<- gevSim(model = list(shape = shape, location = location, 
            scale = scale), n = n)  
        if (report) tkTitle("Simulated GEV Series")
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            n = 100, 
            shape = 0.25, 
            location = 0, 
            scale = 1,
            object2x = TRUE,
            report = TRUE ),
        infoName = "Simulated GEV Series" )
}


# ------------------------------------------------------------------------------


.fExtremes.GEV.blockmaxSeries = 
function()
{   # A function implemented by Diethelm Wuertz

    # Simulate GEV Series:
    helpTopic <<- ""
    myFunction = function(series, block, object2x, report) {
        x = tkEval(series)
        block = tkSplit(block)
        print(block)
        object <<- blockmaxSeries(x, block = block)  
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x", 
            block = "monthly & quarterly",
            object2x = FALSE,
            report = TRUE ),
        infoName = "Block Maxima Series" )
}


# ------------------------------------------------------------------------------


.fExtremes.GEV.blockmaxVector = 
function()
{   # A function implemented by Diethelm Wuertz

    # Simulate GEV Series:
    helpTopic <<- ""
    myFunction = function(series, block, object2x, report) {
        x = tkEval(series)
        object <<- as.ts(blockmaxVector(x, block = block))
        attr(object, "control") = c(block = block)
        if (report) tkTitle("Block Maxima Vector")
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x", 
            block = 20,
            object2x = FALSE,
            report = TRUE ),
        infoName = "Block Maxima Vector" )
}



# ------------------------------------------------------------------------------


.fExtremes.GEV.fit = 
function()
{   # A function implemented by Diethelm Wuertz

    # Fit GEV Parameters:
    helpTopic <<- "gevFit"
    myFunction = function(series, type, gumbel, doplot, par, 
        object2x, report) { 
        x = tkEval(series)
        type = tkSplit(type)
        object <<- gevFit(x = x, type = type, gumbel = gumbel)
        if (doplot) {
            tkEval(par)
            object <<- summary(object)
        }
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            type = "mle & pwm", 
            gumbel = FALSE,
            doplot = TRUE,
            par = "par(mfrow=c(2,2))",
            object2x = FALSE,
            report = TRUE ),
        infoName = "GEV Parameter Fit" )
}


# ------------------------------------------------------------------------------


.fExtremes.GEV.gevrlevelPlot = 
function()
{   # A function implemented by Diethelm Wuertz

    # Return Level Plot:
    helpTopic <<- ""
    myFunction = function(k.blocks, object2x, report) {
        ans = eval(parse(text = object))
        object <<- gevrlevelPlot(object = ans)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            object = "object",
            k.blocks = 20, 
            object2x = TRUE,
            report = TRUE ),
        infoName = "Return Level Plot" ) 
}


# ------------------------------------------------------------------------------


.fExtremes.MDA.hillPlot = 
function()
{   # A function implemented by Diethelm Wuertz

    # Hill Plot:
    helpTopic <<- "hillPlot"
    myFunction = function(series, option, start, reverse, ci,
        object2x, report) {
        x = as.vector(tkEval(series))
        option = tkSplit(option)
        par(mfcol = c(1, 1), cex = 0.7)
        object <<- hillPlot(x = x, option = option, start = start, 
            end = NA, reverse = reverse, p = NA, ci = ci, autoscale = 
            TRUE, labels = TRUE)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            option = "alpha & xi", 
            start = 15, 
            reverse = FALSE, 
            ci = 0.95,
            object2x = FALSE,
            report = TRUE ),
        infoName = "Hill Plot" )
}


# ------------------------------------------------------------------------------


.fExtremes.MDA.shaparmPlot = 
function()
{   # A function implemented by Diethelm Wuertz

    # Shape Parameter Plots:
    helpTopic <<- "shaparmPlot"
    myFunction = function(series, revert, standardize, object2x, report) {
         x = tkEval(series)
         tkEval(par)
         xi.range = tkEval(xi.range)
         alpha.range = tkEval(alpha.range)
         object <<- shaparmPlot(x = x, revert, standardize, 
            tails = 0.01*(1:10), doplot = c(FALSE, FALSE, FALSE, 
                FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE), 
            which = c(TRUE, TRUE, TRUE), doprint = FALSE, both.tails = 
            TRUE, xi.range = xi.range, alpha.range = alpha.range)
         
         object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            revert = FALSE, 
            standardize = TRUE,
            par = "par(mfcol=c(3,2),cex=0.7)",
            xi.range = "c(-0.5, 1.5)", 
            alpha.range = "c(0, 10)",
            object2x = FALSE,
            report = TRUE ),
        infoName = "Shape Parameter Plots" )
}   



################################################################################
# Peaks over Threshold


.fExtremes.GPD.gpdSlider = 
function()
{   # A function implemented by Diethelm Wuertz

    # GEV Distribution Slider:
    helpTopic <<- "dgpd"
    .gpdSlider()
}


# ------------------------------------------------------------------------------


.fExtremes.GPD.sim = 
function()
{   # A function implemented by Diethelm Wuertz

    # Simulate GEV Series:
    helpTopic <<- ""
    myFunction = function(n, shape, location, scale, object2x, report) {
        object <<- as.ts(gpdSim(model = list(shape = shape, 
            location = location, scale = scale), n = n) )
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            n = 100, 
            shape = 0.25, 
            location = 0, 
            scale = 1,
            object2x = TRUE,
            report = TRUE ),
        infoName = "Simulated GPD Series" )
}


# ------------------------------------------------------------------------------


.fExtremes.GPD.fit = 
function()
{   # A function implemented by Diethelm Wuertz

    # Fit GEV Parameters:
    helpTopic <<- ""
    myFunction = function(series, threshold, nextremes, type, 
        object2x, report) { 
        x = tkEval(series)
        type = tkSplit(type)
        threshold = eval(parse(text = threshold))
        nextremes = eval(parse(text = nextremes))
        object <<- gpdFit(x = x, threshold = threshold, 
            nextremes = nextremes, type = type, 
            information = c("observed", "expected") )
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            threshold = "NA",
            nextremes = "max(10, length(x)/100)",
            type = "mle & pwm", 
            object2x = FALSE,
            report = TRUE ),
        infoName = "GPD Parameter Fit")
}


# ------------------------------------------------------------------------------

    
.fExtremes.GPD.q = 
function()
{   # A function implemented by Diethelm Wuertz

    # Quantile estimates and confidence intervals for high quantiles 
    # above the threshold 
    helpTopic <<- ""
    myFunction = function(pp, ci.type, ci.p, like.num, object2x, report) {
        ci.type = tkSplit(ci.type)
        object = gpdtailPlot(fittedObject)
        object <<- gpdqPlot(x = object, pp = pp, ci.type = ci.type, 
            ci.p = 0.95, like.num = 50)
        title(main = "GPD q-Plot")
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            pp = 0.99,
            ci.type = "likelihood & wald",
            ci.p = 0.95,
            like.num = 50,
            object2x = FALSE,
            report = FALSE),
        infoName = "GPD q-Plot" )
}


# ------------------------------------------------------------------------------


.fExtremes.GPD.quant = 
function()
{   # A function implemented by Diethelm Wuertz

    # A plot showing how the estimate of a high quantile in the tail 
    # of a dataset based on the GPD approximation varies with threshold 
    # or number of extremes:
    helpTopic <<- ""
    myFunction = function(series, p, models, start, end, reverse, ci,
        object2x, report) {
        x = tkEval(series)
        object <<- gpdquantPlot(as.vector(x), p = p, models = models, 
            start = start, end = end, reverse = reverse, ci = ci,
            autoscale = TRUE, labels = TRUE)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            p = 0.99,
            models = 30,
            start = 15,
            end = 500,
            reverse = TRUE,
            ci = 0.95,
            object2x = FALSE,
            report = FALSE),
        infoName = "Estimate of a High Quantile" )
}


# ------------------------------------------------------------------------------


.fExtremes.GPD.riskmeasures = 
function()
{   # A function implemented by Diethelm Wuertz

    # GPD Risk Measures:
    helpTopic <<- ""
    myFunction = function(plevels, object2x, report) {
        plevels = tkEval(plevels)
        object <<- gpdriskmeasures(x = fittedObject, plevels = plevels)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            plevels = "c(0.99,0.995,0.999,0.9995,0.9999)",
            object2x = FALSE,
            report = TRUE ),
        infoName = "GPD Risk Measures" )
}


# ------------------------------------------------------------------------------


.fExtremes.GPD.sfall = 
function()
{   # A function implemented by Diethelm Wuertz

    # Calculates expected shortfall estimates:
    helpTopic <<- ""
    myFunction = function(series, pp, ci.p, like.num, object2x, report) {
        x = tkEval(series)
        object <<- gpdsfallPlot(x = x, pp = pp, ci.p = ci.p, 
            like.num = like.num)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            pp = 0.99,
            ci.p = 0.95,
            like.num = 50,
            object2x = FALSE,
            report = TRUE ),
        infoName = "Expected Shortfall Estimates" )
}


# ------------------------------------------------------------------------------


.fExtremes.GPD.shape = 
function()
{   # A function implemented by Diethelm Wuertz

    helpTopic <<- ""
    myFunction = function(series, models, start, end, reverse, ci,
        lables, object2x, report) {
        x = tkEval(series)
        object <<- gpdshapePlot(x = x, models = models, start = start, 
            end = end, reverse = reverse, ci = ci, autoscale = TRUE, 
            labels = labels) 
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            models = 30,
            start = 15,
            end = 500,
            reverse = TRUE,
            ci = 0.95,
            labels = TRUE,
            object2x = FALSE,
            report = TRUE ),
        infoName = "GPD Shape Plot" )
}


# ------------------------------------------------------------------------------


.fExtremes.GPD.tail = 
function()
{   # A function implemented by Diethelm Wuertz

    helpTopic <<- ""
    myFunction = function(fittedObject, extend, labels, object2x, report) {
        fit = eval(parse(text = fittedObject))
        object <<- gpdtailPlot(fit = fit, extend = extend, labels = labels)
        title(main = "GPD Tail Plot")
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            fittedObject = "fit",
            optlog = NA,
            extend = 1.5,
            labels = TRUE,
            object2x = FALSE,
            report = TRUE ),
        infoName = "GPD Tail Plot" )
}


################################################################################

