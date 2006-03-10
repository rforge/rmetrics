
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
# fExtremes Commands


# ******************************************************************************
# Extremes Plots

      
.fExtremes.Plots.bmw = 
function()
{
    # * Example timeSeries: x = BMW Returns
    tkGetData(Data = "bmwDaily", infoName = "Daily BMW Returns")    
}


.fExtremes.Plots.emd = 
function()
{
    # Empirical Distribution Function:
    myFunction = function(series, object2x, report) {
        x = eval(parse(text = series))
        par(mfrow = c(1, 1))
        object <<- emdPlot(x, doplot = TRUE, plottype = "", labels = TRUE,
        	pch = 19, col = "steelblue")
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            object2x = FALSE,
            report = TRUE),
        infoName = "Plot of Empirical Distribution",
        tkoutput = FALSE,
        console = NULL,
        title = "Plot of Empirical Distribution",
        description = NULL )   
}


.fExtremes.Plots.qq = 
function()
{
    # Quantile Quantile Plot:
    myFunction = function(series, object2x, report) {
        x = eval(parse(text = series))
        par(mfrow = c(1, 1))
        object <<- qqPlot(x = x, doplot = TRUE, labels = TRUE,
        	pch = 19, col = "steelblue")
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            object2x = FALSE,
            report = TRUE),
        infoName = "Plot of Normal Quantiles",
        tkoutput = FALSE,
        console = NULL,
        title = "Plot of Normal Quantiles",
        description = NULL )       
}


.fExtremes.Plots.qqbayes = 
function()
{
    # Normal QQ-Plot with 95% Intervals:
    myFunction = function(series, object2x, report) {
        x = eval(parse(text = series))
        par(mfrow = c(1, 1))
        object <<- qqbayesPlot(x = x, doplot = TRUE, labels = TRUE,
        	pch = 19, col = "steelblue")
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            object2x = FALSE,
            report = TRUE),
        infoName = "Normal QQ Bayes Plot",
        tkoutput = FALSE,
        console = NULL,
        title = "Normal QQ Bayes Plot",
        description = NULL )       
}
               

.fExtremes.Plots.q = 
function()
{
    # Exponential/Pareto quantile plot:
    myFunction = function(series, xi, object2x, report) {
        x = eval(parse(text = series))
        par(mfrow = c(1, 1))
        object <<- qPlot(x = x, xi = xi, trim = NA, threshold = NA, 
            doplot = TRUE, labels = TRUE, pch = 19, col = "steelblue")
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            xi = 0,
            object2x = FALSE,
            report = TRUE),
        infoName = "Plot of Pareto Quantiles",
        tkoutput = FALSE,
        console = NULL,
        title = "Plot of Pareto Quantiles",
        description = NULL )       
}


.fExtremes.Plots.me = 
function()
{
    # Mean Excess Function Plot
    myFunction = function(series, object2x, report) {
        x = eval(parse(text = series))
        par(mfrow = c(1, 1))
        object <<- mePlot(x = x, doplot = TRUE, labels = TRUE, pch = 19, 
            col = "steelblue")
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            object2x = FALSE,
            report = TRUE),
        infoName = "Mean Excess Function Plot",
        tkoutput = FALSE,
        console = NULL,
        title = "Mean Excess Function Plot",
        description = NULL )       
}


.fExtremes.Plots.mrl = 
function()
{
    # Mean Residual Life Plot:
    myFunction = function(series, conf, object2x, report) {
        x = eval(parse(text = series))
        par(mfrow = c(1, 1))
        object <<- mrlPlot(x = x, conf = 0.95, umin = NA, umax = NA, 
            nint = 100, doplot = TRUE, plottype = c("autoscale", ""), 
            labels = TRUE, col = "steelblue")
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            conf = 0.95,
            object2x = FALSE,
            report = TRUE),
        infoName = "Mean Residual Life Plot",
        tkoutput = FALSE,
        console = NULL,
        title = "Mean Residual Life Plot",
        description = NULL )       
}


.fExtremes.Plots.mxf = 
function()
{
    # Mean Excess Function Plot:
    myFunction = function(series, tail, object2x, report) {
        x = eval(parse(text = series))
        par(mfrow = c(1, 1))
        object <<- mxfPlot(x = x, tail = 0.05, doplot = TRUE, labels = TRUE,
            pch = 19, col = "steelblue")
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            tail = 0.05,
            object2x = FALSE,
            report = TRUE),
        infoName = "Plot of Mean Excesses",
        tkoutput = FALSE,
        console = NULL,
        title = "Plot of Mean Excessws",
        description = NULL )       
}


.fExtremes.Plots.msratio = 
function()
{
    # Plot of the ratio of maximum and sum:
    myFunction = function(series, p, object2x, report) {
        x = eval(parse(text = series))
        par(mfrow = c(1, 1))
        object <<- msratioPlot(x, p = 1:4, doplot = TRUE,
            plottype = c("autoscale", ""), labels = TRUE)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            object2x = FALSE,
            report = TRUE),
        infoName = "Plot of Maximum/Sum Ratio",
        tkoutput = FALSE,
        console = NULL,
        title = "Plot of Maximum/Sum Ratio",
        description = NULL )       
}


.fExtremes.Plots.records = 
function()
{
    #  Record development compared with iid data:
    myFunction = function(series, conf, object2x, report) {
        x = eval(parse(text = series))
        par(mfrow = c(1, 1))
        object <<- recordsPlot(x, conf = 0.95, doplot = TRUE, 
            labels = TRUE, pch = 19, col = "steelblue")
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            conf = 0.95,
            object2x = FALSE,
            report = TRUE),
        infoName = "Plot of Records Development",
        tkoutput = FALSE,
        console = NULL,
        title = "Plot of Records Development",
        description = NULL )       
}


.fExtremes.Plots.ssrecords = 
function()
{
    # another records plot, investigates subsamples
    myFunction = function(series, subsamples, plottype, object2x, report) {
        x = eval(parse(text = series))
        plottype = eval(parse(text = plottype))
        par(mfrow = c(1, 1))
        object <<- ssrecordsPlot(x = x, subsamples = 10, doplot = TRUE, 
            plottype = plottype, labels = TRUE)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            subsamples = 10,
            plottype = "c('lin', 'log')",
            object2x = FALSE,
            report = TRUE),
        infoName = "Plot of Subsample Records",
        tkoutput = FALSE,
        console = NULL,
        title = "Plot of Subsample Records",
        description = NULL )       
}



.fExtremes.Plots.xacf = 
function()
{
    # ACF of exceedences over a threshold:
    myFunction = function(series, object2x, report) {
        x = eval(parse(text = series))
        par(mfrow = c(1, 1))
        object <<- xacfPlot()
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            object2x = FALSE,
            report = TRUE),
        infoName = "Plot of ACF of Exceedences",
        tkoutput = FALSE,
        console = NULL,
        title = "Plot of ACF of Exceedences",
        description = NULL )       
}



# ******************************************************************************
# Generalized Extreme Value


.fExtremes.GEV.RandSlider = 
function()
{
    # GEV Random Number Slider"
    dummy = .gevSlider(GenerateRandomNumbers = TRUE)
}


.fExtremes.GEV.DistSlider = 
function()
{
    # GEV Distribution Slider:
    dummy = .gevSlider()
}


.fExtremes.GEV.bmw = 
function(choice)
{
    # BMW
    tkGetData(Data = "bmwDaily", infoName = "Daily BMW Returns")
    # .dataSet(bmwmax)
}


.fExtremes.GEV.sim = 
function()
{
    # Simulate GEV Series:
    myFunction = function(n, shape, location, scale, object2x, report) {
        object <<- gevSim(model = list(shape = shape, location = location, 
            scale = scale), n = n)  
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            n = 1000, 
            shape = 0.25, 
            location = 0, 
            scale = 1,
            object2x = TRUE,
            report = TRUE),
        infoName = "Simulated GEV Series",
        tkoutput = FALSE,
        console = "print(head(object))",
        title = "Simulated GEV Series",
        description = NULL )
}


.fExtremes.GEV.fit = 
function()
{
    # Fit GEV Parameters:
    myFunction <<- function(series, type, gumbel, object2x, report) { 
        x = eval(parse(text = series))
        object <<- gevFit(x = x, type = type, gumbel = gumbel)
        fittedObject <<- tkSaveAs(
            data = object, 
            infoName = "GEV Fit", 
            console = NULL, 
            what = "fitted", 
            tkoutput = FALSE)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            type = "mle", 
            gumbel = FALSE,
            object2x = FALSE,
            report = TRUE),
        infoName = "GEV Parameter Fit",
        tkoutput = FALSE,
        console = NULL,
        title = "GEV Parameter Fit",
        description = NULL )
}


.fExtremes.GEV.summary = 
function()
{
    # Summary Report:
    tkSummary(fitted@bject)
}


.fExtremes.GEV.rlevel = 
function()
{
    # Return Level Plot:
    myFunction = function(k.blocks, object2x, report) {
        object <<- gevrlevelPlot(object = fittedObject)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            k.blocks = 20, 
            object2x = FALSE,
            report = TRUE),
        infoName = "Return Level Plot",
        tkoutput = FALSE,
        console = NULL,
        title = "Return Level Plot",
        description = NULL ) 
}


.fExtremes.MDA.hill = 
function()
{
    # Hill Plot:
    myFunction = function(series, option, start, end, reverse, p, ci,
        object2x, report) {
        x = eval(parse(text = series))
        option = eval(parse(text = option))
        par(mfcol = c(1, 1), cex = 0.7)
        object <<- hillPlot(x = x, option, start, end, reverse, p, ci, 
            autoscale = TRUE, labels = TRUE)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            option = "c('alpha', 'xi')", 
            start = 15, 
            end = NA, 
            reverse = FALSE, 
            p = NA, 
            ci = 0.95,
            object2x = FALSE,
            report = TRUE),
        infoName = "Hill Plot",
        tkoutput = FALSE,
        console = NULL,
        title = "Hill Plot",
        description = NULL )
}


.fExtremes.MDA.shaparm = 
function()
{
    # Shape Parameter Plots:
    myFunction <<- function(series, revert, standardize, object2x, report) {
         x = eval(parse(text = series))
         par(mfcol = c(3, 2), cex = 0.7)
         object <<- shaparmPlot(x = x, revert, standardize, 
            tails = 0.01*(1:10), doplot = c(FALSE, FALSE, FALSE, 
                FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE), 
            which = c(TRUE, TRUE, TRUE), doprint = FALSE, both.tails = TRUE, 
            xi.range = c(-0.5, 1.5), alpha.range = c(0, 10))
         object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            revert = FALSE, 
            standardize = FALSE,
            object2x = FALSE,
            report = TRUE),
        infoName = "Shape Parameter Plots",
        tkoutput = FALSE,
        console = NULL,
        title = "Shape Parameter Plots",
        description = NULL )
}   



# ******************************************************************************
# Peaks over Threshold


.fExtremes.GPD.RandSlider = 
function()
{
    # GEV Random Number Slider"
    dummy = .gpdSlider(GenerateRandomNumbers = TRUE)
}


.fExtremes.GPD.DistSlider = 
function()
{
    # GEV Distribution Slider:
    dummy = .gpdSlider()
}


.fExtremes.GPD.bmw = 
function(choice)
{
    # BMW
    tkGetData(Data = "bmwDaily", infoName = "Daily BMW Returns")
    # .dataSet(bmwmax)
}


.fExtremes.GPD.sim = 
function()
{
    # Simulate GEV Series:
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
            report = TRUE),
        infoName = "Simulated GPD Series",
        tkoutput = FALSE,
        console = "print(head(object))",
        title = "Simulated GPD Series",
        description = NULL )
}


.fExtremes.GPD.fit = 
function()
{
    # Fit GEV Parameters:
    myFunction = function(series, threshold, nextremes, type, 
        object2x, report) { 
        x = eval(parse(text = series))
        type = eval(parse(text = type))
        threshold = eval(parse(text = threshold))
        nextremes = eval(parse(text = nextremes))
        object <<- gpdFit(x = x, threshold = threshold, 
            nextremes = nextremes, type = type, 
            information = c("observed", "expected") )
        # Save Fit:
        fittedObject <<- tkSaveAs(
            data = object, 
            infoName = "GPD Fit", 
            console = NULL, 
            what = "fitted", 
            tkoutput = FALSE)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            threshold = "NA",
            nextremes = "max(10, length(x)/100)",
            type = "c('mle', 'pwm')", 
            object2x = FALSE,
            report = TRUE),
        infoName = "GPD Parameter Fit",
        tkoutput = FALSE,
        console = NULL,
        title = "GPD Parameter Fit",
        description = NULL )
}


.fExtremes.GPD.summary = 
function()
{
    # Summary Report:
    tkSummary(fitted@bject)
}

    
.fExtremes.GPD.q = 
function()
{
    # Quantile estimates and confidence intervals for high quantiles 
    # above the threshold 
    myFunction = function(pp, ci.type, ci.p, like.num, object2x, report) {
        ci.type = eval(parse(text = ci.type))
        object = gpdtailPlot(fittedObject)
        object <<- gpdqPlot(x = object, pp = pp, ci.type = ci.type, 
            ci.p = 0.95, like.num = 50)
        title(main = "GPD q-Plot")
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            pp = 0.99,
            ci.type = "c('likelihood', 'wald')",
            ci.p = 0.95,
            like.num = 50,
            object2x = FALSE,
            report = FALSE),
        infoName = "GPD q-Plot",
        tkoutput = FALSE,
        console = NULL,
        title = "GPD q-Plot",
        description = NULL )
}


.fExtremes.GPD.quant = 
function()
{
    # A plot showing how the estimate of a high quantile in the tail 
    # of a dataset based on the GPD approximation varies with threshold 
    # or number of extremes:
    myFunction = function(series, p, models, start, end, reverse, ci,
        object2x, report) {
        x = eval(parse(text = series))
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
        infoName = "Estimate of a High Quantile",
        tkoutput = FALSE,
        console = NULL,
        title = "Estimate of a High Quantile",
        description = NULL )
}


.fExtremes.GPD.riskmeasures = 
function()
{
    # GPD Risk Measures:
    myFunction = function(plevels, object2x, report) {
        plevels = eval(parse(text = plevels))
        object <<- gpdriskmeasures(x = fittedObject, plevels = plevels)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            plevels = "c(0.99, 0.995, 0.999, 0.9995, 0.9999)",
            object2x = FALSE,
            report = TRUE),
        infoName = "GPD Risk Measures",
        tkoutput = FALSE,
        console = NULL,
        title = "GPD Risk Measures",
        description = NULL )
}


.fExtremes.GPD.sfall = 
function()
{
    # Calculates expected shortfall estimates:
    myFunction = function(series, pp, ci.p, like.num, object2x, report) {
        x = eval(parse(text = series))
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
            report = TRUE),
        infoName = "Expected Shortfall Estimates",
        tkoutput = FALSE,
        console = NULL,
        title = "Expected Shortfall Estimates",
        description = NULL )
}


.fExtremes.GPD.shape = 
function()
{
    myFunction = function(series, models, start, end, reverse, ci,
        lables) {
        x = eval(parse(text = series))
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
            labels = TRUE),
        infoName = "GPD Shape Plot",
        tkoutput = FALSE,
        console = NULL,
        title = "GPD Shape Plot",
        description = NULL )
}


.fExtremes.GPD.tail = 
function()
{
    myFunction = function(fittedObject, extend, labels) {
        fit = eval(parse(text = fittedObject))
        par(mfrow = c(1, 1))
        object <<- gpdtailPlot(fit = fit, extend = extend, labels = labels)
        title(main = "GPD Tail Plot")
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            fittedObject = "fit",
            optlog = NA,
            extend = 1.5,
            labels = TRUE),
        infoName = "GPD Tail Plot",
        tkoutput = FALSE,
        console = NULL,
        title = "GPD Tail Plot",
        description = NULL )
}


################################################################################