
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
# Web Data Import


.fBasics.Import.economagicImport = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Economagic Series Download:
    helpTopic <<- "economagicImport"
    myFunction = function(value, file, frequency, save, colname, try,
        object2x, report) {
        object <<- economagicImport(query = value, file = file, 
            frequency = frequency, save = save, colname = colname,
            try = try)
        if (object2x) object <<- as.timeSeries(object@data)
        object }   
    tkExecute(
        fun = myFunction,
        prototypes = list(
            value = "fedny/day-fxus2eu", 
            file = "tempfile",
            source = "http://www.economagic.com/em-cgi/data.exe/",
            frequency = "daily", 
            save = FALSE,
            colname = "USDEUR",
            try = TRUE,
            object2x = TRUE,
            report = TRUE),
        subject = "Economagic Download" ) 
}


# ------------------------------------------------------------------------------


.fBasics.Import.yahooImport = 
function() 
{   # A function implemented by Diethelm Wuertz

    # Yahoo Series Download:
    helpTopic <<- "yahooImport"
    myFunction = function(value, from, to, file, source, save, sep, 
        swap, try, object2x, report) {
        if (to == "today") to = as.character(Sys.Date())
        from.y = as.integer(substr(from, 1, 4))
        from.m = as.integer(substr(from, 6, 7)) - 1
        from.d = as.integer(substr(from, 9, 10))
        to.y = as.integer(substr(to, 1, 4))
        to.m = as.integer(substr(to, 6, 7)) - 1
        to.d = as.integer(substr(to, 9, 10))
        query = paste("s=", value, "&a=", from.m, "&b=", from.d, "&c=", 
             from.y, "&d=", to.m, "&q=", to.d, "&f=", to.y, "&z=", value, 
             "&x=.csv", sep = "")
        object <<- yahooImport(query = query, file = file, source = source, 
            save = save, sep = sep, swap = swap, try = try)
        if (object2x) object <<- as.timeSeries(object@data)
        object }   
    tkExecute(
        fun = myFunction,
        prototypes = list(
            value = "IBM",
            from = "2000-01-01",
            to = "today",
            file = "tempfile",
            source = "http://chart.yahoo.com/table.csv?",
            save = FALSE,
            sep = ";",
            swap = 20,
            try = TRUE,
            object2x = TRUE,
            report = TRUE),
        subject = "Yahoo Download" ) 
}


# ------------------------------------------------------------------------------


.fBasics.Import.fredImport = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # FRED St. Louis Series Download:
    helpTopic <<- "fredImport"
    myFunction = function(value, file, source, frequency, save, sep, 
        try, object2x, report) {
        object <<- fredImport(query = value, file = file, source = source, 
            frequency = frequency, save = save, sep = sep, try = try)
        if (object2x) object <<- as.timeSeries(object@data)
        object }   
    tkExecute(
        fun = myFunction,
        prototypes = list(
            value = "DPRIME", 
            file = "tempfile",
            source = "http://research.stlouisfed.org/fred2/series/", 
            frequency = "daily", 
            save = FALSE, 
            sep = ";", 
            try = TRUE, 
            object2x = TRUE,
            report = TRUE),
        subject = "FRED Download" ) 
}


################################################################################
# Time Series Plots


.fBasics.PlotFunctions.plot = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # 1D: Time Series Plot:
    helpTopic <<- "plot"
    myFunction = function(series, type, col, xlab, ylab, par, grid) {
        x = tkEval(series)
        tkEval(par)
        plot(x, type = type, col = col, xlab = xlab, ylab = ylab)
        if (grid) grid()}
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            type = "l",
            col = "steelblue",
            xlab = "Index",
            ylab = "Series",
            par = "par(mfrow=c(1,1))",
            grid = FALSE ),
        subject = "Series Plot")       
}


# ------------------------------------------------------------------------------


.fBasics.PlotFunctions.acfPlot = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # 1D: ACF Plot:
    helpTopic <<- "acf"
    myFunction = function(series, par, grid) {
        x = as.vector(tkEval(series))
        tkEval(par)
        acfPlot(x)
        if (grid) grid() }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            par = "par(mfrow=c(1,1))",
            grid = FALSE ),
        subject = "ACF Plot" )            
}


# ------------------------------------------------------------------------------


.fBasics.PlotFunctions.pacfPlot = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # 1D: PACF Plot:
    helpTopic <<- "acf"
    myFunction = function(series, par, grid) {
        x = as.vector(tkEval(series))
        tkEval(par)
        pacfPlot(x)
        if (grid) grid() }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            par = "par(mfrow=c(1,1))",
            grid = TRUE),
        subject = "PACF Plot" )            
}


# ------------------------------------------------------------------------------


.fBasics.PlotFunctions.histPlot = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # 1D: Histogram Plot:
    helpTopic <<- "hist"
    myFunction = function(series, par) {
        x = tkEval(series)
        tkEval(par)
        histPlot(x)
        }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            par = "par(mfrow=c(1,1))" ),
        subject = "Histogram Plot" )       
}


# ------------------------------------------------------------------------------


.fBasics.PlotFunctions.bivariatePlot = 
function() 
{   # A function implemented by Diethelm Wuertz
  
    # 2D: Series Plot:
    helpTopic <<- "plot"
    myFunction = function(series, par) {
        x = tkEval(series)
        tkEval(par)
        plot(x, xlab = "", ylab = "")
        }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            par = "par(mfrow=c(1,1))" ),
        subject = "2D Series Plot" )       
}


# ------------------------------------------------------------------------------


.fBasics.PlotFunctions.scatterPlot = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # 2D: Scatter Diagramm Plot:
    helpTopic <<- "plot"
    myFunction = function(series, par, grid) {
        x = tkEval(series)
        tkEval(par)
        plot(as.vector(x[, 1]), as.vector(x[,2]),
            xlab = x@units[1], ylab = x@units[2], 
            pch = 19, col  = "steelblue")
        if (grid) grid()
        }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            par = "par(mfrow=c(1,1))",
            grid = TRUE ),
        subject = "Scatter Diagram Plot" )         
}


################################################################################
# Stylized Facts


.fBasics.StylizedFacts.teffectPlot =  
function() 
{   # A function implemented by Diethelm Wuertz

    # Taylor Effect:
    helpTopic <<- "teffectPlot"
    myFunction = function(series, deltas, lag.max, standardize, 
        doplot, par, grid, object2x, report) {
        x = tkEval(series)
        deltas = eval(parse(text = deltas))
        if (doplot) tkEval(par)
        object <<- teffectPlot(as.vector(x), deltas = deltas, 
            lag.max = lag.max, standardize = standardize) 
        if (grid) grid()
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            deltas = "seq(0.2,3,by=0.05)",
            lag.max = 20,
            standardize = TRUE,
            doplot = TRUE,
            par = "par(mfrow=c(1,1))",
            grid = FALSE,
            object2x = FALSE,
            report = FALSE ),
        subject = "Taylor Effect Plot" ) 
}


# ------------------------------------------------------------------------------


.fBasics.StylizedFacts.lmacfPlot =  
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Long Memory Behaviour:
    helpTopic <<- "lmacfPlot"
    myFunction = function(series, lag.max, ci, doplot, par, object2x, report) {
        x = tkEval(series)
        lag.max = eval(parse(text = lag.max))
        if (doplot) tkEval(par)
        object <<- lmacfPlot(abs(as.vector(x))) 
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            lag.max = "max(2, floor(10*log10(length(x))))",
            ci = 0.95,
            doplot = TRUE,
            par = "par(mfrow=c(2,1))",
            object2x = FALSE,
            report = FALSE ),
        subject = "Long Memory Plot" ) 
}


# ------------------------------------------------------------------------------


.fBasics.StylizedFacts.logpdfPlot =  
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Log PDF Plot:
    helpTopic <<- "logpdfPlot"
    myFunction = function(series, doplot, par, grid, object2x, report) {
        x = tkEval(series)
        if (doplot) tkEval(par)
        object <<- logpdfPlot(abs(as.vector(x))) 
        if (grid) grid()
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            doplot = TRUE,
            par = "par(mfrow=c(1,1))",
            grid = TRUE,
            object2x = FALSE,
            report = FALSE ),
        subject = "Log PDF Plot" ) 
}


# ------------------------------------------------------------------------------


.fBasics.StylizedFacts.qqgaussPlot =  
function() 
{   # A function implemented by Diethelm Wuertz

    # Normal QQ Plot:
    helpTopic <<- "qqgaussPlot"
    myFunction = function(series, span, doplot, par, object2x, report) {
        x = tkEval(series)
        if (doplot) tkEval(par)
        object <<- qqgaussPlot(as.vector(x), span = span) 
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            span = 5,
            doplot = TRUE,
            par = "par(mfrow=c(1,1))",
            object2x = FALSE,
            report = FALSE ),
        subject = "Normal QQ Plot" ) 
}


# ------------------------------------------------------------------------------


.fBasics.StylizedFacts.scalinglawPlot =  
function() 
{   # A function implemented by Diethelm Wuertz
 
    # Scaling Law Plot:
    helpTopic <<- "scalinglawPlot"
    myFunction = function(series, span, doplot, par, object2x, report) {
        x = tkEval(series)
        if (span == "NULL") {
            span = ceiling(log(length(as.vector(x))/252)/log(2))
        } else {
            span = as.numeric(span)
        }
        if (doplot) tkEval(par)
        object <<- scalinglawPlot(x, span = span) 
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            span = "NULL",
            doplot = TRUE,
            par = "par(mfrow=c(1,1))",
            object2x = FALSE,
            report = FALSE ),
        subject = "Scaling Law Plot" ) 
}


################################################################################
# Time Series Statistics


.fBasics.BasicStatistics.mean = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Mean:
    helpTopic <<- "mean"
    myFunction = function(series, object2x, report) {
        x = tkEval(series)
        object <<- c(mean = mean(as.vector(x)))
        if (report) tkTitle("Mean")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            object2x = FALSE,
            report = TRUE),
        subject = "Mean" )  
}


# ------------------------------------------------------------------------------


.fBasics.BasicStatistics.var = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Variance:
    helpTopic <<- "var"
    myFunction = function(series, object2x, report) {
        x = tkEval(series)
        object <<- c(var = var(as.vector(x)))
        if (report) tkTitle("Variance")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            object2x = FALSE,
            report = TRUE),
        subject = "Variance" )
}


# ------------------------------------------------------------------------------


.fBasics.BasicStatistics.skewness = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Skewness:
    helpTopic <<- "skewness"
    myFunction = function(series, na.rm, method, object2x, report) {
        x = tkEval(series)
        method = tkSplit(method)
        object <<- c(skewness = skewness(as.vector(x), 
            na.rm = na.rm, method = method))
        attr(object, "method") <<- method
        if (report) tkTitle("Skewness")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            na.rm = FALSE,
            method = "moment & fisher",
            object2x = FALSE,
            report = TRUE),
        subject = "Skewness" )
}


# ------------------------------------------------------------------------------


.fBasics.BasicStatistics.kurtosis = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Kurtosis:
    helpTopic <<- "kurtosis"
    myFunction = function(series, na.rm, method, object2x, report) {
        x = tkEval(series)
        method = tkSplit(method)
        object <<- c(kurtosis = kurtosis(as.vector(x), 
            na.rm = na.rm, method = method))
        attr(object, "method") <<- method
        if (report) tkTitle("Kurtosis")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            na.rm = FALSE,
            method = "excess & moment & fisher",
            object2x = FALSE,
            report = TRUE),
        subject = "Kurtosis" )
}


# ------------------------------------------------------------------------------


.fBasics.BasicStatistics.summary = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Summary:
    helpTopic <<- "summary"
    myFunction = function(series, object2x, report) {
        x = tkEval(series)
        object <<- summary(as.vector(x))
        if (report) tkTitle("Summary")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            object2x = FALSE,
            report = TRUE),
        subject = "Summary Statistics")   
}


# ------------------------------------------------------------------------------


.fBasics.BasicStatistics.basicStats = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Basic Statistics:
    helpTopic <<- "basicStats"
    myFunction = function(series, ci, column, object2x, report) {
        x = tkEval(series)
        object <<- basicStats(as.vector(x), ci, column) 
        if (report) tkTitle("Basic Statistis")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            ci = 0.95,
            column = 1,
            object2x = FALSE,
            report = TRUE),
        subject = "Basic Statistics" )     
}


################################################################################
# Return Distributions


.fBasics.ReturnDistributions.rnorm = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Normal RVs:
    helpTopic <<- "dnorm"
    myFunction = function(n, mean, sd, as.ts, object2x, report) {
        object <<- rnorm(n, mean, sd)
        if (as.ts) object <<- as.ts(object)
        attr(object, "control") <<- 
            c(dist = "norm", mean = as.character(mean), sd = as.character(sd))
        if (report) tkTitle("Normal RVs")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            n = 100,
            mean = 0,
            sd = 1,
            as.ts = TRUE,
            object2x = TRUE,
            report = TRUE),
        subject = "Normal RVs" )  
}


# ------------------------------------------------------------------------------


.fBasics.ReturnDistributions.rhyp = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Hyperbolic RVs:
    helpTopic <<- "dhyp"
    myFunction = function(n, alpha, beta, delta, mu, parameterization, 
        object2x, report) {
        object <<- rhyp(n = n, alpha = alpha, beta = beta, delta = delta,
            pm = parameterization)
        if (as.ts) object <<- as.ts(object)
        if (report) tkTitle("Hyperbolic RVs")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            n = 100,
            alpha = 1,
            beta = 0,
            delta = 1,
            mu = 0,
            parameterization = 1,
            as.ts = TRUE,
            object2x = TRUE,
            report = TRUE),
        subject = "Hyperbolic RVs" )  
}


# ------------------------------------------------------------------------------


.fBasics.ReturnDistributions.rnig = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Normal Inverse Gaussian RVs:
    helpTopic <<- "dnig"
    myFunction = function(n, alpha, beta, delta, mu, as.ts, 
        object2x, report) {
        object <<- rnig(n = n, alpha = alpha, beta = beta, delta = delta,
            mu = mu)
        if (as.ts) object <<- as.ts(object)
        if (report) tkTitle("Normal Inverse Gaussian RVs")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            n = 100,
            alpha = 1,
            beta = 0,
            delta = 1,
            mu = 0,
            as.ts = TRUE,
            object2x = TRUE,
            report = TRUE),
        subject = "Normal Inverse Gaussian RVs" )  
}


# ------------------------------------------------------------------------------


.fBasics.ReturnDistributions.rsymstb = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Symmetric Stable RVs:
    helpTopic <<- "dsymstb"
    myFunction = function(n, alpha, as.ts, object2x, report) {
        object <<- rsymstb(n = n, alpha = alpha)
        if (as.ts) object <<- as.ts(object)
        if (report) tkTitle("Symmetric Stable RVs")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            n = 100,
            alpha = 1.8,
            as.ts = TRUE,
            object2x = TRUE,
            report = TRUE),
        subject = "Symmetric Stable RVs" )  
}


# ------------------------------------------------------------------------------


.fBasics.ReturnDistributions.rstable = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Stable RVs:
    helpTopic <<- "dstable"
    myFunction = function(n, alpha, beta, gamma, delta, parameterization, 
        as.ts, object2x, report) {
        object <<- rstable(n = n, alpha = alpha, beta = beta, 
            gamma = gamma, delta = delta, pm = parameterization)
        if (as.ts) object <<- as.ts(object)
        if (report) tkTitle("Stable RVs")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            n = 100,
            alpha = 1,
            beta = 0,
            gamma = 1,
            delta = 0,
            mu = 0,
            parameterization = 0,
            as.ts = TRUE,           
            object2x = TRUE,
            report = TRUE),
        subject = "Stable RVs" )  
}


# ------------------------------------------------------------------------------


.fBasics.ReturnDistributions.distSlider = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # RVS Sliders:
    helpTopic <<- "dhyp"
    myFunction = function(dist, object2x, report) {
        dist = tkSplit(dist)
        fun = match.fun(paste(dist, "Slider", sep = ""))
        object <<- fun() 
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            dist = "hyp & nig & symstb & stable",
            object2x = FALSE,
            report = FALSE ),
        subject = "RVs Slider" ) 
}


# ------------------------------------------------------------------------------


.fBasics.ReturnDistributions.distFit = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # RVS Sliders:
    helpTopic <<- ""
    myFunction = function(series, dist, doplot, par, object2x, report) {
        x = tkEval(series)
        dist = tkSplit(dist)
        fun = match.fun(paste(dist, "Fit", sep = ""))
        object <<- fun(x, doplot = doplot) 
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            dist = "hyp & nig",
            doplot = TRUE,
            par = "par(mfrow=c(1,1))",
            object2x = FALSE,
            report = FALSE ),
        subject = "RVs Fit" ) 
}


################################################################################
# Normality Tests


.fBasics.NormalityTests.ksnormTest = 
function() 
{   # A function implemented by Diethelm Wuertz

    # One-sample Kolmogorov-Smirnov Test:
    helpTopic <<- "ksnormTest"
    myFunction = function(series, object2x, report) {
        x = tkEval(series)
        object <<- ksnormTest(x)
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            object2x = FALSE,
            report = TRUE ),
        subject = "One-sample KS Test" ) 
}


# ------------------------------------------------------------------------------


.fBasics.NormalityTests.shapiroTest = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Shapiro - Wilk Test:
    helpTopic <<- "shapiroTest"
    myFunction = function(series, object2x, report) {
        x = tkEval(series)
        object <<- shapiroTest(x)
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            object2x = FALSE,
            report = TRUE ),
        subject = "Shapiro-Wilk Test" ) 
}


# ------------------------------------------------------------------------------


.fBasics.NormalityTests.jarqueberaTest = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Jarque-Bera Test:
    helpTopic <<- "jarqueberaTest"
    myFunction = function(series, object2x, report) {
        x = tkEval(series)
        object <<- jarqueberaTest(x)
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            object2x = FALSE,
            report = TRUE ),
        subject = "Jarque-Bera Test" )    
}


# ------------------------------------------------------------------------------


.fBasics.NormalityTests.dagoTest = 
function() 
{   # A function implemented by Diethelm Wuertz

    # d'Agostino Test:
    helpTopic <<- "dagoTest"
    myFunction = function(series, object2x, report) {
        x = tkEval(series)
        object <<- dagoTest(x)
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            object2x = FALSE,
            report = TRUE ),
        subject = "d'Agostino Test" ) 
}


# ------------------------------------------------------------------------------


.fBasics.NormalityTests.adTest = 
function() 
{   # A function implemented by Diethelm Wuertz

    # Anderson - Darling Normality Test:
    helpTopic <<- "adTest"
    myFunction = function(series, object2x, report) {
        x = tkEval(series)
        object <<- adTest(x)
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            object2x = FALSE,
            report = TRUE ),
        subject = "Anderson-Darling Test" )
}


# ------------------------------------------------------------------------------


.fBasics.NormalityTests.cvmTest = 
function() 
{   # A function implemented by Diethelm Wuertz

    # Cramer - von Mises Normality Test:
    helpTopic <<- "cvmTest"
    myFunction = function(series, object2x, report) {
        x = tkEval(series)
        object <<- cvmTest(x)
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            object2x = FALSE,
            report = TRUE ),
        subject = "Cramer-vonMises Test" )
}


# ------------------------------------------------------------------------------

        
.fBasics.NormalityTests.lillieTest = 
function() 
{   # A function implemented by Diethelm Wuertz

    # Lilliefors (KS) Normality Test:
    helpTopic <<- "lillieTest"
    myFunction = function(series, object2x, report) {
        x = tkEval(series)
        object <<- lillieTest(x)
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            object2x = FALSE,
            report = TRUE ),
        subject = "Lilliefors Test" )
}


# ------------------------------------------------------------------------------


.fBasics.NormalityTests.pchiTest = 
function() 
{   # A function implemented by Diethelm Wuertz

    # Pearson Chi-Square Normality Test:
    helpTopic <<- "pchiTest"
    myFunction = function(series, object2x, report) {
        x = tkEval(series)
        object <<- pchiTest(x)
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            object2x = FALSE,
            report = TRUE ),
        subject = "Pearson Test" )
}


# ------------------------------------------------------------------------------


.fBasics.NormalityTests.sfTest = 
function() 
{   # A function implemented by Diethelm Wuertz

    # Shapiro - Francia Normality Test:
    helpTopic <<- "sfTest"
    myFunction = function(series, object2x, report) {
        x = tkEval(series)
        object <<- sfTest(x)
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            object2x = FALSE,
            report = TRUE ),
        subject = "Shapiro-Francia Test" )
}


################################################################################
# Bivariate Tests


.fBasics.BivariateTests.ks2Test  = 
function() 
{   # A function implemented by Diethelm Wuertz

    # Kolmogorov-Smirnov Test:
    helpTopic <<- "ks2Test"
    myFunction = function(series, object2x, report) {
        x = tkEval(series)
        object <<- ks2Test(x[,1], x[,2])
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            object2x = FALSE,
            report = TRUE ),
        subject = "Kolmogorov-Smirnov" )
}


# ------------------------------------------------------------------------------

        
.fBasics.BivariateTests.tTest  = 
function() 
{   # A function implemented by Diethelm Wuertz

    # Location: Unpaired t-Test:
    helpTopic <<- "tTest"
    myFunction = function(series, object2x, report) {
        x = tkEval(series)
        object <<- tTest(x[,1], x[,2])
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            object2x = FALSE,
            report = TRUE ),
        subject = "Location: Unpaired t" )
}


# ------------------------------------------------------------------------------


.fBasics.BivariateTests.kw2Test  = 
function() 
{   # A function implemented by Diethelm Wuertz

    # Location: Kruskal-Wallis Test:
    helpTopic <<- "kw2Test"
    myFunction = function(series, object2x, report) {
        x = tkEval(series)
        object <<- kw2Test(x[,1], x[,2])
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            object2x = FALSE,
            report = TRUE ),
        subject = "Location: Kruskal-Wallis" )
}


# ------------------------------------------------------------------------------


.fBasics.BivariateTests.varfTest  = 
function() 
{   # A function implemented by Diethelm Wuertz

    # Variance: F Test:
    helpTopic <<- "varfTest"
    myFunction = function(series, object2x, report) {
        x = tkEval(series)
        object <<- varfTest(x[,1], x[,2])
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            object2x = FALSE,
            report = TRUE ),
        subject = "Variance: F Test" )
}


# ------------------------------------------------------------------------------


.fBasics.BivariateTests.bartlett2Test  = 
function() 
{   # A function implemented by Diethelm Wuertz

    # Variance: Bartlett Test:
    helpTopic <<- "bartlett2Test"
    myFunction = function(series, object2x, report) {
        x = tkEval(series)
        object <<- bartlett2Test(x[,1], x[,2])
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            object2x = FALSE,
            report = TRUE ),
        subject = "Variance: Bartlett" )
}


# ------------------------------------------------------------------------------


.fBasics.BivariateTests.fligner2Test  = 
function() 
{   # A function implemented by Diethelm Wuertz

    # Variance: Fligner-Killeen Test:
    helpTopic <<- "fligner2Test"
    myFunction = function(series, object2x, report) {
        x = tkEval(series)
        object <<- fligner2Test(x[,1], x[,2])
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            object2x = FALSE,
            report = TRUE ),
        subject = "Variance: Fligner-Killeen" )
}


# ------------------------------------------------------------------------------


.fBasics.BivariateTests.ansariTest  = 
function() 
{   # A function implemented by Diethelm Wuertz

    # Scale: Ansari-Bradley Test:
    helpTopic <<- "ansariTest"
    myFunction = function(series, object2x, report) {
        x = tkEval(series)
        object <<- ansariTest(x[,1], x[,2])
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            object2x = FALSE,
            report = TRUE ),
        subject = "Scale: Ansari-Bradley" )
}


# ------------------------------------------------------------------------------

        
.fBasics.BivariateTests.moodTest = 
function() 
{   # A function implemented by Diethelm Wuertz

    # Scale: Mood Test:
    helpTopic <<- "moodTest"
    myFunction = function(series, object2x, report) {
        x = tkEval(series)
        object <<- moodTest(x[,1], x[,2])
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            object2x = FALSE,
            report = TRUE ),
        subject = "Scale: Mood" )
}


# ------------------------------------------------------------------------------


.fBasics.BivariateTests.pearsonTest = 
function() 
{   # A function implemented by Diethelm Wuertz

    # Correlation: Pearson Test:
    helpTopic <<- "pearsonTest"
    myFunction = function(series, object2x, report) {
        x = tkEval(series)
        object <<- pearsonTest(x[,1], x[,2])
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            object2x = FALSE,
            report = TRUE ),
        subject = "Correlation: Pearson" )
}


# ------------------------------------------------------------------------------


.fBasics.BivariateTests.kendallTest = 
function() 
{
    # Correlation: Kendall's tau Test:
    helpTopic <<- "kendallTest"
    myFunction = function(series, object2x, report) {
        x = tkEval(series)
        object <<- kendallTest(x[,1], x[,2])
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            object2x = FALSE,
            report = TRUE ),
        subject = "Correlation: Kendall" )
}


# ------------------------------------------------------------------------------


.fBasics.BivariateTests.spearmanTest = 
function() 
{   # A function implemented by Diethelm Wuertz

    # Correlation: Spearman's rho Test:
    helpTopic <<- "spearmanTest"
    myFunction = function(series, object2x, report) {
        x = tkEval(series)
        object <<- spearmanTest(x[,1], x[,2])
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            object2x = FALSE,
            report = TRUE ),
        subject = "Correlation: Spearman" )
}


################################################################################

