
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
    myFunction = function(value, file, frequency, save, colname, try,
        object2x, report) {
        object <<- economagicImport(query = value, file = file, 
            frequency = frequency, save = save, colname = colname,
            try = try)
        if (object2x) object <<- as.timeSeries(object@data)
        object }   
    tkExecute(
        fun = myFunction,
        params = list(
            value = "fedny/day-fxus2eu", 
            file = "tempfile",
            source = "http://www.economagic.com/em-cgi/data.exe/",
            frequency = "daily", 
            save = FALSE,
            colname = "USDEUR",
            try = TRUE,
            object2x = TRUE,
            report = TRUE),
        infoName = "Economagic Download" ) 
}


# ------------------------------------------------------------------------------


.fBasics.Import.yahooImport = 
function() 
{   # A function implemented by Diethelm Wuertz

    # Yahoo Series Download:
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
        object <<- yahooImport(query = value, file = file, source = source, 
            save = save, sep = sep, swap = swap, try = try)
        if (object2x) object <<- as.timeSeries(object@data)
        object }   
    tkExecute(
        fun = myFunction,
        params = list(
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
        infoName = "Yahoo Download" ) 
}


# ------------------------------------------------------------------------------


.fBasics.Import.fredImport = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # FRED St. Louis Series Download:
    myFunction = function(value, file, source, frequency, save, sep, 
        try, object2x, report) {
        object <<- fredImport(query = value, file = file, source = source, 
            frequency = frequency, save = save, sep = sep, try = try)
        if (object2x) object <<- as.timeSeries(object@data)
        object }   
    tkExecute(
        fun = myFunction,
        params = list(
            value = "DPRIME", 
            file = "tempfile",
            source = "http://research.stlouisfed.org/fred2/series/", 
            frequency = "daily", 
            save = FALSE, 
            sep = ";", 
            try = TRUE, 
            object2x = TRUE,
            report = TRUE),
        infoName = "FRED Download" ) 
}


################################################################################
# Basic Statistics


.fBasics.BasicStatistics.mean = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Mean:
    myFunction = function(series, object2x, report) {
        x = tkEval(series)
        object <<- c(mean = mean(as.vector(x)))
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            object2x = FALSE,
            report = TRUE),
        infoName = "Compute Mean" )  
}


# ------------------------------------------------------------------------------


.fBasics.BasicStatistics.var = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Variance:
    myFunction = function(series, object2x, report) {
        x = tkEval(series)
        object <<- c(var = var(as.vector(x)))
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            object2x = FALSE,
            report = TRUE),
        infoName = "Compute Variance" )
}


# ------------------------------------------------------------------------------


.fBasics.BasicStatistics.skewness = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Skewness:
    myFunction = function(series, na.rm, method, object2x, report) {
        x = tkEval(series)
        object <<- c(skewness = skewness(as.vector(x), 
            na.rm = na.rm, method = method))
        attr(object, "method") <<- method
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            na.rm = FALSE,
            method = "moment",
            object2x = FALSE,
            report = TRUE),
        infoName = "Compute Skewness" )
}


# ------------------------------------------------------------------------------


.fBasics.BasicStatistics.kurtosis = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Kurtosis:
    myFunction = function(series, na.rm, method, object2x, report) {
        x = tkEval(series)
        object <<- c(kurtosis = kurtosis(as.vector(x), 
            na.rm = na.rm, method = method))
        attr(object, "method") <<- method
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            na.rm = FALSE,
            method = "excess",
            object2x = FALSE,
            report = TRUE),
        infoName = "Compute Kurtosis" )
}


# ------------------------------------------------------------------------------


.fBasics.BasicStatistics.summary = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Summary:
    myFunction = function(series, object2x, report) {
        x = tkEval(series)
        object <<- summary(as.vector(x))
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            object2x = FALSE,
            report = TRUE),
        infoName = "Print Summary Statistics")   
}


# ------------------------------------------------------------------------------


.fBasics.BasicStatistics.basicStats = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Basic Statistics:
    myFunction = function(series, object2x, report) {
        x = tkEval(series)
        object <<- basicStats(as.vector(x), ci = 0.95) 
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            object2x = FALSE,
            report = TRUE),
        infoName = "Print Basic Statistics" )     
}


################################################################################
# Basic Plots


.fBasics.PlotFunctions.plot = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # 1D: Time Series Plot:
    myFunction = function(series, type, col, xlab, ylab, grid) {
        x = tkEval(series)
        plot(x, type = type, col = col, xlab = xlab, ylab = ylab)
        title(main = paste("\n\n", plotTitle, sep = "")) 
        if (grid) grid()}
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            type = "l",
            col = "steelblue",
            xlab = "Index",
            ylab = "Series",
            grid = FALSE),
        infoName = "Series Plot")       
}


# ------------------------------------------------------------------------------


.fBasics.PlotFunctions.acfPlot = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # 1D: ACF Plot:
    myFunction = function(series, grid) {
        x = tkEval(series)
        acfPlot(x)
        title(main = paste("\n\n", plotTitle, sep = ""))
        if (grid) grid() }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            grid = "FALSE"),
        infoName = "ACF Plot" )            
}


# ------------------------------------------------------------------------------


.fBasics.PlotFunctions.pacfPlot = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # 1D: PACF Plot:
    myFunction = function(series, grid) {
        x = tkEval(series)
        pacfPlot(x)
        title(main = paste("\n\n", plotTitle, sep = ""))
        if (grid) grid() }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            grid = TRUE),
        infoName = "PACF Plot" )            
}


# ------------------------------------------------------------------------------


.fBasics.PlotFunctions.histPlot = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # 1D: Histogram Plot:
    myFunction = function(series) {
        x = tkEval(series)
        histPlot(x)
        title(main = paste("\n\n", plotTitle, sep = "")) }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x"),
        infoName = "Histogram Plot" )       
}


# ------------------------------------------------------------------------------


.fBasics.PlotFunctions.bivariatePlot = 
function() 
{   # A function implemented by Diethelm Wuertz
  
    # 2D: Series Plot:
    myFunction = function(series) {
        x = tkEval(series)
        plot(x, xlab = "", ylab = "")
        title(main = paste("\n\n", plotTitle, sep = "")) }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x"),
        infoName = "2D Series Plot" )       
}


# ------------------------------------------------------------------------------


.fBasics.PlotFunctions.scatterPlot = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # 2D: Scatter Diagramm Plot:
    myFunction = function(series) {
        x = tkEval(series)
        plot(as.vector(x[, 1]), as.vector(x[,2]),
            xlab = x@units[1], ylab = x@units[2], 
            pch = 19, col  = "steelblue")
        title(main = paste("\n\n", plotTitle, sep = "")) }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x"),
        infoName = "Scatter Diagram Plot" )         
}


################################################################################
# Return Distributions


.fBasics.ReturnDistributions.rnorm = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Normal RVs:
    myFunction = function(n, mean, sd, object2x, report) {
        object <<- as.ts(rnorm(n, mean, sd))
        attr(object, "control") <<- 
            c(dist = "norm", mean = as.character(mean), sd = as.character(sd))
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            n = 100,
            mean = 0,
            sd = 1,
            object2x = TRUE,
            report = TRUE),
        infoName = "Normal RVs" )  
}


# ------------------------------------------------------------------------------


.fBasics.ReturnDistributions.rnormSlider = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Normal Slider:
    .normSlider(GenerateRandomNumbers = TRUE)
}


# ------------------------------------------------------------------------------


.fBasics.ReturnDistributions.dnormSlider = 
function() 
{   # A function implemented by Diethelm Wuertz

    # Normal Slider:
    .normSlider()
}


# ------------------------------------------------------------------------------


.fBasics.ReturnDistributions.rhyp = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Hyperbolic RVs:
    myFunction = function(n, alpha, beta, delta, mu, parameterization, 
        object2x, report) {
        object <<- as.ts(rhyp(n = n, alpha = alpha, beta = beta, delta = delta,
            pm = parameterization))
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            n = 100,
            alpha = 1,
            beta = 0,
            delta = 1,
            mu = 0,
            parameterization = 1,
            object2x = TRUE,
            report = TRUE),
        infoName = "Hyperbolic RVs" )  
}


# ------------------------------------------------------------------------------

.fBasics.ReturnDistributions.rhypSlider = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Hyperbolic Slider:
    .hypSlider(GenerateRandomNumbers = TRUE)
}


# ------------------------------------------------------------------------------


.fBasics.ReturnDistributions.dhypSlider = 
function() 
{   # A function implemented by Diethelm Wuertz

    # Hyperbolic Slider:
    .hypSlider()
}


# ------------------------------------------------------------------------------


.fBasics.ReturnDistributions.rnig = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Normal Inverse Gaussian RVs:
    myFunction = function(n, alpha, beta, delta, mu, 
        object2x, report) {
        object <<- as.ts(rnig(n = n, alpha = alpha, beta = beta, 
            delta = delta))
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            n = 100,
            alpha = 1,
            beta = 0,
            delta = 1,
            mu = 0,
            object2x = TRUE,
            report = TRUE),
        infoName = "Normal Inverse Gaussian RVs" )  
}


# ------------------------------------------------------------------------------


.fBasics.ReturnDistributions.rnigSlider = 
function() 
{   # A function implemented by Diethelm Wuertz

    # Normal Inverse Gaussian Slider:
    .nigSlider(GenerateRandomNumbers = TRUE)
}


# ------------------------------------------------------------------------------


.fBasics.ReturnDistributions.dnigSlider = 
function() 
{   # A function implemented by Diethelm Wuertz

    # Normal Inverse Gaussian Slider:
    .nigSlider()  
}


# ------------------------------------------------------------------------------


.fBasics.ReturnDistributions.rsymstb = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Symmetric Stable RVs:
    myFunction = function(n, alpha, object2x, report) {
        object <<- as.ts(rsymstb(n = n, alpha = alpha))
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            n = 100,
            alpha = 1.8,
            object2x = TRUE,
            report = TRUE),
        infoName = "Symmetric Stable RVs" )  
}


# ------------------------------------------------------------------------------


.fBasics.ReturnDistributions.rsymstbSlider = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Symmetric Stable Slider:
    .symstbSlider(GenerateRandomNumbers = TRUE)
}


# ------------------------------------------------------------------------------


.fBasics.ReturnDistributions.dsymstbSlider = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Symmetric Stable Slider:
    .symstbSlider()
}


# ------------------------------------------------------------------------------


.fBasics.ReturnDistributions.rstable = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Stable RVs:
    myFunction = function(n, alpha, beta, gamma, delta, parameterization, 
        object2x, report) {
        object <<- as.ts(rstable(n = n, alpha = alpha, beta = beta, 
            gamma = gamma, delta = delta, pm = parameterization)) 
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            n = 100,
            alpha = 1,
            beta = 0,
            gamma = 1,
            delta = 0,
            mu = 0,
            parameterization = 0,
            object2x = TRUE,
            report = TRUE),
        infoName = "Stable RVs" )  
}


# ------------------------------------------------------------------------------


.fBasics.ReturnDistributions.rstableSlider = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Stable Slider:
    .stableSlider(GenerateRandomNumbers = TRUE)
}


# ------------------------------------------------------------------------------


.fBasics.ReturnDistributions.dstableSlider = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Stable Slider:
    .stableSlider()
}


################################################################################
# Distribution Fits


.fBasics.DistributionFits.normFit = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Normal Distribution:
    myFunction = function(series, object2x, report) {
        x = as.vector(eval(parse(text = series)))
        object <<- list(estimate = c(mean = mean(x), sd = sd(x))) 
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            object2x = FALSE,
            report = TRUE),
        infoName = "Normal Fit" )      
} 


# ------------------------------------------------------------------------------


.fBasics.DistributionFits.hypFit = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Hyperbolic Distribution:
    myFunction = function(series, object2x, report) {
        x = tkEval(series)
        object <<- hypFit(x)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            object2x = FALSE,
            report = TRUE),
        infoName = "Hyperbolic Fit" ) 
} 


# ------------------------------------------------------------------------------


.fBasics.DistributionFits.nigFit = 
function() 
{   # A function implemented by Diethelm Wuertz

    # Normal Inverse Gaussian Distribution:
    myFunction = function(series, object2x, report) {
        x = tkEval(series)
        object <<- nigFit(x)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            object2x = FALSE,
            report = TRUE),
        infoName = "Normal Inverse Gaussian Fit" )
}


################################################################################
# Normality Tests


.fBasics.NormalityTests.ksnormTest = 
function() 
{   # A function implemented by Diethelm Wuertz

    # One-sample Kolmogorov-Smirnov Test:
    myFunction = function(series, object2x, report) {
        x = tkEval(series)
        object <<- ksnormTest(x)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            object2x = FALSE,
            report = TRUE ),
        infoName = "One-sample Kolmogorov-Smirnov Test" ) 
}


# ------------------------------------------------------------------------------


.fBasics.NormalityTests.shapiroTest = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Shapiro - Wilk Test:
    myFunction = function(series, object2x, report) {
        x = tkEval(series)
        object <<- shapiroTest(x)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            object2x = FALSE,
            report = TRUE ),
        infoName = "Shapiro - Wilk Normality Test" ) 
}


# ------------------------------------------------------------------------------


.fBasics.NormalityTests.jarqueberaTest = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Jarque-Bera Test:
    myFunction = function(series, object2x, report) {
        x = tkEval(series)
        object <<- jarqueberaTest(x)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            object2x = FALSE,
            report = TRUE ),
        infoName = "Jarque - Bera Normality Test" )    
}


# ------------------------------------------------------------------------------


.fBasics.NormalityTests.dagoTest = 
function() 
{   # A function implemented by Diethelm Wuertz

    # d'Agostino Test:
    myFunction = function(series, object2x, report) {
        x = tkEval(series)
        object <<- dagoTest(x)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            object2x = FALSE,
            report = TRUE ),
        infoName = "d'Agostino Normality Test" ) 
}


# ------------------------------------------------------------------------------


.fBasics.NormalityTests.adTest = 
function() 
{   # A function implemented by Diethelm Wuertz

    # Anderson - Darling Normality Test:
    myFunction = function(series, object2x, report) {
        x = tkEval(series)
        object <<- adTest(x)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            object2x = FALSE,
            report = TRUE ),
        infoName = "Anderson - Darling Normality Test" )
}


# ------------------------------------------------------------------------------


.fBasics.NormalityTests.cvmTest = 
function() 
{   # A function implemented by Diethelm Wuertz

    # Cramer - von Mises Normality Test:
    myFunction = function(series, object2x, report) {
        x = tkEval(series)
        object <<- cvmTest(x)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            object2x = FALSE,
            report = TRUE ),
        infoName = "Cramer - von Mises Normality Test" )
}


# ------------------------------------------------------------------------------

        
.fBasics.NormalityTests.lillieTest = 
function() 
{   # A function implemented by Diethelm Wuertz

    # Lilliefors (KS) Normality Test:
    myFunction = function(series, object2x, report) {
        x = tkEval(series)
        object <<- lillieTest(x)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            object2x = FALSE,
            report = TRUE ),
        infoName = "Lilliefors (KS) Normality Test" )
}


# ------------------------------------------------------------------------------


.fBasics.NormalityTests.pchiTest = 
function() 
{   # A function implemented by Diethelm Wuertz

    # Pearson Chi-Square Normality Test:
    myFunction = function(series, object2x, report) {
        x = tkEval(series)
        object <<- pchiTest(x)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            object2x = FALSE,
            report = TRUE ),
        infoName = "Pearson Chi-Square Normality Test" )
}


# ------------------------------------------------------------------------------


.fBasics.NormalityTests.sfTest = 
function() 
{   # A function implemented by Diethelm Wuertz

    # Shapiro - Francia Normality Test:
    myFunction = function(series, object2x, report) {
        x = tkEval(series)
        object <<- sfTest(x)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            object2x = FALSE,
            report = TRUE ),
        infoName = "Shapiro - Francia Normality Test" )
}


################################################################################
# Bivariate Tests


.fBasics.BivariateTests.ks2Test  = 
function() 
{   # A function implemented by Diethelm Wuertz

    # Kolmogorov-Smirnov Test:
    myFunction = function(series, object2x, report) {
        x = tkEval(series)
        object <<- ks2Test(x[,1], x[,2])
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            object2x = FALSE,
            report = TRUE ),
        infoName = "Kolmogorov-Smirnov Test" )
}


# ------------------------------------------------------------------------------

        
.fBasics.BivariateTests.tTest  = 
function() 
{   # A function implemented by Diethelm Wuertz

    # Location: Unpaired t-Test:
    myFunction = function(series, object2x, report) {
        x = tkEval(series)
        object <<- tTest(x[,1], x[,2])
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            object2x = FALSE,
            report = TRUE ),
        infoName = "Location: Unpaired t-Test" )
}


# ------------------------------------------------------------------------------


.fBasics.BivariateTests.kw2Test  = 
function() 
{   # A function implemented by Diethelm Wuertz

    # Location: Kruskal-Wallis Test:
    myFunction = function(series, object2x, report) {
        x = tkEval(series)
        object <<- kw2Test(x[,1], x[,2])
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            object2x = FALSE,
            report = TRUE ),
        infoName = "Location: Kruskal-Wallis Test" )
}


# ------------------------------------------------------------------------------


.fBasics.BivariateTests.varfTest  = 
function() 
{   # A function implemented by Diethelm Wuertz

    # Variance: F Test:
    myFunction = function(series, object2x, report) {
        x = tkEval(series)
        object <<- varfTest(x[,1], x[,2])
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            object2x = FALSE,
            report = TRUE ),
        infoName = "Variance: F Test" )
}


# ------------------------------------------------------------------------------


.fBasics.BivariateTests.bartlett2Test  = 
function() 
{   # A function implemented by Diethelm Wuertz

    # Variance: Bartlett Test:
    myFunction = function(series, object2x, report) {
        x = tkEval(series)
        object <<- bartlett2Test(x[,1], x[,2])
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            object2x = FALSE,
            report = TRUE ),
        infoName = "Variance: Bartlett Test" )
}


# ------------------------------------------------------------------------------


.fBasics.BivariateTests.fligner2Test  = 
function() 
{   # A function implemented by Diethelm Wuertz

    # Variance: Fligner-Killeen Test:
    myFunction = function(series, object2x, report) {
        x = tkEval(series)
        object <<- fligner2Test(x[,1], x[,2])
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            object2x = FALSE,
            report = TRUE ),
        infoName = "Variance: Fligner-Killeen Test" )
}


# ------------------------------------------------------------------------------


.fBasics.BivariateTests.ansariTest  = 
function() 
{   # A function implemented by Diethelm Wuertz

    # Scale: Ansari-Bradley Test:
    myFunction = function(series, object2x, report) {
        x = tkEval(series)
        object <<- ansariTest(x[,1], x[,2])
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            object2x = FALSE,
            report = TRUE ),
        infoName = "Scale: Ansari-Bradley Test" )
}


# ------------------------------------------------------------------------------

        
.fBasics.BivariateTests.moodTest = 
function() 
{   # A function implemented by Diethelm Wuertz

    # Scale: Mood Test:
    myFunction = function(series, object2x, report) {
        x = tkEval(series)
        object <<- moodTest(x[,1], x[,2])
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            object2x = FALSE,
            report = TRUE ),
        infoName = "Scale: Mood Test" )
}


# ------------------------------------------------------------------------------


.fBasics.BivariateTests.pearsonTest = 
function() 
{   # A function implemented by Diethelm Wuertz

    # Correlation: Pearson Test:
    myFunction = function(series, object2x, report) {
        x = tkEval(series)
        object <<- pearsonTest(x[,1], x[,2])
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            object2x = FALSE,
            report = TRUE ),
        infoName = "Correlation: Pearson Test" )
}


# ------------------------------------------------------------------------------


.fBasics.BivariateTests.kendallTest = 
function() 
{
    # Correlation: Kendall's tau Test:
    myFunction = function(series, object2x, report) {
        x = tkEval(series)
        object <<- kendallTest(x[,1], x[,2])
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            object2x = FALSE,
            report = TRUE ),
        infoName = "Correlation: Kendall's tau Test" )
}


# ------------------------------------------------------------------------------


.fBasics.BivariateTests.spearmanTest = 
function() 
{   # A function implemented by Diethelm Wuertz

    # Correlation: Spearman's rho Test:
    myFunction = function(series, object2x, report) {
        x = tkEval(series)
        object <<- spearmanTest(x[,1], x[,2])
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            object2x = FALSE,
            report = TRUE ),
        infoName = "Correlation: Spearman's rho Test" )
}


################################################################################
# Stylized Facts


.fBasics.StylizedFacts.teffectPlot =  
function() 
{   # A function implemented by Diethelm Wuertz

    # Taylor Effect:
    myFunction = function(series, deltas, lag.max, standardize, report) {
        x = tkEval(series)
        deltas = eval(parse(text = deltas))
        object <<- teffectPlot(as.vector(x), deltas = deltas, 
            lag.max = lag.max, standardize = standardize) 
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            deltas = "seq(0.2,3,by=0.2)",
            lag.max = 10,
            standardize = TRUE,
            report = FALSE),
        infoName = "Taylor Effect Plot",
        tkoutput = FALSE,
        title = "Taylor Effect Plot",
        description = NULL ) 
}


# ------------------------------------------------------------------------------


.fBasics.StylizedFacts.lmacfPlot =  
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Long Memory Behaviour:
    myFunction = function(series, lag.max, ci, report) {
        x = tkEval(series)
        lag.max = eval(parse(text = lag.max))
        object <<- lmacfPlot(abs(as.vector(x))) 
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            lag.max = "max(2, floor(10*log10(length(x))))",
            ci = 0.95,
            report = FALSE),
        infoName = "Long Memory Plot",
        tkoutput = FALSE,
        title = "Long Memory Plot",
        description = NULL ) 
}


# ------------------------------------------------------------------------------


.fBasics.StylizedFacts.logpdfPlot =  
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Log PDF Plot:
    myFunction = function(series) {
        x = tkEval(series)
        object <<- logpdfPlot(abs(as.vector(x))) 
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x"),
        infoName = "Log PDF Plot",
        tkoutput = FALSE,
        title = "Log PDF Plot",
        description = NULL ) 
}


# ------------------------------------------------------------------------------


.fBasics.StylizedFacts.qqgaussPlot =  
function() 
{   # A function implemented by Diethelm Wuertz

    # Normal QQ Plot:
    myFunction = function(series, span, report) {
        x = tkEval(series)
        object <<- qqgaussPlot(as.vector(x), span = span) 
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            span = 5,
            report = FALSE),
        infoName = "Normal QQ Plot",
        tkoutput = FALSE,
        title = "Normal QQ Plot",
        description = NULL ) 
}


# ------------------------------------------------------------------------------


.fBasics.StylizedFacts.scalinglawPlot =  
function() 
{   # A function implemented by Diethelm Wuertz
 
    # Scaling Law Plot:
    myFunction = function(series, span, report) {
        x = tkEval(series)
        if (span == "NULL") span = ceiling(log(length(as.vector(x))/252)/log(2))
        print(span)
        object <<- scalinglawPlot(x, span = span) 
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            span = "NULL",
            report = FALSE),
        infoName = "Scaling Law Plot",
        tkoutput = FALSE,
        title = "Scaling Law Plot",
        description = NULL ) 
}


################################################################################

