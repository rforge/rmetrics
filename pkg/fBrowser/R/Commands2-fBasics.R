
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
# Web Data Import


.fBasics.Import.class = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Print fWEBDATA Class Representation:
    tkGetClass("fWEBDATA")  
}


.fBasics.Import.economagic = 
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
        infoName = "Economagic Download",
        tkoutput = TRUE,
        console = NULL,
        title = "Economagic Download",
        description = NULL ) 
}


.fBasics.Import.yahoo = 
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
        infoName = "Yahoo Download",
        tkoutput = TRUE,
        console = NULL,
        title = "Yahoo Download",
        description = NULL ) 
}


.fBasics.Import.fred = 
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
        infoName = "FRED Download",
        tkoutput = TRUE,
        console = NULL,
        title = "FRED Download",
        description = NULL ) 
}


.fBasics.Import.keystats = 
function() 
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Yahoo Key Statistics Download:
    tkExecute(
        fun = keystatsImport,
        params = list(
            query = "IBM", 
            file = "tempfile", 
            source = "http://finance.yahoo.com/q/ks?s=", 
            save = FALSE, 
            try = TRUE),
        infoName = "Yahoo Key Statistics",
        tkoutput = TRUE,
        console = NULL)
}



# ******************************************************************************
# Basic Statistics


.fBasics.BasicStatistics.sp500monthly = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Example timeSeries: x = Monthly SP500 Returns
    tkGetData(Data = "sp500Monthly", infoName = "End of Month SP500 Returns")
}


.fBasics.BasicStatistics.mean = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Mean:
    myFunction = function(series, object2x, report) {
        x = eval(parse(text = series))
        object <<- c(mean = mean(as.vector(x)))
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            object2x = FALSE,
            report = TRUE),
        infoName = "Compute Mean",
        tkoutput = TRUE,
        console = NULL,
        title = "Compute Mean",
        description = NULL )  
}


.fBasics.BasicStatistics.var = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Variance:
    myFunction = function(series, object2x, report) {
        x = eval(parse(text = series))
        object <<- c(var = var(as.vector(x)))
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            object2x = FALSE,
            report = TRUE),
        infoName = "Compute Variance",
        tkoutput = TRUE,
        console = NULL,
        title = "Compute Variance",
        description = NULL )
}


.fBasics.BasicStatistics.skewness = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Skewness:
    myFunction = function(series, na.rm, method, object2x, report) {
        x = eval(parse(text = series))
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
        infoName = "Compute Skewness",
        tkoutput = TRUE,
        console = NULL,
        title = "Compute Skewness",
        description = NULL )
}


.fBasics.BasicStatistics.kurtosis = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Kurtosis:
    myFunction = function(series, na.rm, method, object2x, report) {
        x = eval(parse(text = series))
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
        infoName = "Compute Kurtosis",
        tkoutput = TRUE,
        console = NULL,
        title = "Compute Kurtosis",
        description = NULL )
}


.fBasics.BasicStatistics.summary = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Summary:
    myFunction = function(series, object2x, report) {
        x = eval(parse(text = series))
        object <<- summary(as.vector(x))
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            object2x = FALSE,
            report = TRUE),
        infoName = "Print Summary Statistics",
        tkoutput = TRUE,
        console = NULL,
        title = "Print Summary Statistics",
        description = NULL )   
}


.fBasics.BasicStatistics.basicStats = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Basic Statistics:
    myFunction = function(series, object2x, report) {
        x = eval(parse(text = series))
        object <<- basicStats(as.vector(x), ci = 0.95) 
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            object2x = FALSE,
            report = TRUE),
        infoName = "Print Basic Statistics",
        tkoutput = TRUE,
        console = NULL,
        title = "Print Basic Statistics",
        description = NULL )     
}


# ******************************************************************************
# Basic Plots


.fBasics.PlotFunctions.sp500Monthly = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # * Example timeSeries: x = Monthly SP500 Returns:
    tkGetData(Data = "sp500Monthly", infoName = "Monthly SP500 Returns")
}


.fBasics.PlotFunctions.plot = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # 1D: Series Plot:
    myFunction = function(series, typecol, xlab, ylab) {
        x = eval(parse(text = series))
        par(mfrow = c(1, 1), cex = 0.7) 
        plot(x, type = type, col = col, xlab = xlab, ylab = ylab)
        title(main = paste("\n\n", plotTitle, sep = ""))
        invisible() }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            type = "l",
            col = "steelblue",
            xlab = "Index",
            ylab = "Series"),
        infoName = "Series Plot",
        tkoutput = TRUE,
        console = NULL,
        title = NULL,
        description = NULL )       
}


.fBasics.PlotFunctions.acfPlot = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # 1D: ACF Plot:
    myFunction = function(series, grid, cex) {
        x = eval(parse(text = series))
        grid = as.logical(grid)
        cex = as.numeric(cex)
        par(mfrow = c(1,1), cex = cex) 
        ans = acfPlot(x)
        title(main = paste("\n\n", plotTitle, sep = ""))
        if (grid) grid()
        ans }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            grid = "FALSE",
            cex = 1),
        infoName = "ACF Plot",
        console = "print(object)" )        
}


.fBasics.PlotFunctions.pacfPlot = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # 1D: PACF Plot:
    myFunction = function(series) {
        x = eval(parse(text = series))
        par(mfrow = c(1, 1), cex = 0.7) 
        ans = pacfPlot(x)
        title(main = paste("\n\n", plotTitle, sep = ""))
        ans }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x"),
        infoName = "PACF Plot",
        console = "print(object)" )        
}


.fBasics.PlotFunctions.histPlot = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # 1D: Histogram Plot:
    myFunction = function(series) {
        x = eval(parse(text = series))
        par(mfrow = c(1, 1), cex = 0.7) 
        histPlot(x)
        title(main = paste("\n\n", plotTitle, sep = ""))
        invisible() }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x"),
        infoName = "Histogram Plot",
        tkoutput = TRUE,
        console = NULL,
        title = NULL,
        description = NULL )       
}


.fBasics.PlotFunctions.msftsp500Monthly = 
function() 
{   # A function implemented by Diethelm Wuertz

    # * Example timeSeries: x = MSFT|SP500 Returns:
    tkGetData(Data = "msftsp500Monthly", infoName = "Monthly MSFT|SP500 Returns")
}


.fBasics.PlotFunctions.bivariatePlot = 
function() 
{   # A function implemented by Diethelm Wuertz
  
    # 2D: Series Plot:
    myFunction = function(series) {
        x = eval(parse(text = series))
        par(mfrow = c(1, 1), cex = 0.7) 
        plot(x, xlab = "", ylab = "")
        title(main = paste("\n\n", plotTitle, sep = ""))
        invisible() }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x"),
        infoName = "2D Series Plot",
        tkoutput = TRUE,
        console = NULL,
        title = NULL,
        description = NULL )       
}


.fBasics.PlotFunctions.scatterPlot = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # 2D: Scatter Diagramm Plot:
    myFunction = function(series) {
        x = eval(parse(text = series))
        par(mfrow = c(1, 1), cex = 0.7) 
        plot(as.vector(x[, 1]), as.vector(x[,2]),
            xlab = x@units[1], ylab = x@units[2], 
            pch = 19, col  = "steelblue")
        title(main = paste("\n\n", plotTitle, sep = ""))
        invisible() }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x"),
        infoName = "Scatter Diagram Plot",
        tkoutput = TRUE,
        console = NULL,
        title = NULL,
        description = NULL )         
}


# ******************************************************************************
# Return Distributions


.fBasics.ReturnDistributions.nyseDaily = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # * Example timeSeries: x - Daily NYSE Returns
    tkGetData(Data = "nyseDaily", infoName = "Daily NYSE Returns")
} 


.fBasics.ReturnDistributions.rnorm = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Normal RVs:
    myFunction = function(n, mean, sd, object2x, report) {
        object <<- as.ts(rnorm(n, mean, sd))
        attr(object, "control") <<- 
            c(dist = "norm", mean = as.character(mean), sd = as.character(sd))
        object}
    tkExecute(
        fun = myFunction,
        params = list(
            n = 100,
            mean = 0,
            sd = 1,
            object2x = TRUE,
            report = TRUE),
        infoName = "Normal RVs",
        tkoutput = TRUE,
        console = NULL,
        title = "Normal RVs",
        description = NULL )  
}


.fBasics.ReturnDistributions.rnormSlider = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Normal Slider:
    .normSlider(GenerateRandomNumbers = TRUE)
}


.fBasics.ReturnDistributions.dnormSlider = 
function() 
{   # A function implemented by Diethelm Wuertz

    # Normal Slider:
    .normSlider()
}


.fBasics.ReturnDistributions.rhyp = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Hyperbolic RVs:
    myFunction = function(n, alpha, beta, delta, mu, parameterization, 
        object2x, report) {
        object <<- as.ts(rhyp(n = n, alpha = alpha, beta = beta, delta = delta,
            pm = parameterization))
        attr(object, "param") <<- NULL   
        attr(object, "control") <<- 
            c(dist = "hyp", 
            alpha = as.character(alpha), 
            beta = as.character(beta),
            delta = as.character(delta), 
            mu = as.character(mu),
            pm = as.character(parameterization))
        object}
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
        infoName = "Hyperbolic RVs",
        tkoutput = TRUE,
        console = NULL,
        title = "Hyperbolic RVs",
        description = NULL )  
}


.fBasics.ReturnDistributions.rhypSlider = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Hyperbolic Slider:
    .hypSlider(GenerateRandomNumbers = TRUE)
}


.fBasics.ReturnDistributions.dhypSlider = 
function() 
{   # A function implemented by Diethelm Wuertz

    # Hyperbolic Slider:
    .hypSlider()
}


.fBasics.ReturnDistributions.rnig = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Normal Inverse Gaussian RVs:
    myFunction = function(n, alpha, beta, delta, mu, 
        object2x, report) {
        object <<- as.ts(rnig(n = n, alpha = alpha, beta = beta, 
            delta = delta))
        # attr(object, "param") <<- NULL   
        attr(object, "control") <<- 
            c(dist = "nig", 
            alpha = as.character(alpha), 
            beta = as.character(beta),
            delta = as.character(delta), 
            mu = as.character(mu))
        object}
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
        infoName = "Normal Inverse Gaussian RVs",
        tkoutput = TRUE,
        console = NULL,
        title = "Normal Inverse Gaussian RVs",
        description = NULL )  
}


.fBasics.ReturnDistributions.rnigSlider = 
function() 
{   # A function implemented by Diethelm Wuertz

    # Normal Inverse Gaussian Slider:
    .nigSlider(GenerateRandomNumbers = TRUE)
}


.fBasics.ReturnDistributions.dnigSlider = 
function() 
{   # A function implemented by Diethelm Wuertz

    # Normal Inverse Gaussian Slider:
    .nigSlider()  
}


.fBasics.ReturnDistributions.rsymstb = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Symmetric Stable RVs:
    myFunction = function(n, alpha, object2x, report) {
        object <<- as.ts(rsymstb(n = n, alpha = alpha))
        # attr(object, "param") <<- NULL   
        attr(object, "control") <<- 
            c(dist = "symstb", 
            alpha = as.character(alpha))
        object}
    tkExecute(
        fun = myFunction,
        params = list(
            n = 100,
            alpha = 1.8,
            object2x = TRUE,
            report = TRUE),
        infoName = "Symmetric Stable RVs",
        tkoutput = TRUE,
        console = NULL,
        title = "Symmetric Stable RVs",
        description = NULL )  
}


.fBasics.ReturnDistributions.rsymstbSlider = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Symmetric Stable Slider:
    .symstbSlider(GenerateRandomNumbers = TRUE)
}


.fBasics.ReturnDistributions.dsymstbSlider = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Symmetric Stable Slider:
    .symstbSlider()
}


.fBasics.ReturnDistributions.rstable = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Stable RVs:
    myFunction = function(n, alpha, beta, gamma, delta, parameterization, 
        object2x, report) {
        object <<- as.ts(rstable(n = n, alpha = alpha, beta = beta, 
            gamma = gamma, delta = delta, pm = parameterization)) 
        attr(object, "control") <<- 
            c(dist = "stable", 
            alpha = as.character(alpha), 
            beta = as.character(beta),
            gamma = as.character(gamma),
            delta = as.character(delta), 
            pm = as.character(parameterization))
        object}
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
        infoName = "Stable RVs",
        tkoutput = TRUE,
        console = NULL,
        title = "Stable RVs",
        description = NULL )  
}

.fBasics.ReturnDistributions.rstableSlider = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Stable Slider:
    .stableSlider(GenerateRandomNumbers = TRUE)
}


.fBasics.ReturnDistributions.dstableSlider = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Stable Slider:
    .stableSlider()
}


# ******************************************************************************
# Distribution Fits


.fBasics.DistributionFits.norm = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Normal Distribution:
    myFunction = function(series) {
        x = eval(parse(text = series))
        object <<- .normFit(x)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x"),
        infoName = "Normal Fit",
        tkoutput = TRUE,
        console = NULL,
        title = "Normal Fit",
        description = NULL )      
} 


.fBasics.DistributionFits.hyp = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Hyperbolic Distribution:
    myFunction = function(series) {
        x = eval(parse(text = series))
        object <<- hypFit(x)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x"),
        infoName = "Hyperbolic Fit",
        tkoutput = TRUE,
        console = NULL,
        title = "Hyperbolic Fit",
        description = NULL ) 
} 


.fBasics.DistributionFits.nig = 
function() 
{   # A function implemented by Diethelm Wuertz

    # Normal Inverse Gaussian Distribution:
    myFunction = function(series) {
        x = eval(parse(text = series))
        object <<- nigFit(x)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x"),
        infoName = "Normal Inverse Gaussian Fit",
        tkoutput = TRUE,
        console = NULL,
        title = "Normal Inverse Gaussian Fit",
        description = NULL )
}


# ******************************************************************************
# One Sample Tests


.fBasics.NormalityTests.sp500Monthly = 
function() 
{   # A function implemented by Diethelm Wuertz

    # * Example timeSeries: - x = Monthly SP500 Returns
    tkGetData(Data = "sp500Monthly", infoName = "Monthly SP500 Returns")
}


.fBasics.NormalityTests.getClass = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Print fHTEST Class Representation:
    tkGetClass("fHTEST")
}


.fBasics.NormalityTests.ksnormTest = 
function() 
{   # A function implemented by Diethelm Wuertz

    # One-sample Kolmogorov-Smirnov Test:
    myFunction = function(series) {
        x = eval(parse(text = series))
        object <<- ksnormTest(x)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x"),
        infoName = "One-sample Kolmogorov-Smirnov Test",
        tkoutput = TRUE,
        console = NULL,
        title = NULL,
        description = NULL ) 
}

      
.fBasics.NormalityTests.shapiroTest = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Shapiro - Wilk Test:
    myFunction = function(series) {
        x = eval(parse(text = series))
        object <<- shapiroTest(x)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x"),
        infoName = "Shapiro - Wilk Normality Test",
        tkoutput = TRUE,
        console = NULL,
        title = NULL,
        description = NULL ) 
}


.fBasics.NormalityTests.jarqueberaTest = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Jarque-Bera Test:
    myFunction = function(series) {
        x = eval(parse(text = series))
        object <<- jarqueberaTest(x)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x"),
        infoName = "Jarque - Bera Normality Test",
        tkoutput = TRUE,
        console = NULL,
        title = NULL,
        description = NULL )    
}


.fBasics.NormalityTests.dagoTest = 
function() 
{   # A function implemented by Diethelm Wuertz

    # d'Agostino Test:
    myFunction = function(series) {
        x = eval(parse(text = series))
        object <<- dagoTest(x)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x"),
        infoName = "d'Agostino Normality Test",
        tkoutput = TRUE,
        console = NULL,
        title = NULL,
        description = NULL ) 
}


.fBasics.NormalityTests.adTest = 
function() 
{   # A function implemented by Diethelm Wuertz

    # Anderson - Darling Normality Test:
    myFunction = function(series) {
        x = eval(parse(text = series))
        object <<- adTest(x)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x"),
        infoName = "Anderson - Darling Normality Test",
        tkoutput = TRUE,
        console = NULL,
        title = NULL,
        description = NULL )
}


.fBasics.NormalityTests.cvmTest = 
function() 
{   # A function implemented by Diethelm Wuertz

    # Cramer - von Mises Normality Test:
    myFunction = function(series) {
        x = eval(parse(text = series))
        object <<- cvmTest(x)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x"),
        infoName = "Cramer - von Mises Normality Test",
        tkoutput = TRUE,
        console = NULL,
        title = NULL,
        description = NULL )
}

        
.fBasics.NormalityTests.lillieTest = 
function() 
{   # A function implemented by Diethelm Wuertz

    # Lilliefors (KS) Normality Test:
    myFunction = function(series) {
        x = eval(parse(text = series))
        object <<- lillieTest(x)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x"),
        infoName = "Lilliefors (KS) Normality Test",
        tkoutput = TRUE,
        console = NULL,
        title = NULL,
        description = NULL )
}


.fBasics.NormalityTests.pchiTest = 
function() 
{   # A function implemented by Diethelm Wuertz

    # Pearson Chi-Square Normality Test:
    myFunction = function(series) {
        x = eval(parse(text = series))
        object <<- pchiTest(x)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x"),
        infoName = "Pearson Chi-Square Normality Test",
        tkoutput = TRUE,
        console = NULL,
        title = NULL,
        description = NULL )
}


.fBasics.NormalityTests.sfTest = 
function() 
{   # A function implemented by Diethelm Wuertz

    # Shapiro - Francia Normality Test:
    myFunction = function(series) {
        x = eval(parse(text = series))
        object <<- sfTest(x)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x"),
        infoName = "Shapiro - Francia Normality Test",
        tkoutput = TRUE,
        console = NULL,
        title = NULL,
        description = NULL )
}


# ******************************************************************************
# Two Sample Tests


.fBasics.BivariateTests.msftsp500Monthly  = 
function() 
{   # A function implemented by Diethelm Wuertz

    # * Example timeSeries: x = MSFT|SP500 Returns
    tkGetData(Data = "msftsp500Monthly", 
        infoName = "Monthly MSFT|SP500 Returns")
}


.fBasics.BivariateTests.getClass  = 
function() 
{   # A function implemented by Diethelm Wuertz

    # Print fHTEST Class Representation
    tkGetClass("fHTEST") 
}


.fBasics.BivariateTests.ks2Test  = 
function() 
{   # A function implemented by Diethelm Wuertz

    # Kolmogorov-Smirnov Test:
    myFunction = function(series) {
        x = eval(parse(text = series))
        object <<- ks2Test(x[,1], x[,2])
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x"),
        infoName = "Kolmogorov-Smirnov Test",
        tkoutput = TRUE,
        console = NULL,
        title = NULL,
        description = NULL )
}

        
.fBasics.BivariateTests.tTest  = 
function() 
{   # A function implemented by Diethelm Wuertz

    # Location: Unpaired t-Test:
    myFunction = function(series) {
        x = eval(parse(text = series))
        object <<- tTest(x[,1], x[,2])
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x"),
        infoName = "Location: Unpaired t-Test",
        tkoutput = TRUE,
        console = NULL,
        title = NULL,
        description = NULL )
}


.fBasics.BivariateTests.kw2Test  = 
function() 
{   # A function implemented by Diethelm Wuertz

    # Location: Kruskal-Wallis Test:
    myFunction = function(series) {
        x = eval(parse(text = series))
        object <<- kw2Test(x[,1], x[,2])
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x"),
        infoName = "Location: Kruskal-Wallis Test",
        tkoutput = TRUE,
        console = NULL,
        title = NULL,
        description = NULL )
}


.fBasics.BivariateTests.varfTest  = 
function() 
{   # A function implemented by Diethelm Wuertz

    # Variance: F Test:
    myFunction = function(series) {
        x = eval(parse(text = series))
        object <<- varfTest(x[,1], x[,2])
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x"),
        infoName = "Variance: F Test",
        tkoutput = TRUE,
        console = NULL,
        title = NULL,
        description = NULL )
}


.fBasics.BivariateTests.bartlett2Test  = 
function() 
{   # A function implemented by Diethelm Wuertz

    # Variance: Bartlett Test:
    myFunction = function(series) {
        x = eval(parse(text = series))
        object <<- bartlett2Test(x[,1], x[,2])
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x"),
        infoName = "Variance: Bartlett Test",
        tkoutput = TRUE,
        console = NULL,
        title = NULL,
        description = NULL )
}


.fBasics.BivariateTests.fligner2Test  = 
function() 
{   # A function implemented by Diethelm Wuertz

    # Variance: Fligner-Killeen Test:
    myFunction = function(series) {
        x = eval(parse(text = series))
        object <<- fligner2Test(x[,1], x[,2])
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x"),
        infoName = "Variance: Fligner-Killeen Test",
        tkoutput = TRUE,
        console = NULL,
        title = NULL,
        description = NULL )
}


.fBasics.BivariateTests.ansariTest  = 
function() 
{   # A function implemented by Diethelm Wuertz

    # Scale: Ansari-Bradley Test:
    myFunction = function(series) {
        x = eval(parse(text = series))
        object <<- ansariTest(x[,1], x[,2])
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x"),
        infoName = "Scale: Ansari-Bradley Test",
        tkoutput = TRUE,
        console = NULL,
        title = NULL,
        description = NULL )
}

        
.fBasics.BivariateTests.moodTest = 
function() 
{   # A function implemented by Diethelm Wuertz

    # Scale: Mood Test:
    myFunction = function(series) {
        x = eval(parse(text = series))
        object <<- moodTest(x[,1], x[,2])
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x"),
        infoName = "Scale: Mood Test",
        tkoutput = TRUE,
        console = NULL,
        title = NULL,
        description = NULL )
}


.fBasics.BivariateTests.pearsonTest = 
function() 
{   # A function implemented by Diethelm Wuertz

    # Correlation: Pearson Test:
    myFunction = function(series) {
        x = eval(parse(text = series))
        object <<- pearsonTest(x[,1], x[,2])
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x"),
        infoName = "Correlation: Pearson Test",
        tkoutput = TRUE,
        console = NULL,
        title = NULL,
        description = NULL )
}


.fBasics.BivariateTests.kendallTest = 
function() 
{
    # Correlation: Kendall's tau Test:
    myFunction = function(series) {
        x = eval(parse(text = series))
        object <<- kendallTest(x[,1], x[,2])
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x"),
        infoName = "Correlation: Kendall's tau Test",
        tkoutput = TRUE,
        console = NULL,
        title = NULL,
        description = NULL )
}


.fBasics.BivariateTests.spearmanTest = 
function() 
{   # A function implemented by Diethelm Wuertz

    # Correlation: Spearman's rho Test:
    myFunction = function(series) {
        x = eval(parse(text = series))
        object <<- spearmanTest(x[,1], x[,2])
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x"),
        infoName = "Correlation: Spearman's rho Test",
        tkoutput = TRUE,
        console = NULL,
        title = NULL,
        description = NULL )
}


# ******************************************************************************
# D1-StylizedFacts


.fBasics.StylizedFacts.nyseDaily = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # * Example timeSeries: x = Daily NYSE Returns
    tkGetData(Data = "nyseDaily", infoName = "Daily NYSE Returns")
}


.fBasics.StylizedFacts.teffectPlot =  
function() 
{   # A function implemented by Diethelm Wuertz

    # Taylor Effect:
    myFunction = function(series, deltas, lag.max, standardize, report) {
        x = eval(parse(text = series))
        deltas = eval(parse(text = deltas))
        par(mfrow = c(1, 1), cex = 0.7)
        object <<- teffectPlot(as.vector(x), deltas = deltas, 
            lag.max = lag.max, standardize = standardize) 
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            deltas = "seq(0.2, 3, by=0.2)",
            lag.max = 10,
            standardize = TRUE,
            report = FALSE),
        infoName = "Taylor Effect Plot",
        tkoutput = FALSE,
        console = "print(object)",
        title = "Taylor Effect Plot",
        description = NULL ) 
}


.fBasics.StylizedFacts.lmacfPlot =  
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Long Memory Behaviour:
    myFunction = function(series, lag.max, ci, report) {
        x = eval(parse(text = series))
        lag.max = eval(parse(text = lag.max))
        par(mfrow = c(2, 1), cex = 0.7)
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
        console = "print(object)",
        title = "Long Memory Plot",
        description = NULL ) 
}


.fBasics.StylizedFacts.logpdfPlot =  
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Log PDF Plot:
    myFunction = function(series) {
        x = eval(parse(text = series))
        par(mfrow = c(1, 1), cex = 0.7)
        object <<- logpdfPlot(abs(as.vector(x))) 
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x"),
        infoName = "Log PDF Plot",
        tkoutput = FALSE,
        console = "print(object)",
        title = "Log PDF Plot",
        description = NULL ) 
}


.fBasics.StylizedFacts.qqgaussPlot =  
function() 
{   # A function implemented by Diethelm Wuertz

    # Normal QQ Plot:
    myFunction = function(series, span, report) {
        x = eval(parse(text = series))
        par(mfrow = c(1, 1), cex = 0.7)
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
        console = "print(object)",
        title = "Normal QQ Plot",
        description = NULL ) 
}


.fBasics.StylizedFacts.scalinglawPlot =  
function() 
{   # A function implemented by Diethelm Wuertz
 
    # Scaling Law Plot:
    myFunction = function(series, span, report) {
        x = eval(parse(text = series))
        if (span == "NULL") span = ceiling(log(length(as.vector(x))/252)/log(2))
        print(span)
        par(mfrow = c(1, 1), cex = 0.7)
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
        console = "print(object)",
        title = "Scaling Law Plot",
        description = NULL ) 
}


################################################################################

