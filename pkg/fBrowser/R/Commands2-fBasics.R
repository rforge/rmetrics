
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


.fBasics.TimeSeriesImport.1 = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Print fWEBDATA Class Representation:
    tkGetClass("fWEBDATA")  
}


.fBasics.TimeSeriesImport.2 = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Economagic Series Download:
    tkExecute(
        fun = economagicImport,
        params = list(
            query = "fedny/day-fxus2eu", 
            file = "tempfile",
            source = "http://www.economagic.com/em-cgi/data.exe/",
            frequency = "daily", 
            save = FALSE,
            colname = "USDEUR",
            try = TRUE),
        infoName = "Economagic Download",
        tkoutput = TRUE,
        console = NULL ) 
}


.fBasics.TimeSeriesImport.3 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # Yahoo Series Download:
    tkExecute(
        fun = yahooImport,
        params = list(
            query = "s=IBM&a=11&b=1&c=1999&d=0&q=31&f=2000&z=IBM&x=.csv",
            file = "tempfile",
            source = "http://chart.yahoo.com/table.csv?",
            save = FALSE,
            sep = ";",
            swap = 20,
            try = TRUE),
        infoName = "Yahoo Download",
        tkoutput = TRUE,
        console = NULL )
}


.fBasics.TimeSeriesImport.4 = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # FRED St. Louis Series Download:
    tkExecute(
        fun = fredImport,
        params = list(
            query = "DPRIME", 
            file = "tempfile",
            source = "http://research.stlouisfed.org/fred2/series/", 
            frequency = "daily", 
            save = FALSE, 
            sep = ";", 
            try = TRUE),
        infoName = "FRED Download",
        tkoutput = TRUE,
        console = NULL )
}


.fBasics.TimeSeriesImport.5 = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # ... Extract Data Slot as.timeSeries:
    tkDataSot(object)
}


.fBasics.TimeSeriesImport.6 = 
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


.fBasics.BasicStatistics.1 = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Example timeSeries: x = Monthly SP500 Returns
    tkGetData(Data = "sp500Monthly", infoName = "Daily SP500 Returns")
}


.fBasics.BasicStatistics.2 = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Mean:
    myFunction = function(series) {
        x = eval(parse(text = series))
        object <<- mean(as.vector(x))
        .tkReport("Mean")
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x"),
        infoName = "Mean",
        tkoutput = TRUE,
        console = NULL,
        title = NULL,
        description = NULL )  
}


.fBasics.BasicStatistics.3 = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Variance:
    myFunction = function(series) {
        x = eval(parse(text = series))
        object <<- var(as.vector(x))
        .tkReport("Variance")
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x"),
        infoName = "Variance",
        tkoutput = TRUE,
        console = NULL,
        title = NULL,
        description = NULL )
}


.fBasics.BasicStatistics.4 = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Skewness:
    myFunction = function(series) {
        x = eval(parse(text = series))
        object <<- skewness(as.vector(x))
        .tkReport("Skewness")
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x"),
        infoName = "Skewness",
        tkoutput = TRUE,
        console = NULL,
        title = NULL,
        description = NULL )
}


.fBasics.BasicStatistics.5 = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Kurtosis:
    myFunction = function(series) {
        x = eval(parse(text = series))
        object <<- kurtosis(as.vector(x))
        .tkReport("Kurtosis")
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x"),
        infoName = "Kurtosis",
        tkoutput = TRUE,
        console = NULL,
        title = NULL,
        description = NULL )
}


.fBasics.BasicStatistics.6 = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Summary:
    myFunction = function(series) {
        x = eval(parse(text = series))
        object <<- summary(as.vector(x))
        .tkReport("Summary")
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x"),
        infoName = "Summary",
        tkoutput = TRUE,
        console = NULL,
        title = NULL,
        description = NULL )   
}


.fBasics.BasicStatistics.7 = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Basic Statistics:
    myFunction = function(series) {
        x = eval(parse(text = series))
        object <<- basicStats(as.vector(x), ci = 0.95) 
        .tkReport("Basic Statistics")
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x"),
        infoName = "Basic Statistics",
        tkoutput = TRUE,
        console = NULL,
        title = NULL,
        description = NULL )     
}


# ******************************************************************************
# Basic Plots


.fBasics.PlotFunctions.1 = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # * Example timeSeries: x = Monthly SP500 Returns:
    tkGetData(Data = "sp500Monthly", infoName = "Monthly SP500 Returns")
}


.fBasics.PlotFunctions.2 = 
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


.fBasics.PlotFunctions.3 = 
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


.fBasics.PlotFunctions.4 = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # 1D: Series Plot:
    myFunction = function(series) {
        x = eval(parse(text = series))
        par(mfrow = c(1, 1), cex = 0.7) 
        plot(x, col = "steelblue", xlab = "", ylab = "")
        title(main = paste("\n\n", plotTitle, sep = ""))
        invisible() }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x"),
        infoName = "Series Plot",
        tkoutput = TRUE,
        console = NULL,
        title = NULL,
        description = NULL )       
}


.fBasics.PlotFunctions.5 = 
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


.fBasics.PlotFunctions.6 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # * Example timeSeries: x = MSFT|SP500 Returns:
    tkGetData(Data = "msftsp500Monthly", 
        infoName = "Monthly MSFT|SP500 Returns")
}


.fBasics.PlotFunctions.7 = 
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


.fBasics.PlotFunctions.8 = 
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


.fBasics.ReturnDistributions.1 = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Normal Slider:
    .normSlider(GenerateRandomNumbers = TRUE)
}


.fBasics.ReturnDistributions.2 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # Normal Slider:
    .normSlider()
}


.fBasics.ReturnDistributions.3 = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Hyperbolic Slider:
    .hypSlider(GenerateRandomNumbers = TRUE)
}


.fBasics.ReturnDistributions.4 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # Hyperbolic Slider:
    .hypSlider()
}


.fBasics.ReturnDistributions.5 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # Normal Inverse Gaussian Slider:
    .nigSlider(GenerateRandomNumbers = TRUE)
}


.fBasics.ReturnDistributions.6 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # Normal Inverse Gaussian Slider:
    .nigSlider()  
}


.fBasics.ReturnDistributions.7 = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Symmetric Stable Slider:
    .symstbSlider(GenerateRandomNumbers = TRUE)
}


.fBasics.ReturnDistributions.8 = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Symmetric Stable Slider:
    .symstbSlider()
}


.fBasics.ReturnDistributions.9 = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Stable Slider:
    .stableSlider(GenerateRandomNumbers = TRUE)
}


.fBasics.ReturnDistributions.10 = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Stable Slider:
    .stableSlider()
}


.fBasics.ReturnDistributions.11 = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # * Example timeSeries: x = Monthly SP500 Returns:
    tkGetData(Data = "sp500Monthly", infoName = "Monthly SP500 Returns")
}


.fBasics.ReturnDistributions.12 = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    return(NA)
    
    o = ssdFit(rnorm(100))
    print(o)
    
    # Plot Data:     
    xmin = min(x)
    xmax = max(x)
    s = seq(xmin, xmax, length = 100)
    y1 = dssd(s, object)
    y2 = phyp(s, object)
    main1 = paste("Spline Smoothed Density\n", 
        "alpha = 1.4", " | ",
        "seed = 4711" )
    main2 = paste("Spline Smoothed Probability\n",
        "xmin = ", as.character(xmin), " | ",
        "xmax = ", as.character(xmax) )  
              
    par(mfrow = c(2, 1), cex = 0.7)
    
    hist(x, probability = TRUE, col = "steelblue", border = "white",
        xlim = c(xmin, xmax))
    lines(s, y1)
    abline(h = 0, lty = 3)
    abline(v = mu, lty = 3, col = "red")
    title(main = main1)  
         
    plot(s, y2, type = "l", xlim = c(xmin, xmax), ylim = c(0, 1),
        col = "steelblue" )
    abline(h = 0.0, lty = 3)
    abline(h = 1.0, lty = 3)
    abline(h = 0.5, lty = 3)
    abline(v = mu, lty = 3, col = "red")
    title(main = main2) 
          
    par(mfrow = c(1, 1), cex = 0.7)
}


# ******************************************************************************
# Distribution Fits


.fBasics.DistributionFits.1 = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # * Example timeSeries: x - Daily NYSE Returns
    tkGetData(Data = "nyseDaily", infoName = "Daily NYSE Returns")
} 


.fBasics.DistributionFits.2 = 
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
        title = NULL,
        description = NULL )      
} 


.fBasics.DistributionFits.3 = 
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
        title = NULL,
        description = NULL ) 
} 


.fBasics.DistributionFits.4 = 
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
        title = NULL,
        description = NULL )
}


# ******************************************************************************
# One Sample Tests


.fBasics.OneSampleTests.1 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # * Example timeSeries: - x = Monthly SP500 Returns
    tkGetData(Data = "sp500Monthly", infoName = "Monthly SP500 Returns")
}


.fBasics.OneSampleTests.2 = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Print fHTEST Class Representation:
    tkGetClass("fHTEST")
}


.fBasics.OneSampleTests.3 = 
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

      
.fBasics.OneSampleTests.4 = 
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


.fBasics.OneSampleTests.5 = 
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


.fBasics.OneSampleTests.6 = 
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


.fBasics.OneSampleTests.7 = 
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


.fBasics.OneSampleTests.8 = 
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

        
.fBasics.OneSampleTests.9 = 
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


.fBasics.OneSampleTests.10 = 
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


.fBasics.OneSampleTests.11 = 
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


.fBasics.TwoSampleTests.1  = 
function() 
{   # A function implemented by Diethelm Wuertz

    # * Example timeSeries: x = MSFT|SP500 Returns
    tkGetData(Data = "msftsp500Monthly", 
        infoName = "Monthly MSFT|SP500 Returns")
}


.fBasics.TwoSampleTests.2  = 
function() 
{   # A function implemented by Diethelm Wuertz

    # Print fHTEST Class Representation
    tkGetClass("fHTEST") 
}


.fBasics.TwoSampleTests.3  = 
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

        
.fBasics.TwoSampleTests.4  = 
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


.fBasics.TwoSampleTests.5  = 
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


.fBasics.TwoSampleTests.6  = 
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


.fBasics.TwoSampleTests.7  = 
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


.fBasics.TwoSampleTests.8  = 
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


.fBasics.TwoSampleTests.9  = 
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

        
.fBasics.TwoSampleTests.10 = 
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


.fBasics.TwoSampleTests.11 = 
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


.fBasics.TwoSampleTests.12 = 
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


.fBasics.TwoSampleTests.13 = 
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


.fBasics.StylizedFacts.1 = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # * Example timeSeries: x = Daily NYSE Returns
    tkGetData(Data = "nyseDaily", infoName = "Daily NYSE Returns")
}


.fBasics.StylizedFacts.2 =  
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


.fBasics.StylizedFacts.3 =  
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


.fBasics.StylizedFacts.4 =  
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


.fBasics.StylizedFacts.5 =  
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


.fBasics.StylizedFacts.6 =  
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

