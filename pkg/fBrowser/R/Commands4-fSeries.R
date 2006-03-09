
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
# Arma Modelling
# Garch Modelling
# Long Memory Modelling
# Chaotic Time Series
# PortableI nnovations
# Time Series Tests
# Unit Root Distribution
# Unit Root Tests
# Heaviside Function
# SkewNormal Distribution
# Skew Student Distribution
# Skew Ged Distribution
# Garch Distribution Fits
################################################################################
    

# ******************************************************************************
# Arma Modelling


.fSeries.ArmaModelling.1 = 
function()
{   # A function implemented by Diethelm Wuertz
     
    # Example timeSeries: x = NYSE Returns
    # * Example timeSeries: x - NYSE Returns
    tkGetData(Data = "nyseDaily", infoName = "Daily NYSE Returns")
}


.fSeries.ArmaModelling.2 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Example timeSeries: x = DEMGPD Returns
    tkGetData(Data = "dem2gbp", infoName = "Daily DEMGBP Returns")
}


.fSeries.ArmaModelling.3 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Print ARMA Class Representation:
    tkGetClass("fARMA")   
}


.fSeries.ArmaModelling.4 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Simulate ARMA Process:
    myFunction <<- function(ar, d, ma, n, object2x, report) {
        ar = as.numeric(unlist(strsplit(ar, ",")))
        d = as.numeric(d)
        ma = as.numeric(unlist(strsplit(ma, ",")))
        n = as.integer(n)
        object <<- armaSim(
            model = list(ar = ar, d = d, ma = ma), n = n, innov = NULL, 
            n.start = 100, start.innov = NULL, rand.gen = rnorm)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            ar = "0.5, -0.5", 
            d = "0",
            ma = "0.8",
            n = 100,
            object2x = TRUE,
            report = FALSE),
        infoName = "Simulated ARMA",
        tkoutput = FALSE,
        console = ".header.ts(x)",
        title = "Simulated ARMA Model",
        description = NULL )
}


.fSeries.ArmaModelling.5 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Fit ARMA Process:
    myFunction <<- function(formula, method, include.mean) {
        formula = as.formula(formula)
        include.mean = as.logical(include.mean)
        object <<- armaFit(formula = formula, method = method, 
            include.mean = include.mean, fixed = NULL,
            fracdiff.M = 100, fracdiff.h = -1, title = NULL, 
            description = NULL)
        fittedObject <<- tkSaveAs(
            data = object,
            infoName = "Fitted ARMA",
            console = NULL,
            what = "fitted")
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            formula = "x ~ arima(2, 0, 1)", 
            method = "CSS-ML",
            include.mean = TRUE),
        infoName = "Fitted ARMA",
        tkoutput = TRUE,
        console = NULL,
        title = NULL,
        description = NULL )  
}


.fSeries.ArmaModelling.6 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Summary Report:
    par(mfrow = c(2, 2), cex = 0.7)
    tkSummary(object)   
}


.fSeries.ArmaModelling.7 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Save Fitted Values in x:        
    x <<- tkSaveAs(
        data = fitted.values(object@fit), 
        infoName = "Vector of ARMA Fitted Values",
        console = "cat('\nVector of ARMA Fitted Values:\n', data[1:5]) ",
        what = "x" ) 
}


.fSeries.ArmaModelling.8 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Save Residual Values in x:
    x <<- tkSaveAs(
        data = residuals(object@fit), 
        infoName = "Vector of ARMA Residuals",
        console = "cat('\nVector of ARMA Residuals:\n', data[1:5]) ",
        what = "x" )
}


.fSeries.ArmaModelling.9 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # ARMA Forecast:
    myFunction = function(fit, n.ahead, n.back, conf) {
        fit = eval(parse(text = as.character(fit)))
        conf = eval(parse(text = conf))
        object <<- predict.fARMA(object = fit, n.ahead = n.ahead, 
            n.back = n.back, conf = conf, doplot = TRUE, 
            doprint = TRUE)
        predictedObject <<- tkSaveAs(
            data = object, 
            infoName = "ARMA Prediction",
            what = "predicted") 
        output = capture.output(object)
        .tkTitle("ARIMA Prediction")
        .tkOutput(output)
        .tkDescription(date())
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            fit = "fittedObject",
            n.ahead = 10,
            n.back = 50,
            conf = "c(80, 95)"),
        infoName = "ARMA Prediction",
        tkoutput = TRUE,
        console = NULL,
        title = NULL,
        description = NULL )  
}
    
   
# ******************************************************************************
# Garch Modelling


.fSeries.GarchModelling.1 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Example timeSeries: x = NYSE Returns
    tkGetData(Data = "nyseDaily", infoName = "Daily NYSE Returns")
}


.fSeries.GarchModelling.2 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Example timeSeries: x = GBPDEM Returns
    tkGetData(Data = "dem2gbp", infoName = "Daily DEMGBP Returns")
}


.fSeries.GarchModelling.3 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Print GARCH Class Representation
    tkGetClass("fGARCH")   
}


.fSeries.GarchModelling.4 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Simulate GARCH Process:
    myFunction = function(omega, alpha, beta, mu, n, object2x) {
        omega = omega
        alpha = as.numeric(unlist(strsplit(alpha, ",")))
        beta = as.numeric(unlist(strsplit(beta, ",")))
        mu = mu
        n = as.integer(n)
        object <<- garchSim(model = list(omega = omega, alpha = alpha, 
            beta = beta, mu = mu), n = n, innov = NULL, 
            n.start = 100, start.innov = NULL, rand.gen = rnorm)
        if (object2x) 
            x <<- tkSaveAs(
                data = object,
                infoName = "Simulated GARCH") 
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            omega = 1.0e-6,
            alpha = "0.1", 
            beta = "0.8",
            mu = 0,
            n = 1000,
            object2x = TRUE),
        infoName = "Simulated GARCH",
        tkoutput = FALSE,
        console = ".header.ts(x)",
        title = NULL,
        description = NULL )
}


.fSeries.GarchModelling.5 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Fit GARCH Process:
    myFunction = function(order) {
        order = eval(parse(text = order))
        garchFit(x = x, order = order)
    }
    tkExecute(
        fun = myFunction,
        params = list(
            order = "c(1, 1)"),
        infoName = "Fitted GARCH",
        tkoutput = TRUE,
        console = NULL,
        title = NULL,
        description = NULL ) 
}


.fSeries.GarchModelling.6 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # ... Print Summary Report:
    tkSummary(object)
}


.fSeries.GarchModelling.7 = 
function()
{   # A function implemented by Diethelm Wuertz

    # ... Fitted Values:
    x <<- tkSaveAs(
        data = as.vector(object$fitted.values[,1]), 
        infoName = "GARCH Fitted Values",
        console = "print(data[1:5])",
        what = "x" ) 
}


.fSeries.GarchModelling.8 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # ... Residual Values:
    x <<- tkSaveAs(
        data = object$residuals, 
        infoName = "GARCH Residuals",
        console = "print(data[1:5])" )
}


.fSeries.GarchModelling.9 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Print fAPARCH Class Representation
    tkGetClass("fGARCH")     
}


.fSeries.GarchModelling.10 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Simulate  GARCH Process:
    myFunction = function(omega, alpha, gamma, alpha.lags, beta, 
        beta.lags, delta, n, object2x) {
        alpha = as.numeric(unlist(strsplit(alpha, ",")))
        gamma = as.numeric(unlist(strsplit(gamma, ",")))
        alpha.lags = as.numeric(unlist(strsplit(alpha.lags, ",")))
        beta = as.numeric(unlist(strsplit(beta, ",")))
        beta.lags = as.numeric(unlist(strsplit(beta.lags, ",")))
        n = as.integer(n)
        rand.gen = rnorm 
        object <<- aparchSim(model = list(omega = omega, alpha = alpha, 
            gamma = gamma, alpha.lags = alpha.lags, beta = beta, 
            beta.lags = beta.lags, delta = delta), n = n, 
            innov = rand.gen(n, ...), n.start = 100, start.innov = NULL, 
            rand.gen = rnorm) 
        if (object2x) 
            x <<- tkSaveAs(
                data = object,
                infoName = "Simulated APARCH") 
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            omega = 1.0e-6,
            alpha = "0.1", 
            gamma = "0",
            alpha.lags = "1",
            beta = "0.8",
            beta.lags = "1",
            delta = 1.0,
            n = 1000,
            object2x = TRUE),
        infoName = "Simulated APARCH",
        tkoutput = FALSE,
        console = ".header.ts(x)",
        title = NULL,
        description = NULL )
}


.fSeries.GarchModelling.11 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Fit GARCH Process:
    myFunction = function(alpha.lags, beta.lags, delta, 
        opt.gamma, opt.delta, opt.disparm, distribution, disparm) {
        alpha.lags = as.numeric(unlist(strsplit(alpha.lags, ",")))
        beta.lags = as.numeric(unlist(strsplit(beta.lags, ",")))
        disparm = as.numeric(unlist(strsplit(disparm, ",")))
        aparchFit(x = x, order = list(alpha.lags = alpha.lags, 
            beta.lags = beta.lags, delta = delta), opt = list(gamma = 
            opt.gamma, delta = opt.delta, disparm = opt.disparm), 
            distribution = distribution, disparm = disparm, 
            n.cond = NULL, doprint = TRUE, method = "Nelder-Mead") 
    }
    tkExecute(
        fun = myFunction,
        params = list(
            alpha.lags = "1",
            beta.lags = "1",
            delta = 2.0,
            opt.gamma = FALSE,
            opt.delta = FALSE,
            opt.disparm = FALSE,
            distribution = "norm",
            disparm = "1, 4, 1.9"),
        infoName = "Fitted APARCH",
        tkoutput = TRUE,
        console = NULL,
        title = NULL,
        description = NULL ) 
}


.fSeries.GarchModelling.12 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # ... Print Summary Report:
    tkSummary(object)
}


.fSeries.GarchModelling.13 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # ... Fitted Values:
    x <<- tkSaveAs(
        data = as.vector(object$fitted.values[,1]), 
        infoName = "APARCH Fitted Values",
        console = "print(data[1:5])",
        what = "x" ) 
}

.fSeries.GarchModelling.14 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # ... Residual Values:
    x <<- tkSaveAs(
        data = object$residuals, 
        infoName = "APARCH Residuals",
        console = "print(data[1:5])",
        what = "x" )
}


# ******************************************************************************
# Long Memory Modelling


.fSeries.LongMemoryModelling.1 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # FGN Simulation:
    myFunction = function(n, H, method, mean, std) {
        n = as.integer(n)
        H = as.numeric(H)
        mean = as.numeric(mean)
        std = as.numeric(std)
        object <<- fgnSim(n = n, H = H, method = method, 
            mean = mean, std = std) 
        object }
    .xMenu(
        fun = myFunction,
        params = list(
            n = "1000", 
            H = "0.7",
            method = "beran",
            mean = "0",
            std = "1"),
        infoName = "Simulated FGN Process",
        tkoutput = FALSE,
        console = "print(head(x))",
        title = NULL,
        description = NULL )  
} 


# ******************************************************************************
# Chaotic Time Series


.fSeries.ChaoticTimeSeries.1 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Henon Map:
    myFunction = function(n, a, b, start, oject2x) {
        n = as.integer(n)
        parms = c(a = as.numeric(a), b = as.numeric(b))
        start = eval(parse(text = start))
        object <<- henonSim(n = n, parms = parms, start = start, 
            doplot = TRUE)
        object }
    par(mfrow = c(1, 1), cex = 0.7)
    tkExecute(
        fun = myFunction,
        params = list(
            n = "1000", 
            a = "1.4",
            b = "0.3",
            start = "runif(2)",
            object2x = FALSE ),
        infoName = "Henon Map",
        tkoutput = FALSE,
        console = NULL,
        title = NULL,
        description = NULL ) 
}


.fSeries.ChaoticTimeSeries.2 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Ikeda Map:
    myFunction = function(n, a, b, start, object2x) {
        n = as.integer(n)
        parms = c(a = as.numeric(a), b = as.numeric(b), c = as.numeric(c))
        start = eval(parse(text = start))
        object <<- ikedaSim(n = n, parms = parms, start = start, 
            doplot = TRUE)
        object }
    par(mfrow = c(2, 2), cex = 0.7)
    tkExecute(
        fun = myFunction,
        params = list(
            n = "1000", 
            a = "0.4",
            b = "6.0",
            c = "0.9",
            start = "runif(2)",
            object2x = FALSE ),
        infoName = "Henon Map",
        tkoutput = FALSE,
        console = NULL,
        title = NULL,
        description = NULL ) 
}


.fSeries.ChaoticTimeSeries.3 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Logistic Map:
    myFunction = function(n, r, start, object2x) {
        n = as.integer(n)
        parms = c(r = as.numeric(r))
        start = eval(parse(text = start))
        object <<- logisticSim(n = n, parms = parms, start = start, 
            doplot = TRUE)
        object }
    par(mfrow = c(1, 1), cex = 0.7)
    tkExecute(
        fun = myFunction,
        params = list(
            n = 1000, 
            r = 4.0,
            start = "runif(1)",
            object2x = FALSE ),
        infoName = "Logistic Map",
        tkoutput = FALSE,
        console = NULL,
        title = NULL,
        description = NULL ) 
}


.fSeries.ChaoticTimeSeries.4 = 
function()
{   # A function implemented by Diethelm Wuertz

    # Lorentz Attractor:
    myFunction = function(times, sigma, r, b, start, object2x) {
        par(mfrow = c(3, 2), cex = 0.7)
        times = eval(parse(text = times))
        parms = c(sigma = sigma, r = r, b = b)
        start = eval(parse(text = start))
        object <<- lorentzSim(times = times, parms = parms, 
            start = start, doplot = TRUE)
        object }
    par(mfrow = c(3, 2), cex = 0.7)
    tkExecute(
        fun = myFunction,
        params = list(
            times = "seq(0, 40, by = 0.01)", 
            sigma = 16.0,
            r = 45.92,
            b = 4.0,
            start = "c(-14, -13, 47)",
            object2x = FALSE ),
        infoName = "Lorentz Attractor",
        tkoutput = FALSE,
        console = NULL,
        title = NULL,
        description = NULL )
}


.fSeries.ChaoticTimeSeries.5 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Roessler Attractor:
    myFunction = function(times, a, b, c, start, object2x) {
        par(mfrow = c(3, 2), cex = 0.7)
        times = eval(parse(text = times))
        start = eval(parse(text = start))
        object <<- roesslerSim(times = times, parms = c(a = a, 
            b = b, c = c), start = start, doplot = TRUE)
        object }
    par(mfrow = c(4, 2), cex = 0.7)
    tkExecute(
        fun = myFunction,
        params = list(
            times = "seq(0, 100, by = 0.01)", 
            a = 0.2,
            b = 0.2,
            c = 8.0,
            start = "c(-1.894, -9.92, 0.025)",
            object2x = FALSE ),
        infoName = "Roessler Attractor",
        tkoutput = FALSE,
        console = NULL,
        title = NULL,
        description = NULL )
}


# ******************************************************************************
# Portable Innovations


.fSeries.PortableInnovations.1 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Uniform Innovations:
    myFunction = function(n, min, max, object2x) {
        n = as.integer(n)
        min = as.numeric(min)
        max = as.numeric(max)
        object <<- runif.lcg(n = n, min = min, max = max)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            n = 1000, 
            min = 0,
            max = 1,
            object2x = FALSE),
        infoName = "Portable Uniform Innovations",
        tkoutput = FALSE,
        console = "print(head(object))",
        title = NULL,
        description = NULL )
}
  

.fSeries.PortableInnovations.2 = 
function()
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Normal Innovations:
    myFunction = function(n, min, max, object2x) {
        n = as.integer(n)
        mean = as.numeric(mean)
        sd = as.numeric(sd)
       object <<-  rnorm.lcg(n = n, mean = mean, sd = sd)
       object
    }
    tkExecute(
        fun = myFunction,
        params = list(
            n = 1000, 
            mean = 0,
            sd = 1,
            object2x = FALSE),
        infoName = "Portable Normal Innovations",
        tkoutput = FALSE,
        console = "print(head(object))",
        title = NULL,
        description = NULL )
}
  

.fSeries.PortableInnovations.3 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Student-t Innovations:
    myFunction = function(n, df, object2x) {
        n = as.integer(n)
        df = as.numeric(df)
        object <<- rt.lcg(n = n, df = df)
        object
    }
    tkExecute(
        fun = myFunction,
        params = list(
            n = 1000, 
            df = 4,
            object2x = FALSE),
        infoName = "Portable Student-t Innovations",
        tkoutput = FALSE,
        console = "print(head(object))",
        title = NULL,
        description = NULL )
}



# ******************************************************************************
# Time Series Tests


.fSeries.TimeSeriesTests.1 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Example timeSeries: x - NYSE log Returns
    tkGetData(Data = "nyseDaily", infoName = "Daily NYSE Returns")
}
   
 
.fSeries.TimeSeriesTests.2 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # BDS NN Test:
    myFunction = function(m, eps) {
        m = as.integer(m)
        if (eps == "NULL") eps = NULL
        object <<- bdsTest(x = x, m = m, eps = eps, title = NULL, 
            description = NULL) 
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            m = 3,
            eps = "NULL"),
        infoName = "BDS NN Test",
        tkoutput = TRUE,
        console = NULL,
        title = NULL,
        description = NULL )
}
  
  
.fSeries.TimeSeriesTests.3 = 
function()
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Teraesvirta NN Test:
    myFunction = function(lag) {
        lag = as.integer(lag)
        object <<- tnnTest(x = x, lag = lag, title = NULL, 
            description = NULL) 
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            lag = "1"),
        infoName = "Teraesvirta NN Test",
        tkoutput = TRUE,
        console = NULL,
        title = NULL,
        description = NULL )
}
   
 
.fSeries.TimeSeriesTests.4 = 
function()
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # White NN Test:
    myFunction = function(lag, qstar, q, range) {
        lag = as.integer(lag)
        qstar = as.integer(qstar)
        q = as.integer(q)
        range = as.integer(range)
        object <<- wnnTest(x = x, lag = lag, qstar = qstar, q = q, 
            range = range, title = NULL, description = NULL) 
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            lag = 1,
            qstar = 2,
            q = 10,
            range = 4),
        infoName = "White NN Test",
        tkoutput = TRUE,
        console = NULL,
        title = NULL,
        description = NULL ) 
}


# ******************************************************************************
# Unit Root Distribution


.fSeries.UnitRootDistribution.1 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Output:
    output = capture.output(.unitrootSlider())
    .tkTitle(What)
    .tkOutput(output)
    .tkDescription() 
}


# ******************************************************************************
# Unit Root Tests


.fSeries.UnitRootTests.1 = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # has Unit Root:
    x <<- rnorm(1000)
    
    # Info:activeDataSet <<- paste("x =", What[choice])
    infoLabelText <<- tclVar(paste("Active Series Data:", activeDataSet))
    tkconfigure(infoLabel, textvariable = infoLabelText)
    tkgrid(infoLabel)
}
   
 
.fSeries.UnitRootTests.2 = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # has No Unit Root
    x <<- cumsum(c(0, rnorm(1000))) 
    
    # Info:activeDataSet <<- paste("x =", What[choice])
    infoLabelText <<- tclVar(paste("Active Series Data:", activeDataSet))
    tkconfigure(infoLabel, textvariable = infoLabelText)
    tkgrid(infoLabel)
}
   
 
.fSeries.UnitRootTests.3 = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # ADF Unit Root Test
    output <<- capture.output(adfTest(x))
    .tkOutput(output)
}
    
.fSeries.UnitRootTests.4 = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # McKinnon Unit Root Test
    output <<- capture.output(unitrootTest(x))
    .tkOutput(output)
}


# ******************************************************************************
# Heaviside Function


.fSeries.HeavisideFunction.1 = 
function(choice)
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Heaviside And Related Functions:
    .HeavisideSlider()
}


# ******************************************************************************
# Skew Normal|Student|GED Distributions


.fSeries.GarchDistributions.1 = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Generate Skew Normal Random Numbers
    .snormSlider(TRUE)
}


.fSeries.GarchDistributions.2 = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Skew Normal Distribution Slider
    .normSlider(TRUE)
}


.fSeries.GarchDistributions.3 = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Generate Skew Student-t Random Numbers
    .sstdSlider(TRUE)
}


.fSeries.GarchDistributions.4 = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Generate Student-t Distribution Slider
    .stdSlider(TRUE)
}


.fSeries.GarchDistributions.5 = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Generate Skew GED Random Numbers
    .sgedSlider(TRUE)
}


.fSeries.GarchDistributions.6 = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Generate GED Distribution Slider
    .gedSlider(TRUE)
}


# ******************************************************************************
# Garch Distribution Fits


.fSeries.GarchDistributionFits.1 = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # * Example timeSeries: x - NYSE log Returns
    tkGetData(Data = "nyseDaily", infoName = "Daily NYSE Returns")
}
   

.fSeries.GarchDistributionFits.2 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Fit to Normal Distribution
    tkFit(normFit, "Fit to Normal Distribution")
}
   

.fSeries.GarchDistributionFits.3 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Fit to Skew Normal Distribution
    tkFit(snormFit, "Fit to Skew Normal Distribution")
}
   

.fSeries.GarchDistributionFits.4 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Fit to Sudent-t Distribution
    tkFit(stdFit, "Fit to Student-t Distribution")        
}
   

.fSeries.GarchDistributionFits.5 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Fit Skew Sudent-t Distribution
    tkFit(sstdFit, "Fit to Skew Student-t Distribution")     
}
   

.fSeries.GarchDistributionFits.6 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Fit GED Distribution
    tkFit(gedFit, "Fit to GED Distribution")     
}
   

.fSeries.GarchDistributionFits.7 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Fit Skew GED Distribution
    tkFit(sgedFit, "Fit to Skew GED Distribution")   
}       
        
        
# ******************************************************************************
# fSeries data

        
.fSeries.SeriesData.1 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # nyseres:
    tkGetData(Data = "nyseres", infoName = "Data Set")
}


.fSeries.SeriesData.2 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # recession:
    tkGetDataFrame(Data = "recession", infoName = "Data Set")
}


.fSeries.SeriesData.3 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # dem2gbp:
    tkGetData(Data = "dem2gbp", infoName = "Data Set")
}


.fSeries.SeriesData.4 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # cac40:
    tkGetDataFrame(Data = "cac40", infoName = "Data Set")
}


.fSeries.SeriesData.5 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # nelsonplosser:
    tkGetDataFrame(Data = "nelsonplosser", infoName = "Data Set")
}


.fSeries.SeriesData.6 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # surex1.ts:
    tkGetDataFrame(Data = "surex1.ts", infoName = "Data Set")
}


.fSeries.SeriesData.7 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # rf.30day:
    tkGetData(Data = "rf.30day", infoName = "Data Set")
}


.fSeries.SeriesData.8 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # black.ts:
    tkGetData(Data = "black.ts", infoName = "Data Set")
}


.fSeries.SeriesData.9 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # klein:
    tkGetData(Data = "klein", infoName = "Data Set")
}


.fSeries.SeriesData.10 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # kmenta:
    tkGetData(Data = "kmenta", infoName = "Data Set")
}
   
    
# ------------------------------------------------------------------------------
# Mills Data


.fSeries.MillsData.1 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # RS - Monthly 91 Day Treasury Bill Rate:
    tkGetData(Data = "RS", infoName = "Monthly 91 Day Treasury Bill Rate")
}


.fSeries.MillsData.2 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # R20 - Monthly Yield on 20 Year UK Gilts:
    tkGetData(Data = "R20", infoName = "Monthly Yield on 20 Year UK Gilts")
}


.fSeries.MillsData.3 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # RSQ - Quarterly 91 Day Treasury Bill Rate:
    tkGetData(Data = "RSQ", infoName = "Quarterly 91 Day Treasury Bill Rate")
}


.fSeries.MillsData.4 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # R20Q - Quarterly Yield on 20 Year UK Gilts:
    tkGetData(Data = "R20Q", infoName = "Quarterly Yield on 20 Year UK Gilts")
}
 

.fSeries.MillsData.5 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # RSQREAL - Quarterly Real 91 Day Treasury Bill:
    tkGetData(Data = "RSQREAL", infoName = "Quarterly Real 91 Day TBill")
}


.fSeries.MillsData.6 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # FTAPRICE - FTA All Share Price Index:
    tkGetData(Data = "FTAPRICE", infoName = "FTA All Share Price Index")
}


.fSeries.MillsData.7 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # FTADIV - FTA All Share Dividend Index:
    tkGetData(Data = "FTADIV", infoName = "FTA All Share Dividend Index")
}


.fSeries.MillsData.8 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # FTARET - FTA All Share Nominal Returns:
    tkGetData(Data = "FTARET", infoName = "FTA All Share Nominal Returns")
}


.fSeries.MillsData.9 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # RPI - UK Retail Price Index:
    tkGetData(Data = "RPI", infoName = "UK Retail Price Index")
}


.fSeries.MillsData.10 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # EXCHD - Dollar/Sterling Exchange Rate:
    tkGetData(Data = "EXCHD", infoName = "")
}


.fSeries.MillsData.11 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # EXCHQ - Dollar/Sterling Exchange Rate:
    tkGetData(Data = "EXCHQ", infoName = "Dollar/Sterling Exchange Rate")
}


.fSeries.MillsData.12 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # SP500 - SP 500 Annual Data Index:
    tkGetData(Data = "SP500", infoName = "")
}


.fSeries.MillsData.13 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # SP500R - SP 500 Real Returns:
    tkGetData(Data = "SP500R", infoName = "SP 500 Annual Data Index")
}


.fSeries.MillsData.14 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # SP500D - SP 500 Daily Data Index:
    tkGetData(Data = "SP500D", infoName = "SP 500 Daily Data Index")
}


.fSeries.MillsData.15 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # FT30 - Financial Times FT 30 Index:
    tkGetData(Data = "FT30", infoName = "SP 500 Daily Data Index")
}


.fSeries.MillsData.16 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # FTSE100 - FTSE 100 Index:
    tkGetData(Data = "FTSE100", infoName = "FTSE 100 Index")
}

.fSeries.MillsData.17 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # CTLD - Courtaulds Share Price:
    tkGetData(Data = "CTLD", infoName = "Courtaulds Share Price")
}


.fSeries.MillsData.18 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # LGEN - Legal and General Share Price:
    tkGetData(Data = "LGEN", infoName = "Legal and General Share Price")
}


.fSeries.MillsData.19 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # PRU - Prudential Share Price:
    tkGetData(Data = "PRU", infoName = "Prudential Share Price")
}


################################################################################

