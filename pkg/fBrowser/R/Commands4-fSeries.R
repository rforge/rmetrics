
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


.fSeries.ArmaModelling.nyseDaily =  
function()
{   # A function implemented by Diethelm Wuertz

    # BMW Data Set:
    myFunction = function(object2x, report) {
        object <<- tkGetDemoData(Data = "nyseDaily", report = report) 
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            object2x = TRUE,
            report = FALSE),
        infoName = "Nyse Daily Data Set" )       
}


# ------------------------------------------------------------------------------


.fSeries.ArmaModelling.armaSim = 
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
        infoName = "Simulated ARMA" )
}


# ------------------------------------------------------------------------------


.fSeries.ArmaModelling.armaFit = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Fit ARMA Process:
    myFunction <<- function(formula, method, include.mean, doplot, 
        par, object2x, report) {
        formula = as.formula(formula)
        include.mean = as.logical(include.mean)
        object <<- armaFit(formula = formula, method = method, 
            include.mean = include.mean, fixed = NULL,
            fracdiff.M = 100, fracdiff.h = -1, title = NULL, 
            description = NULL)
        if (doplot) {
            eval(parse(text = par))
            summary(object)
        }
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            formula = "x ~ arima(2, 0, 1)", 
            method = "CSS-ML",
            include.mean = TRUE,
            doplot = TRUE,
            par = "par(mfrow=c(2,2),cex=0.7)",
            object2x = FALSE,
            report = TRUE ),
        infoName = "Fitted ARMA")  
}


# ------------------------------------------------------------------------------


.fSeries.ArmaModelling.predict = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # ARMA Forecast:
    myFunction = function(fit, n.ahead, n.back, conf, object2x, report) {
        fit = eval(parse(text = as.character(fit)))
        conf = eval(parse(text = conf))
        object <<- predict.fARMA(object = fit, n.ahead = n.ahead, 
            n.back = n.back, conf = conf, doplot = TRUE, 
            doprint = TRUE) 
        output = capture.output(object)
        tkTitle("ARIMA Prediction")
        tkOutput(output)
        tkDescription(date())
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            fit = "fittedObject",
            n.ahead = 10,
            n.back = 50,
            conf = "c(80, 95)",
            object2x = FALSE,
            report = TRUE ),
        infoName = "Predicted ARMA" )  
}
    
   
################################################################################
# Garch Modelling


.fSeries.GarchModelling.dem2gbpDaily =  
function()
{   # A function implemented by Diethelm Wuertz

    # BMW Data Set:
    myFunction = function(object2x, report) {
        object <<- tkGetDemoData(Data = "dem2gbp", report = report) 
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            object2x = TRUE,
            report = FALSE),
        infoName = "DEMGBP Daily Data Set" )       
}



# ------------------------------------------------------------------------------


.fSeries.GarchModelling.fGARCH = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Print GARCH Class Representation
    tkGetClass("fGARCH")   
}


# ------------------------------------------------------------------------------


.fSeries.GarchModelling.garchSim = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Simulate GARCH Process:
    myFunction = function(model, n, n.start, presample, cond.dist,
        rseed, object2x, report) {
        model = eval(parse(text = model)) 
        if (presample == "NULL") presample = NULL
        cond.dist = tkSplit(cond.dist) 
        if (rseed == "NULL") rseed = NULL
        object <<- garchSim(model, n, n.start, presample, cond.dist, rseed)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            model = "list(omega=1e-06,alpha=0.1,beta=0.8)",
            n = 100,
            n.start = 100,
            presample = "NULL",
            cond.dist = "rnorm & rged & rstd & rsnorm & rsged & rsstd",
            rseed= "NULL",
            object2x = TRUE,
            report = TRUE ),
        infoName = "Simulated GARCH" )
}


# ------------------------------------------------------------------------------


.fSeries.GarchModelling.garchFit = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Fit GARCH Process:
    myFunction <<- function(formula.mean, formula.var, series, init.rec, 
        delta, skew, shape, cond.dist, include.mean, include.delta, 
        include.skew, include.shape, leverage, trace, algorithm, control, 
        object2x, report) {
        formula.mean = as.formula(formula.mean)
        formula.var = as.formula(formula.var)
        x = tkEval(series)
        init.rec = tkSplit(init.rec)
        cond.dist = tkSplit(cond.dist)
        if (include.delta == "NULL") include.delta = NULL
        if (include.skew == "NULL") include.skew = NULL
        if (include.shape == "NULL") include.shape = NULL
        if (leverage == "NULL") leverage = NULL
        algorithm = tkSplit(algorithm)
        control = eval(parse(text = control))
        object <<- garchFit(formula.mean, formula.var, series = x, init.rec, 
            delta, skew, shape, cond.dist, include.mean, include.delta, 
            include.skew, include.shape, leverage, trace, algorithm, 
            control, title = NULL, description = NULL) 
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            formula.mean = "~arma(0, 0)", 
            formula.var = "~garch(1, 1)",
            series = "x",
            init.rec = "mci & uev",
            delta = 2, 
            skew = 1, 
            shape = 4,
            cond.dist = "dnorm & dsnorm & dged & dsged & dstd & dsstd", 
            include.mean = TRUE, 
            include.delta = "NULL", 
            include.skew = "NULL", 
            include.shape = "NULL", 
            leverage = "NULL", 
            trace = TRUE, 
            algorithm = "sqp & nlminb & lbfgsb & nlminb+nm & lbfgsb+nm", 
            control = "list()",
            object2x = FALSE,
            report = TRUE ),
        infoName = "Fitted GARCH")  
}


# ------------------------------------------------------------------------------


.fSeries.GarchModelling.predict = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # ... Print Summary Report:
    tkGetSummary(object)
}



################################################################################
# Long Memory Modelling


.fSeries.LongMemoryModelling.fHURST = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Print ARMA Class Representation:
    tkGetClass("fHURST")   
}


# ------------------------------------------------------------------------------


.fSeries.LongMemoryModelling.fgnSim = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Fractional Gaussian Noise Simulation:
    myFunction = function(n, H, method, object2x, report) {
        method = tkSplit(method) 
        object <<- fgnSim(n, H, method) 
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            n = 100,
            H = 0.7,
            method = "beran & durbin & paxson",
            object2x = TRUE,
            report = TRUE),
        infoName = "Simulated Fractional Gaussian Noise" )  
} 


# ------------------------------------------------------------------------------


.fSeries.LongMemoryModelling.fbmSim = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Fractional Brownian Motion Simulation:
    myFunction = function(n, H, method, object2x, report) {
        method = tkSplit(method) 
        object <<- fbmSim(n, H, method, waveJ, doplot = TRUE, fgn) 
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            n = 100,
            H = 0.7,
            method = "mvn & chol & lev & circ & wave",
            waveJ = 7,
            fgn = FALSE,
            object2x = TRUE,
            report = TRUE),
        infoName = "Simulated Fractional Brownian Motion" )  
} 


################################################################################
# Chaotic Time Series


.fSeries.ChaoticTimeSeries.henonSim = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Henon Map:
    myFunction = function(n, a, b, start, par, oject2x, report) {
        parms = c(a = a, b = b)
        if (!is.numeric(start)) start = eval(parse(text = start))
        eval(parse(text = par))
        object <<- henonSim(n = n, parms = parms, start = start, 
            doplot = TRUE)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            n = 1000, 
            a = 1.4,
            b = 0.3,
            start = "runif(2)",
            par = "par(mfrow=c(1,1))",
            object2x = FALSE,
            report = TRUE ),
        infoName = "Henon Map" ) 
}


# ------------------------------------------------------------------------------


.fSeries.ChaoticTimeSeries.ikedaSim = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Ikeda Map:
    myFunction = function(n, a, b, start, par, object2x, report) {
        n = as.integer(n)
        parms = c(a = a, b = b, c = c)
        start = eval(parse(text = start))
        eval(parse(text = par))
        object <<- ikedaSim(n = n, parms = parms, start = start, doplot = TRUE)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            n = 1000, 
            a = 0.4,
            b = 6.0,
            c = 0.9,
            start = "runif(2)",
            par = "par(mfrow=c(2,2))",
            object2x = FALSE,
            report = TRUE ),
        infoName = "Ikeda Map") 
}


# ------------------------------------------------------------------------------


.fSeries.ChaoticTimeSeries.logisticSim = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Logistic Map:
    myFunction = function(n, r, start, par, object2x, report) {
        n = as.integer(n)
        parms = c(r = as.numeric(r))
        start = eval(parse(text = start))
        eval(parse(text = par))
        object <<- logisticSim(n = n, parms = parms, start = start, 
            doplot = TRUE)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            n = 1000, 
            r = 4.0,
            start = "runif(1)",
            par = "par(mfrow=c(1,1))",
            object2x = FALSE,
            report = TRUE ),
        infoName = "Logistic Map" ) 
}


# ------------------------------------------------------------------------------


.fSeries.ChaoticTimeSeries.lorentzSim = 
function()
{   # A function implemented by Diethelm Wuertz

    # Lorentz Attractor:
    myFunction = function(times, sigma, r, b, start, par, object2x, report) {
        times = tkEval(times)
        parms = c(sigma = sigma, r = r, b = b)
        start = tkEval(start)
        eval(parse(text = par))
        object <<- lorentzSim(times = times, parms = parms, 
            start = start, doplot = TRUE)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            times = "seq(0, 20, by = 0.01)", 
            sigma = 16.0,
            r = 45.92,
            b = 4.0,
            start = "c(-14, -13, 47)",
            par = "par(mfrow=c(3,2))",
            object2x = FALSE,
            report = TRUE ),
        infoName = "Lorentz Attractor" )
}


# ------------------------------------------------------------------------------


.fSeries.ChaoticTimeSeries.roesslerSim = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Roessler Attractor:
    myFunction = function(times, a, b, c, start, par, object2x, report) {
        par(mfrow = c(3, 2), cex = 0.7)
        times = eval(parse(text = times))
        start = eval(parse(text = start))
        eval(parse(text = par))
        object <<- roesslerSim(times = times, parms = c(a = a, 
            b = b, c = c), start = start, doplot = TRUE)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            times = "seq(0,50,by=0.01)", 
            a = 0.2,
            b = 0.2,
            c = 8.0,
            start = "c(-1.894,-9.92,0.025)",
            par = "par(mfrow=c(3,2))",
            object2x = FALSE ),
        infoName = "Roessler Attractor" )
}


################################################################################
# Dependency Tests


.fSeries.TimeSeriesTests.1 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Example timeSeries: x - NYSE log Returns
    tkGetData(Data = "nyseDaily", infoName = "Daily NYSE Returns")
}
   

# ------------------------------------------------------------------------------

 
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
   

# ------------------------------------------------------------------------------


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


################################################################################
# Unit Root Distribution


.fSeries.UnitRootDistribution.1 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Output:
    output = capture.output(.unitrootSlider())
    tkTitle(What)
    tkOutput(output)
    tkDescription() 
}


################################################################################
# Unit Root Tests


.fSeries.UnitRootTests.1 = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # has Unit Root:
    x <<- rnorm(1000)
}
   

# ------------------------------------------------------------------------------

 
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
   

# ------------------------------------------------------------------------------

 
.fSeries.UnitRootTests.3 = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # ADF Unit Root Test
    output <<- capture.output(adfTest(x))
    tkOutput(output)
}
  
  
.fSeries.UnitRootTests.4 = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # McKinnon Unit Root Test
    output <<- capture.output(unitrootTest(x))
    tkOutput(output)
}


.fSeries.UnitRootTests.urersTest = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # ERS Unit Root Test:
    myFunction = function(series, type, model, lag.max, doplot, 
        object2x, report) {
        x = tkEval(series)
        type = tkSplit(type)
        print(type)
        object <<- urersTest(x, type, model, lag.max, doplot) 
        if (report) tkTitle("ERS Unit Root Test")
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x", 
            type = "DF-GLS | P-test", 
            model = "constant | trend",
            by = "day", 
            lag.max = 4, 
            format = "%Y-%m-%d", 
            doplot = TRUE,
            object2x = FALSE,
            report = TRUE),
        infoName = "ERS Unit Root Test" )          
}


################################################################################
# Heaviside Function


.fSeries.HeavisideFunction.1 = 
function(choice)
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Heaviside And Related Functions:
    .HeavisideSlider()
}


################################################################################
# Skew Normal|Student|GED Distributions


.fSeries.GarchDistributions.1 = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Generate Skew Normal Random Numbers
    .snormSlider(TRUE)
}


# ------------------------------------------------------------------------------


.fSeries.GarchDistributions.2 = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Skew Normal Distribution Slider
    .normSlider(TRUE)
}


# ------------------------------------------------------------------------------


.fSeries.GarchDistributions.3 = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Generate Skew Student-t Random Numbers
    .sstdSlider(TRUE)
}


# ------------------------------------------------------------------------------


.fSeries.GarchDistributions.4 = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Generate Student-t Distribution Slider
    .stdSlider(TRUE)
}


# ------------------------------------------------------------------------------


.fSeries.GarchDistributions.5 = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Generate Skew GED Random Numbers
    .sgedSlider(TRUE)
}


# ------------------------------------------------------------------------------


.fSeries.GarchDistributions.6 = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Generate GED Distribution Slider
    .gedSlider(TRUE)
}


################################################################################
# Garch Distribution Fits


.fSeries.GarchDistributionFits.1 = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # * Example timeSeries: x - NYSE log Returns
    tkGetData(Data = "nyseDaily", infoName = "Daily NYSE Returns")
}
   

# ------------------------------------------------------------------------------


.fSeries.GarchDistributionFits.2 = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Fit to Normal Distribution
    tkGetFit(normFit, "Fit to Normal Distribution")
}
   

.fSeries.GarchDistributionFits.3 = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Fit to Skew Normal Distribution
    tkGetFit(snormFit, "Fit to Skew Normal Distribution")
}
   

# ------------------------------------------------------------------------------


.fSeries.GarchDistributionFits.4 = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Fit to Sudent-t Distribution
    tkGetFit(stdFit, "Fit to Student-t Distribution")        
}
   

# ------------------------------------------------------------------------------


.fSeries.GarchDistributionFits.5 = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Fit Skew Sudent-t Distribution
    tkGetFit(sstdFit, "Fit to Skew Student-t Distribution")     
}
   

# ------------------------------------------------------------------------------


.fSeries.GarchDistributionFits.6 = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Fit GED Distribution
    tkGetFit(gedFit, "Fit to GED Distribution")     
}
   

.fSeries.GarchDistributionFits.7 = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Fit Skew GED Distribution
    tkGetFit(sgedFit, "Fit to Skew GED Distribution")   
}       
        
        
################################################################################
# fSeries Data

        
.fSeries.SeriesData.1 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # nyseres:
    tkGetData(Data = "nyseres", infoName = "Data Set")
}


# ------------------------------------------------------------------------------


.fSeries.SeriesData.2 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # recession:
    tkGetDataFrame(Data = "recession", infoName = "Data Set")
}


# ------------------------------------------------------------------------------


.fSeries.SeriesData.3 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # dem2gbp:
    tkGetData(Data = "dem2gbp", infoName = "Data Set")
}


# ------------------------------------------------------------------------------


.fSeries.SeriesData.4 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # cac40:
    tkGetDataFrame(Data = "cac40", infoName = "Data Set")
}


# ------------------------------------------------------------------------------


.fSeries.SeriesData.5 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # nelsonplosser:
    tkGetDataFrame(Data = "nelsonplosser", infoName = "Data Set")
}


# ------------------------------------------------------------------------------


.fSeries.SeriesData.6 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # surex1.ts:
    tkGetDataFrame(Data = "surex1.ts", infoName = "Data Set")
}


# ------------------------------------------------------------------------------


.fSeries.SeriesData.7 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # rf.30day:
    tkGetData(Data = "rf.30day", infoName = "Data Set")
}


# ------------------------------------------------------------------------------


.fSeries.SeriesData.8 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # black.ts:
    tkGetData(Data = "black.ts", infoName = "Data Set")
}


# ------------------------------------------------------------------------------


.fSeries.SeriesData.9 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # klein:
    tkGetData(Data = "klein", infoName = "Data Set")
}


# ------------------------------------------------------------------------------


.fSeries.SeriesData.10 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # kmenta:
    tkGetData(Data = "kmenta", infoName = "Data Set")
}


################################################################################
# Portable Innovations


.fSeries.PortableInnovations.runiflcg = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Uniform Innovations:
    myFunction = function(n, min, max, as.ts, object2x, report) {
        object <<- runif.lcg(n = n, min = min, max = max)
        if (as.ts) object <<- as.ts(object)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            n = 100, 
            min = 0,
            max = 1,
            as.ts = TRUE,
            object2x = FALS,
            report = TRUE ),
        infoName = "Portable Uniform Innovations" )
}
  

# ------------------------------------------------------------------------------


.fSeries.PortableInnovations.rnormlcg = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Normal Innovations:
    myFunction = function(n, mean, sd, as.ts, object2x, report) {
       object <<- rnorm.lcg(n = n, mean = mean, sd = sd)
       if (as.ts) object <<- as.ts(object)
       object }
    tkExecute(
        fun = myFunction,
        params = list(
            n = 100, 
            mean = 0,
            sd = 1,
            as.ts = TRUE,
            object2x = FALSE,
            report = TRUE ),
        infoName = "Portable Normal Innovations" )
}
  

# ------------------------------------------------------------------------------


.fSeries.PortableInnovations.rtlcg = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Student-t Innovations:
    myFunction = function(n, df, as.ts, object2x, report) {
        object <<- rt.lcg(n = n, df = df)
        if (as.ts) object <<- as.ts(object)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            n = 100, 
            df = 4,
            as.ts = TRUE, 
            object2x = FALSE,
            report = TRUE),
        infoName = "Portable Student-t Innovations" )
}
   
    
################################################################################
# Mills Data


.fSeries.MillsData.RS = 
function() 
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # RS - Monthly 91 Day Treasury Bill Rate:
    tkGetData(Data = "RS", infoName = "Monthly 91 Day Treasury Bill Rate")
}


# ------------------------------------------------------------------------------


.fSeries.MillsData.R20 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # R20 - Monthly Yield on 20 Year UK Gilts:
    tkGetData(Data = "R20", infoName = "Monthly Yield on 20 Year UK Gilts")
}


# ------------------------------------------------------------------------------


.fSeries.MillsData.RSQ = 
function() 
{   # A function implemented by Diethelm Wuertz

    # RSQ - Quarterly 91 Day Treasury Bill Rate:
    tkGetData(Data = "RSQ", infoName = "Quarterly 91 Day Treasury Bill Rate")
}


# ------------------------------------------------------------------------------


.fSeries.MillsData.R20Q = 
function() 
{   # A function implemented by Diethelm Wuertz

    # R20Q - Quarterly Yield on 20 Year UK Gilts:
    tkGetData(Data = "R20Q", infoName = "Quarterly Yield on 20 Year UK Gilts")
}
 

# ------------------------------------------------------------------------------


.fSeries.MillsData.RSQREAL = 
function() 
{   # A function implemented by Diethelm Wuertz

    # RSQREAL - Quarterly Real 91 Day Treasury Bill:
    tkGetData(Data = "RSQREAL", infoName = "Quarterly Real 91 Day TBill")
}


# ------------------------------------------------------------------------------


.fSeries.MillsData.FTAPRICE = 
function() 
{   # A function implemented by Diethelm Wuertz

    # FTAPRICE - FTA All Share Price Index:
    tkGetData(Data = "FTAPRICE", infoName = "FTA All Share Price Index")
}


# ------------------------------------------------------------------------------


.fSeries.MillsData.FTADIV = 
function() 
{   # A function implemented by Diethelm Wuertz

    # FTADIV - FTA All Share Dividend Index:
    tkGetData(Data = "FTADIV", infoName = "FTA All Share Dividend Index")
}


# ------------------------------------------------------------------------------


.fSeries.MillsData.FTARET = 
function() 
{   # A function implemented by Diethelm Wuertz

    # FTARET - FTA All Share Nominal Returns:
    tkGetData(Data = "FTARET", infoName = "FTA All Share Nominal Returns")
}


# ------------------------------------------------------------------------------


.fSeries.MillsData.RPI = 
function() 
{   # A function implemented by Diethelm Wuertz

    # RPI - UK Retail Price Index:
    tkGetData(Data = "RPI", infoName = "UK Retail Price Index")
}


# ------------------------------------------------------------------------------


.fSeries.MillsData.EXCHD = 
function() 
{   # A function implemented by Diethelm Wuertz

    # EXCHD - Dollar/Sterling Exchange Rate:
    tkGetData(Data = "EXCHD", infoName = "")
}


# ------------------------------------------------------------------------------


.fSeries.MillsData.EXCHQ = 
function() 
{   # A function implemented by Diethelm Wuertz

    # EXCHQ - Dollar/Sterling Exchange Rate:
    tkGetData(Data = "EXCHQ", infoName = "Dollar/Sterling Exchange Rate")
}


# ------------------------------------------------------------------------------


.fSeries.MillsData.SP500 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # SP500 - SP 500 Annual Data Index:
    tkGetData(Data = "SP500", infoName = "")
}


# ------------------------------------------------------------------------------


.fSeries.MillsData.SP500R = 
function() 
{   # A function implemented by Diethelm Wuertz

    # SP500R - SP 500 Real Returns:
    tkGetData(Data = "SP500R", infoName = "SP 500 Annual Data Index")
}


# ------------------------------------------------------------------------------


.fSeries.MillsData.SP500D = 
function() 
{   # A function implemented by Diethelm Wuertz

    # SP500D - SP 500 Daily Data Index:
    tkGetData(Data = "SP500D", infoName = "SP 500 Daily Data Index")
}


# ------------------------------------------------------------------------------


.fSeries.MillsData.FT30 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # FT30 - Financial Times FT 30 Index:
    tkGetData(Data = "FT30", infoName = "SP 500 Daily Data Index")
}


# ------------------------------------------------------------------------------


.fSeries.MillsData.FTSE100 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # FTSE100 - FTSE 100 Index:
    tkGetData(Data = "FTSE100", infoName = "FTSE 100 Index")
}


# ------------------------------------------------------------------------------


.fSeries.MillsData.CTLD = 
function() 
{   # A function implemented by Diethelm Wuertz

    # CTLD - Courtaulds Share Price:
    tkGetData(Data = "CTLD", infoName = "Courtaulds Share Price")
}


# ------------------------------------------------------------------------------


.fSeries.MillsData.LGEN = 
function() 
{   # A function implemented by Diethelm Wuertz

    # LGEN - Legal and General Share Price:
    tkGetData(Data = "LGEN", infoName = "Legal and General Share Price")
}


# ------------------------------------------------------------------------------


.fSeries.MillsData.PRU = 
function() 
{   # A function implemented by Diethelm Wuertz

    # PRU - Prudential Share Price:
    tkGetData(Data = "PRU", infoName = "Prudential Share Price")
}


################################################################################

