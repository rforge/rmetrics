
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


.fSeries.ArmaModelling.armaSim = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Simulate ARMA Process:
    helpTopic <<- "armaSim"
    myFunction = function(ar, d, ma, n, object2x, report) {
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
        prototypes = list(
            ar = "0.5, -0.5", 
            d = "0",
            ma = "0.8",
            n = 100,
            object2x = TRUE,
            report = FALSE),
        subject = "Simulated ARMA" )
}


# ------------------------------------------------------------------------------


.fSeries.ArmaModelling.armaFit = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Fit ARMA Process:
    helpTopic <<- "armaFit"
    myFunction = function(formula, method, include.mean, doplot, 
        par, object2x, report) {
        formula = as.formula(formula)
        include.mean = as.logical(include.mean)
        method = tkSplit(method)
        object <<- armaFit(formula = formula, method = method, 
            include.mean = include.mean, fixed = NULL,
            fracdiff.M = 100, fracdiff.h = -1)
        if (doplot) {
            eval(parse(text = par))
            summary(object)
        }
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            formula = "x ~ arima(2, 0, 1)", 
            method = "CSS-ML & MLE & CSS",
            include.mean = TRUE,
            doplot = TRUE,
            par = "par(mfrow=c(2,2),cex=0.7)",
            object2x = FALSE,
            report = TRUE ),
        subject = "Fitted ARMA")  
}


# ------------------------------------------------------------------------------


.fSeries.ArmaModelling.predict = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # ARMA Forecast:
    helpTopic <<- "armaFit"
    myFunction = function(fit, n.ahead, n.back, conf, doplot, par,
        object2x, report) {
        fit = eval(parse(text = as.character(fit)))
        conf = eval(parse(text = conf))
        if (doplot) tkEval(par)
        object@predicted.values <<- predict.fARMA(object = fit, 
            n.ahead = n.ahead, n.back = n.back, conf = conf, 
            doplot = TRUE, doprint = FALSE) 
        if (report) tkTitle("ARMA Prediction")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            fit = "object",
            n.ahead = 10,
            n.back = 50,
            conf = "c(80, 95)",
            doplot = TRUE,
            par = "par(mfrow = c(1,1))",
            object2x = FALSE,
            report = TRUE ),
        subject = "Predicted ARMA" )  
}
    
   
################################################################################
# Garch Modelling


.fSeries.GarchModelling.garchSim = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Simulate GARCH Process:
    helpTopic <<- ""
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
        prototypes = list(
            model = "list(omega=1e-06,alpha=0.1,beta=0.8)",
            n = 100,
            n.start = 100,
            presample = "NULL",
            cond.dist = "rnorm & rged & rstd & rsnorm & rsged & rsstd",
            rseed= "NULL",
            object2x = TRUE,
            report = TRUE ),
        subject = "Simulated GARCH" )
}


# ------------------------------------------------------------------------------


.fSeries.GarchModelling.garchFit = 
function()
{   # A function implemented by Diethelm Wuertz

    # Fit GARCH Process:
    helpTopic <<- "garchFit"
    myFunction = function(formula.mean, formula.var, series, init.rec, 
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
            control) 
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
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
        subject = "Fitted GARCH")  
}


# ------------------------------------------------------------------------------


.fSeries.GarchModelling.predict = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # GARCH Forecast:
    helpTopic <<- "garchFit"
    myFunction = function(fit, n.ahead, object2x, report) {
        fit = eval(parse(text = as.character(fit)))
        object <<- predict(object = fit, n.ahead = n.ahead, trace = FALSE)
        if (report) tkTitle("GARCH Prediction")
        report <<- FALSE
        object}
    tkExecute(
        fun = myFunction,
        prototypes = list(
            fit = "object",
            n.ahead = 10,
            object2x = FALSE,
            report = TRUE ),
        subject = "Predicted GARCH" )  
}


# ------------------------------------------------------------------------------


.fSeries.GarchDistributions.conddistSlider = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # GARCH Conditional Distribution Sliders:
    helpTopic <<- ""
    myFunction = function(slider, object2x, report) {
        slider = tkSplit(slider)
        if (slider == "t") slider = "std"
        fun = match.fun(paste(".s", slider, "Slider", sep = ""))
        object <<- fun() 
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            slider = "norm & t & ged",
            object2x = FALSE,
            report = FALSE ),
        subject = "GARCH Sliders" ) 
}


# ------------------------------------------------------------------------------


.fSeries.GarchDistributions.conddistFit = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # GARCH Conditional Distribution Fit:
    helpTopic <<- ""
    myFunction = function(series, cond.dist, skew, object2x, report) {
        x = tkEval(series)
        cond.dist = tkSplit(cond.dist)
        if (cond.dist == "t") cond.dist = "std"
        if (skew) cond.dist = paste("s", cond.dist, sep = "")
        fun = match.fun(paste(cond.dist, "Fit", sep = ""))
        if (report) tkTitle("GARCH Conditional Distribution Fit")
        object <<- fun(x) 
        object$cond.dist = cond.dist
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            cond.dist = "norm & t & ged",
            skew = FALSE,
            object2x = FALSE,
            report = TRUE ),
        subject = "Conditional Distribution Fit" ) 
}


################################################################################
# Long Memory Modelling


.fSeries.LongMemoryModelling.fgnSim = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Fractional Gaussian Noise Simulation:
    helpTopic <<- ""
    myFunction = function(n, H, method, object2x, report) {
        method = tkSplit(method) 
        object <<- fgnSim(n, H, method) 
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            n = 100,
            H = 0.7,
            method = "beran & durbin & paxson",
            object2x = TRUE,
            report = FALSE ),
        subject = "Simulated Fractional Gaussian Noise" )  
} 


# ------------------------------------------------------------------------------


.fSeries.LongMemoryModelling.fbmSim = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Fractional Brownian Motion Simulation:
    helpTopic <<- ""
    myFunction = function(n, H, method, object2x, report) {
        method = tkSplit(method) 
        object <<- fbmSim(n, H, method, waveJ, doplot = TRUE, fgn) 
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            n = 100,
            H = 0.7,
            method = "mvn & chol & lev & circ & wave",
            waveJ = 7,
            fgn = FALSE,
            object2x = TRUE,
            report = FALSE ),
        subject = "Simulated Fractional Brownian Motion" )  
} 


################################################################################
# Chaotic Time Series


.fSeries.ChaoticTimeSeries.henonSim = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Henon Map:
    helpTopic <<- ""
    myFunction = function(n, a, b, start, par, oject2x, report) {
        parms = c(a = a, b = b)
        if (!is.numeric(start)) start = eval(parse(text = start))
        eval(parse(text = par))
        object <<- henonSim(n = n, parms = parms, start = start, 
            doplot = TRUE)
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            n = 1000, 
            a = 1.4,
            b = 0.3,
            start = "runif(2)",
            par = "par(mfrow=c(1,1))",
            object2x = FALSE,
            report = FALSE ),
        subject = "Henon Map" ) 
}


# ------------------------------------------------------------------------------


.fSeries.ChaoticTimeSeries.ikedaSim = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Ikeda Map:
    helpTopic <<- ""
    myFunction = function(n, a, b, start, par, object2x, report) {
        n = as.integer(n)
        parms = c(a = a, b = b, c = c)
        start = eval(parse(text = start))
        eval(parse(text = par))
        object <<- ikedaSim(n = n, parms = parms, start = start, doplot = TRUE)
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            n = 1000, 
            a = 0.4,
            b = 6.0,
            c = 0.9,
            start = "runif(2)",
            par = "par(mfrow=c(2,2))",
            object2x = FALSE,
            report = FALSE ),
        subject = "Ikeda Map") 
}


# ------------------------------------------------------------------------------


.fSeries.ChaoticTimeSeries.logisticSim = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Logistic Map:
    helpTopic <<- ""
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
        prototypes = list(
            n = 1000, 
            r = 4.0,
            start = "runif(1)",
            par = "par(mfrow=c(1,1))",
            object2x = FALSE,
            report = FALSE ),
        subject = "Logistic Map" ) 
}


# ------------------------------------------------------------------------------


.fSeries.ChaoticTimeSeries.lorentzSim = 
function()
{   # A function implemented by Diethelm Wuertz

    # Lorentz Attractor:
    helpTopic <<- ""
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
        prototypes = list(
            times = "seq(0, 20, by = 0.01)", 
            sigma = 16.0,
            r = 45.92,
            b = 4.0,
            start = "c(-14, -13, 47)",
            par = "par(mfrow=c(3,2))",
            object2x = FALSE,
            report = FALSE ),
        subject = "Lorentz Attractor" )
}


# ------------------------------------------------------------------------------


.fSeries.ChaoticTimeSeries.roesslerSim = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Roessler Attractor:
    helpTopic <<- ""
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
        prototypes = list(
            times = "seq(0,50,by=0.01)", 
            a = 0.2,
            b = 0.2,
            c = 8.0,
            start = "c(-1.894,-9.92,0.025)",
            par = "par(mfrow=c(3,2))",
            object2x = FALSE,
            report = FALSE ),
        subject = "Roessler Attractor" )
}


################################################################################
# Dependency Tests


.fSeries.TimeSeriesTests.runsTest = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Runs Test:
    helpTopic <<- "runsTest"
    myFunction = function(series, object2x, report) {
        x = tkEval(series)
        object <<- runsTest(x = x) 
        if (report) tkTitle("Runs Test")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            object2x = FALSE,
            report = TRUE ),
        subject = "Runs Test" )
}


.fSeries.TimeSeriesTests.bdsTest = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # BDS Test:
    helpTopic <<- "bdsTest"
    myFunction = function(m, eps, object2x, report) {
        m = as.integer(m)
        if (eps == "NULL") eps = NULL
        object <<- bdsTest(x = x, m = m, eps = eps) 
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            m = 3,
            eps = "NULL",
            object2x = FALSE,
            report = TRUE ),
        subject = "BDS Test" )
}
  
  
.fSeries.TimeSeriesTests.tnnTest = 
function()
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Teraesvirta NN Test:
    helpTopic <<- "tnnTest"
    myFunction = function(lag, object2x, report) {
        lag = as.integer(lag)
        object <<- tnnTest(x = x, lag = lag) 
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            lag = "1",
            object2x = FALSE,
            report = TRUE ),
        subject = "Teraesvirta NN Test" )
}
   

# ------------------------------------------------------------------------------


.fSeries.TimeSeriesTests.wnnTest = 
function()
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # White NN Test:
    helpTopic <<- "wnnTest"
    myFunction = function(lag, qstar, q, range, object2x, report) {
        lag = as.integer(lag)
        qstar = as.integer(qstar)
        q = as.integer(q)
        range = as.integer(range)
        object <<- wnnTest(x = x, lag = lag, qstar = qstar, q = q, 
            range = range) 
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            lag = 1,
            qstar = 2,
            q = 10,
            range = 4,
            object2x = FALSE,
            report = TRUE ),
        subject = "White NN Test" ) 
}


################################################################################
# Unit Root Distribution

.fSeries.UnitRootDistribution.1 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Output:
    helpTopic <<- ""
    output = capture.output(.unitrootSlider())
    tkTitle(What)
    tkOutput(output)
    tkDescription() 
}


################################################################################
# Unit Root Tests


.fSeries.UnitRootTests.hasUnitRoot = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Has Unit Root:
    helpTopic <<- ""
    myFunction = function(n, as.ts, object2x, report) {
        ans = rnorm(n)
        if (as.ts) ans = as.ts(ans)
        object <<- ans
        if (report) tkTitle("Has No Unit Root: rnorm(1000)")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            n = 100,
            as.ts = FALSE,
            object2x = TRUE,
            report = FALSE),
        subject = "Data Set has Unit Root" )     
}
   

# ------------------------------------------------------------------------------

 
.fSeries.UnitRootTests.hasNoUnitRoot = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Has No Unit Root:
    helpTopic <<- ""
    myFunction = function(n, as.ts, object2x, report) {
        ans = cumsum(c(0, rnorm(n))) 
        if (as.ts) ans = as.ts(ans)
        object <<- ans 
        if (report) tkTitle("Has No Unit Root: cumsum(c(0, rnorm(1000)))")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            n = 100,
            as.ts = TRUE,
            object2x = TRUE,
            report = FALSE),
        subject = "Data Set has no Unit Root" )        
}
   

# ------------------------------------------------------------------------------

 
.fSeries.UnitRootTests.adfTest = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # ADF Unit Root Test:
    helpTopic <<- ""
    myFunction = function(series, lags, type, object2x, report) {
        x = tkEval(series)
        type = tkSplit(type)
        object <<- adfTest(x, lags, type) 
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            lags = 1,
            type = "nc & c & ct", 
            object2x = FALSE,
            report = TRUE ),
        subject = "ADF Unit Root Test" ) 
}


# ------------------------------------------------------------------------------
  
  
.fSeries.UnitRootTests.unitrootTest = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # McKinnon ADF Unit Root Test:
    helpTopic <<- ""
    myFunction = function(series, lags, type, object2x, report) {
        x = tkEval(series)
        type = tkSplit(type)
        object <<- unitrootTest(x, lags, type) 
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            lags = 1,
            type = "nc & c & ct", 
            object2x = FALSE,
            report = TRUE ),
        subject = "McKinnon Unit Root Test" ) 
}


# ------------------------------------------------------------------------------


.fSeries.UnitRootTests.urca =
function()
{   # A function implemented by Diethelm Wuertz

    # Return Value:
    require(urca)
}


# ------------------------------------------------------------------------------


.fSeries.UnitRootTests.urersTest = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Elliott-Rothenberg-Stock Unit Root Test:
    helpTopic <<- ""
    myFunction = function(series, type, model, lag.max, doplot, 
        object2x, report) {
        x = tkEval(series)
        type = tkSplit(type)
        model = tkSplit(model)
        object <<- urersTest(x, type, model, lag.max, doplot) 
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x", 
            type = "DF-GLS & P-test", 
            model = "constant & trend",
            lag.max = 4, 
            doplot = TRUE,
            object2x = FALSE,
            report = TRUE),
        subject = "ERS Unit Root Test" )          
}


# ------------------------------------------------------------------------------


.fSeries.UnitRootTests.urkpssTest =
function()
{   # A function implemented by Diethelm Wuertz
    
    # "KPSS Unit Root Test
    helpTopic <<- ""
    myFunction = function(series, lags, type, doplot, object2x, report) {
        x = tkEval(series)
        type = tkSplit(type)
        lags = tkSplit(lags)
        object <<- urkpssTest(x, type, lags, use.lag = NULL, doplot) 
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            type = "mu & tau",
            lags = "short & long & nil",
            doplot = TRUE,
            object2x = FALSE,
            report = TRUE ),
        subject = "KPSS Unit Root Test" ) 
}


# ------------------------------------------------------------------------------


.fSeries.UnitRootTests.urppTest = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Phillips-Perron Unit Root Test:
    helpTopic <<- ""
    myFunction = function(series, lags, type, object2x, report) {
        x = tkEval(series)
        type = tkSplit(type)
        model = tkSplit(model)
        lags = tkSplit(lags)
        object <<- urppTest(x, type, model, lags, use.lag = NULL, doplot) 
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            type = "Z-alpha & Z-tau", 
            model = "constant & trend",
            lags = "short & long",
            doplot = TRUE,
            object2x = FALSE,
            report = TRUE ),
        subject = "Phillips-Perron Unit Root Test" ) 
}


# ------------------------------------------------------------------------------


.fSeries.UnitRootTests.urspTest = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Schmidt-Phillips Unit Root Test:
    helpTopic <<- ""
    myFunction = function(series, type, pol.deg, signif, doplot, 
        object2x, report) {
        x = tkEval(series)
        type = tkSplit(type)
        pol.deg = tkEval(pol.deg)
        signif = tkEval(signif)
        object <<- urspTest(x, type, pol.deg, signif, doplot) 
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            type = "tau & rho", 
            pol.deg = "c(1,2,3,4)",
            signif = "c(0.01,0.05,0.1)",
            doplot = TRUE,
            object2x = FALSE,
            report = TRUE ),
        subject = "Schmidt-Phillips Unit Root Test" ) 
}


# ------------------------------------------------------------------------------
    
.fSeries.UnitRootTests.urzaTest =
function()
{   # A function implemented by Diethelm Wuertz
    
    # Zivot & Andrews Unit Root Test:
    helpTopic <<- ""
    myFunction = function(series, model, lag, doplot, object2x, report) {
        x = tkEval(series)
        model = tkSplit(model)
        object <<- urzaTest(x, model, lag, doplot) 
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            model = "intercept & trend & both",
            lag = 2,
            doplot = TRUE,
            object2x = FALSE,
            report = TRUE ),
        subject = "Zivot & Andrews Unit Root Test" ) 
}

        
################################################################################
# fSeries Data

        
.fSeries.SeriesData.1 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # nyseres:
    .tkGetData(Data = "nyseres", subject = "Data Set")
}


# ------------------------------------------------------------------------------


.fSeries.SeriesData.2 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # recession:
    ..tkGetDataFrame(Data = "recession", subject = "Data Set")
}


# ------------------------------------------------------------------------------


.fSeries.SeriesData.3 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # dem2gbp:
    .tkGetData(Data = "dem2gbp", subject = "Data Set")
}


# ------------------------------------------------------------------------------


.fSeries.SeriesData.4 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # cac40:
    ..tkGetDataFrame(Data = "cac40", subject = "Data Set")
}


# ------------------------------------------------------------------------------


.fSeries.SeriesData.5 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # nelsonplosser:
    ..tkGetDataFrame(Data = "nelsonplosser", subject = "Data Set")
}


# ------------------------------------------------------------------------------


.fSeries.SeriesData.6 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # surex1.ts:
    ..tkGetDataFrame(Data = "surex1.ts", subject = "Data Set")
}


# ------------------------------------------------------------------------------


.fSeries.SeriesData.7 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # rf.30day:
    .tkGetData(Data = "rf.30day", subject = "Data Set")
}


# ------------------------------------------------------------------------------


.fSeries.SeriesData.8 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # black.ts:
    .tkGetData(Data = "black.ts", subject = "Data Set")
}


# ------------------------------------------------------------------------------


.fSeries.SeriesData.9 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # klein:
    .tkGetData(Data = "klein", subject = "Data Set")
}


# ------------------------------------------------------------------------------


.fSeries.SeriesData.10 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # kmenta:
    .tkGetData(Data = "kmenta", subject = "Data Set")
}


################################################################################
# Portable Innovations


.fSeries.PortableInnovations.runiflcg = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Uniform Innovations:
    helpTopic <<- ""
    myFunction = function(n, min, max, as.ts, object2x, report) {
        object <<- runif.lcg(n = n, min = min, max = max)
        if (as.ts) object <<- as.ts(object)
        attr(object, "control") = c(min = min, max = max)
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            n = 100, 
            min = 0,
            max = 1,
            as.ts = TRUE,
            object2x = FALSE,
            report = TRUE ),
        subject = "Portable Uniform Innovations" )
}
  

# ------------------------------------------------------------------------------


.fSeries.PortableInnovations.rnormlcg = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Normal Innovations:
    helpTopic <<- ""
    myFunction = function(n, mean, sd, as.ts, object2x, report) {
       object <<- rnorm.lcg(n = n, mean = mean, sd = sd)
       if (as.ts) object <<- as.ts(object)
       attr(object, "control") = c(mean = mean, sd = sd)
       object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            n = 100, 
            mean = 0,
            sd = 1,
            as.ts = TRUE,
            object2x = FALSE,
            report = TRUE ),
        subject = "Portable Normal Innovations" )
}
  

# ------------------------------------------------------------------------------


.fSeries.PortableInnovations.rtlcg = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Student-t Innovations:
    helpTopic <<- ""
    myFunction = function(n, df, as.ts, object2x, report) {
        object <<- rt.lcg(n = n, df = df)
        if (as.ts) object <<- as.ts(object)
        attr(object, "control") = c(df = df)
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            n = 100, 
            df = 4,
            as.ts = TRUE, 
            object2x = FALSE,
            report = TRUE),
        subject = "Portable Student-t Innovations" )
}
   
    
################################################################################
# Mills Data


.fSeries.MillsData.RS = 
function() 
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # RS - Monthly 91 Day Treasury Bill Rate:
    .tkGetData(Data = "RS", subject = "91Day M-TBills")
}


# ------------------------------------------------------------------------------


.fSeries.MillsData.R20 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # R20 - Monthly Yield on 20 Year UK Gilts:
    .tkGetData(Data = "R20", subject = "20Y Gilts M-Yield")
}


# ------------------------------------------------------------------------------


.fSeries.MillsData.RSQ = 
function() 
{   # A function implemented by Diethelm Wuertz

    # RSQ - Quarterly 91 Day Treasury Bill Rate:
    .tkGetData(Data = "RSQ", subject = "91Day Q-TBills")
}


# ------------------------------------------------------------------------------


.fSeries.MillsData.R20Q = 
function() 
{   # A function implemented by Diethelm Wuertz

    # R20Q - Quarterly Yield on 20 Year UK Gilts:
    .tkGetData(Data = "R20Q", subject = "20Y Gilts Q-Yield")
}
 

# ------------------------------------------------------------------------------


.fSeries.MillsData.RSQREAL = 
function() 
{   # A function implemented by Diethelm Wuertz

    # RSQREAL - Quarterly Real 91 Day Treasury Bill:
    .tkGetData(Data = "RSQREAL", subject = "91Day TBill Q-Real")
}


# ------------------------------------------------------------------------------


.fSeries.MillsData.FTAPRICE = 
function() 
{   # A function implemented by Diethelm Wuertz

    # FTAPRICE - FTA All Share Price Index:
    .tkGetData(Data = "FTAPRICE", subject = "FTA AS Price")
}


# ------------------------------------------------------------------------------


.fSeries.MillsData.FTADIV = 
function() 
{   # A function implemented by Diethelm Wuertz

    # FTADIV - FTA All Share Dividend Index:
    .tkGetData(Data = "FTADIV", subject = "FTA AS DivIndex")
}


# ------------------------------------------------------------------------------


.fSeries.MillsData.FTARET = 
function() 
{   # A function implemented by Diethelm Wuertz

    # FTARET - FTA All Share Nominal Returns:
    .tkGetData(Data = "FTARET", subject = "FTA AS NomRets")
}


# ------------------------------------------------------------------------------


.fSeries.MillsData.RPI = 
function() 
{   # A function implemented by Diethelm Wuertz

    # RPI - UK Retail Price Index:
    .tkGetData(Data = "RPI", subject = "UK Retail PI")
}


# ------------------------------------------------------------------------------


.fSeries.MillsData.EXCHD = 
function() 
{   # A function implemented by Diethelm Wuertz

    # EXCHD - Dollar/Sterling Exchange Rate:
    .tkGetData(Data = "EXCHD", subject = "USDGBP D-FX")
}


# ------------------------------------------------------------------------------


.fSeries.MillsData.EXCHQ = 
function() 
{   # A function implemented by Diethelm Wuertz

    # EXCHQ - Dollar/Sterling Exchange Rate:
    .tkGetData(Data = "EXCHQ", subject = "USDGBP Q-FX")
}


# ------------------------------------------------------------------------------


.fSeries.MillsData.SP500 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # SP500 - SP 500 Annual Data Index:
    .tkGetData(Data = "SP500", subject = "")
}


# ------------------------------------------------------------------------------


.fSeries.MillsData.SP500R = 
function() 
{   # A function implemented by Diethelm Wuertz

    # SP500R - SP 500 Real Returns:
    .tkGetData(Data = "SP500R", subject = "SP500Ann Index")
}


# ------------------------------------------------------------------------------


.fSeries.MillsData.SP500D = 
function() 
{   # A function implemented by Diethelm Wuertz

    # SP500D - SP 500 Daily Data Index:
    .tkGetData(Data = "SP500D", subject = "SP500Daily Index")
}


# ------------------------------------------------------------------------------


.fSeries.MillsData.FT30 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # FT30 - Financial Times FT 30 Index:
    .tkGetData(Data = "FT30", subject = "FT30Daily Index")
}


# ------------------------------------------------------------------------------


.fSeries.MillsData.FTSE100 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # FTSE100 - FTSE 100 Index:
    .tkGetData(Data = "FTSE100", subject = "FTSE100 Index")
}


# ------------------------------------------------------------------------------


.fSeries.MillsData.CTLD = 
function() 
{   # A function implemented by Diethelm Wuertz

    # CTLD - Courtaulds Share Price:
    .tkGetData(Data = "CTLD", subject = "Courtaulds SP")
}


# ------------------------------------------------------------------------------


.fSeries.MillsData.LGEN = 
function() 
{   # A function implemented by Diethelm Wuertz

    # LGEN - Legal and General Share Price:
    .tkGetData(Data = "LGEN", subject = "Legal&General SP")
}


# ------------------------------------------------------------------------------


.fSeries.MillsData.PRU = 
function() 
{   # A function implemented by Diethelm Wuertz

    # PRU - Prudential Share Price:
    .tkGetData(Data = "PRU", subject = "Prudential SP")
}


################################################################################

