#
# Examples:
#   A Compendium for R and Rmetrics users to the book 
#     "Modeling Financial Time Series with S-Plus" 
#     written by E. Zivot and J. Wang
#   ISBN 0-387-95549-6
#
# Description:
#   See Chapter 3 of the book.
#
# Notes:
#   This is not a COPY of the S-Plus "example.ssc" files accompanying the
#     book of Zivot and Wang. It is worth to note that this file contents a 
#     new implementation of the examples tailored to Rmetrics based on R.
#
# Author:
#   (C) Rmetrics 1997-2005, Diethelm Wuertz, GPL
# 	  www.rmetrics.org
# 	  www.itp.phys.ethz.ch
# 	  www.finance.ch
#


################################################################################
## Chapter 3 - Overview 


    # Zivot and Wang:
    #   "This chapter provides background material on time series 
    #   concepts that are used throughout the book. These concepts 
    #   are presented in an informal way, and extensive examples 
    #   using S-PLUS are used to build intuition. Section 3.2 
    #   discusses time series concepts for stationary and ergodic 
    #   univariate time series. Topics include testing for white 
    #   noise, linear and autoregressive moving average (ARMA) process, 
    #   estimation and forecasting from ARMA models, and long-run 
    #   variance estimation. Section 3.3 introduces univariate
    #   nonstationary time series and defines the important concepts 
    #   of I(0) and I(1) time series. Section 3.4 explains univariate 
    #   long memory time series. Section 3.5 covers concepts for 
    #   stationary and ergodic multivariate time series, introduces 
    #   the class of vector autoregression models, and discusses
    #   long-run variance estimation."
    ###
    
    
################################################################################
# Section 3.2 - Univariate Time Series Import


    # Chapter 3.2 - Load Data Sets:
    # Where are the unctions and Data?
    require(fBasics)
    require(fSeries)
    ###

    
    # IMPORTANT:
    # Remember, the data slot for 'timeDate' and 'timeSeries' objects 
    # is named here "@Data" and not "@.Data" what is used by S-Plus!
    ###
    
    
    # Example 1
    # Gaussian White Noise Processes ...
    # p.58
    set.seed(101)               
    y = rnorm(n = 100, sd = 1)
    # Estimate Sample Moments:
    y.bar = mean(y)
    g.hat = acf(y, lag.max = 10, type = "covariance", plot = FALSE)
    g.hat
    r.hat = acf(y, lag.max = 10, type = "correlation", plot = FALSE)    
    r.hat
    # Figure 3.1:
    # Plot Data and SACF 
    par(mfrow = c(2, 2), cex = 0.7)
    ts.plot(y, ylab = "y", main = "Series y", col = "steelblue4")
    grid()
    r.hat = acf(y, lag.max = 10, type = "correlation")  
    ###

    
    # Example 2
    # Testing for Normality ...
    # p. 60
    # Figure 3.2:
    par(mfrow = c(2, 1), cex = 0.7)
    qqnorm(y)
    qqline(y)
    # Normality tests
    normalTest(y, method = "sw")
    normalTest(y, method = "jb")
    ###

      
    # The file "DowJones30.csv" contains closing prices for 30 stocks
    # represented in the Dow Jones Industrial Average Index. 
    # Data are downloadable and can be updated from Yahoo's web site.   
    data(DowJones30)
    head(DowJones30)
    class(DowJones30)
    # Transform 'data.frame' to 'timSeries' object ...
    DowJones30 = as.timeSeries(DowJones30)
    class(DowJones30)
    c(start(DowJones30), end(DowJones30))
    ###
    
    
    # Example 3
    # Daily Returns on Microsoft ...
    # p.62
    # Note, Rmetrics uses for the computation of returns a function
    #   named 'returnSeries', this is in the tradition of the 
    #   other '*Series' functions ...
    r.msft = returnSeries(DowJones30[, "MSFT"], type = "continuous")
    r.msft@title = "Daily returns on Microsoft"
    c(start(r.msft), end(r.msft))
    # Rmetrics has a function to cut out a piece from a 'timeSeries'
    r.msft.2000 = cutSeries(r.msft, "2000-01-01", "2000-12-31")
    # Figure - 3.3:
    par(mfrow = c(3, 2), cex = 0.7)
    plot(r.msft.2000, type = "l", ylab = "r.msft", main = "MSFT",
        col = "steelblue4")
    abline(h = 0, lty = 3)
    r.acf = acf(seriesData(r.msft.2000), col = "steelblue4")
    hist(seriesData(r.msft), col = "steelblue4", border = "white")          
    qqnorm(seriesData(r.msft), col = "steelblue4")
    ###
    
    
    # Histogram/Quantile [sorry no Trellis] Plot:
    # p. 63
    # Note, Trellis plots are not yet available in Rmetrics
    #   Use alternatively the traditional plots ...
    par(mfrow = c(3, 2), cex = 0.7)
    hist(seriesData(r.msft), main = "MSFT monthly return",
        col = "steelblue4", border = "white")
    # Note, Rmetrics has a function 'histPlot' which also supports
    #   a 'timeSeries' object as input ...
    args(histPlot)
    histPlot(r.msft, main = "MSFT monthly return")
    # Quantile-Quantiles [Trellis] Plot:
    qqnorm(seriesData(r.msft), main = "MSFT monthly return",
        col = "steelblue4")
    # Add Line:
    qqline(seriesData(r.msft))
    # Note, Rmetrics has a function 'qqgaussPlot' which also supports
    #   a 'timeSeries' object as input ...
    #   [The function produces a normalized qqnorm plot]
    args(qqgaussPlot)
    qqgaussPlot(r.msft, span = 8, main = "MSFT standardized return")
    ###
    
    
    # Test for white noise using [autocorTest]:
    # p. 63
    # Note, Rmetrics has no function 'autocorTest' which performs
    #   the Box-Pierce and Ljung-Box test. Let us write one ...
    autocorTest = function(x, ...) UseMethod("autocorTest")
    autocorTest.default = 
        function(x, lag.n = 1, method = c("lb", "bp"), na.rm = FALSE) {
            x = as.vector(as.matrix(x)[,1])
            if (na.rm) x = na.omit(x)
            type = c(bp = "Box-Pierce", lb = "Ljung-Box") 
            type = type[method[1]]
            # Use Box.test() from 'stats'
            Box.test(x = x, lag = lag.n, type = type[method[1]]) }
    # Show Arguments:
    args(autocorTest.default)
    autocorTest(r.msft, lag.n = 10, method = "lb")
    # Or use directly ...
    Box.test(seriesData(r.msft), lag = 10, type = "Ljung-Box")
    ###
    

################################################################################
# Section 3.2.3 - Autoregressive Models


    # IMPORTANT NOTES:
    #   Note, there are essential differences between S-Plus and R
    #   Be aware of the details ... 
    help(arima)
    ###
    
    
    # Simulate Stationary AR(1) 
    # p. 66/67
    # with mu = 1, phi = .75 and sigma = 1
    # Note: set a seed to create reproducable results
    set.seed(101)
    # not needed: > e = rnorm(n = 100, sd = 1)
    # not needed: > e.start = rnorm(n = 25, sd = 1)
    y.ar1 = 1 + arima.sim(model = list(ar = 0.75), n = 100)
    mean(y.ar1)
    var(y.ar1)      
    # Compute true ACF:
    gamma.j = rep(0.75, 10)^seq(10)
    # Plot:
    # Figure 3.4
    par(mfrow = c(3, 2), cex = 0.7)
    ts.plot(y.ar1, main = "Simulated AR(1)", col = "steelblue4")
    grid()
    abline(h = 1)
    ts.plot(gamma.j, type = "h", main = "ACF and IRF for AR(1)",
        ylab = "Autocorrelation", xlab = "lag", col = "steelblue4")
    tmp = acf(y.ar1, lag.max = 10, col = "steelblue4")
    # Compute half life:
    log(0.5) / log(0.75)
    # Note, Rmetrics has also a synonyme function for 'ts.plot' ...
    tsPlot(y.ar1)
    ###

    
    # The file "lexrates.dat.csv) contains data for Spot and Forward 
    # Exchange Rate Data in Logarithmic Scale. The 12 data columns are:
    #   USCNS, USCNF, USDMS, USDMF, USFRS, USFRF, 
    #   USILS, USILF, USJYS, USJYF, USUKS, USUKF .
    # The first two letters abbreviate the home currency US Dollar,
    # the next two letters the foreign currency for the following six
    # countries/countries CN, DM, FR, IL, JY, UK, and the last letters
    # S or F abbreviates Spot or Forward.
    # Source:
    # Zivot E. (2000), Cointegration and forward and spot exchange 
    # rate regressions. Journal of International Money and Finance,
    # 19(6):785-812. 6:387-401. 
    # Data available from Thompson Financial, formerly Datastream.
    data(lexrates.dat) 
    lexrates.dat = as.timeSeries(lexrates.dat, format = "%m/%d/%Y")
    head(lexrates.dat)
    c(start(lexrates.dat),end(lexrates.dat))
    ###
    
        
    # ACF for Interest Rate Spreads:
    # p. 68
    # [from term structure and exchange rates]
    uscn.id = 100*(lexrates.dat[, "USCNF"] - lexrates.dat[, "USCNS"])
    # colIds(uscn.id) = "USCNID"
    uscn.id@title = "US/CA 30 day interest rate differential"
    # Figure 3.5
    # Plot:
    par(mfrow = c(3, 1), cex = 0.7)
    plot(uscn.id, type = "l", main = "Interest Rate Differential",
        col = "steelblue4")
    grid()
    abline(h = 0, lty = 3)
    tmp = acf(seriesData(uscn.id), main = "Interest Rate Differential",
        col = "steelblue4")
    ###

    
    # The file "varex.ts.csv" contains real stock returns and output 
    # growth data. The column MARKET.REAL lists continuously compounded 
    # real returns on the SP500 index, the column RF.REAL lists real 
    # interest rates of 30-day US Treasury Bills, the column INF lists
    # continuously compounded growth rate of US CPI, and the column
    # IPG lists continuously compounded growth rate of US industrial 
    # production. 
    # Data are downloadable from Economagic's web site, use the
    # Rmetrics function 'economagicImport'.
    data(varex.ts) 
    varex.ts = as.timeSeries(varex.ts)
    head(varex.ts)
    c(start(varex.ts), end(varex.ts))
    ###
    

    # Example 4: 
    # Monthly Real Interest Rates:
    # p. 71/72
    colIds(varex.ts)
    # Use alternatively ...
    colnames(seriesData(varex.ts))
    # ... or
    colnames(varex.ts@Data)
    # ... or
    varex.ts@units
    # Specify the in.format:
    smpl = cutSeries(varex.ts, from = "1961-01-01", to = end(varex.ts))
    irate.real = smpl[, "RF.REAL"]
    # Figure 3.6
    # Plot:
    par(mfrow = c(3, 2), cex = 0.5)
    tmp = acf(seriesData(irate.real))
    plot(irate.real, type = "l", col = "steelblue4",
        main = "Monthly Real Interest Rate")
    grid(lty = "solid")
    tmp = acf(seriesData(irate.real), type = "partial",
        col = "steelblue4")
    ###
    

################################################################################
# Section 3.2.4 - Moving Average Models


    # Example 5: 
    # p. 71-73
    # Simulate Signal + Noise Model - Try several samples:
    set.seed(8057)
    eps = rnorm(n = 100, sd = 1.0)
    eta = rnorm(n = 100, sd = 0.5)
    z = cumsum(eta)
    y = z + eps
    dy = diff(y)
    # Figure 3.7
    # Plot:
    par(mfrow = c(3, 2), cex = 0.7)
    ts.plot(y, main = "Signal plus Noise", ylab = "y", col = "steelblue4")
    grid()
    ts.plot(dy, main = "1st Difference", ylab = "dy", col = "steelblue4")
    tmp = acf(dy, col = "steelblue4")
    grid()
    tmp = acf(dy, type = "partial", col = "steelblue4")
    ###
    
    
    # The file "singleIndex.dat.csv" contains the monthly closing 
    # prices for Microsoft Corporation and the S&P 500 index.
    # Data are downloadable from Yahoo's web site.
    data(singleIndex.dat)
    singleIndex.dat = as.timeSeries(singleIndex.dat, format = "%d-%b-%Y")
    head(singleIndex.dat)
    c(start(singleIndex.dat), end(singleIndex.dat))
    ###

    
    # Example 6: 
    # p 73/74
    # Overlapping Annual Returns for [msft]
    sp500.mret = returnSeries(singleIndex.dat[, "SP500"], type = "continuous")
    sp500.mret@title = "Monthly Returns on SP 500 Index"
    # Use applySeries for aggregate Series ...
    # > sp500.aret = aggregateSeries(sp500.mret, moving = 12, FUN = sum)
    c(start(sp500.mret), end(sp500.mret))
    from = timeSequence(
        from = timeDate("1990-03-01"), 
        to = timeDate("2000-03-01"), 
        by = "month", 
        format = "%Y-%m-%d", 
        FinCenter = "") - 24*3600
    to = timeSequence("1991-02-01", "2001-02-01", 
        "month", format = "%Y-%m-%d", FinCenter = "") - 24*3600 
    sp500.aret = applySeries(x = sp500.mret, from, to, sum)
    sp500.aret@title = "Monthly Annual Returns on SP 500 Index"
    # Figure 3.8
    # Plot:
    par(mfrow = c(3, 2), cex = 0.5)
    plot(sp500.mret, type = "l", main = sp500.mret@title, col = "steelblue4")
    grid()
    plot(sp500.aret, type = "l", main = sp500.aret@title, col = "steelblue4")
    grid()
    tmp = acf(seriesData(sp500.aret), main = sp500.mret@title,
        col = "steelblue4")
    tmp = acf(seriesData(sp500.aret), type = "partial", 
        main = sp500.aret@title, col = "steelblue4")
    ###


################################################################################
# Section 3.2.6 - Estimation of ARMA Models and Forecasting 

    # Show Help Page:
    ?arima
    ###
    
    
    # Show Argument list:
    # p. 78
    # Use "arima" for "arima.mle"
    args(arima)
    ###
    
    
    # The file "lexrates.dat.csv" contains data for Spot and Forward 
    # Exchange Rate Data in Logarithmic Scale. The 12 data columns are:
    #   USCNS, USCNF, USDMS, USDMF, USFRS, USFRF, 
    #   USILS, USILF, USJYS, USJYF, USUKS, USUKF .
    # The first two letters abbreviate the home currency US Dollar,
    # the next two letters the foreign currency for the following six
    # countries/countries CN, DM, FR, IL, JY, UK, and the last letters
    # S or F abbreviates Spot or Forward.
    # Source:
    # Zivot E. (2000), Cointegration and forward and spot exchange 
    # rate regressions. Journal of International Money and Finance,
    # 19(6):785-812. 6:387-401. 
    # Data available from Thompson Financial, formerly Datastream.
    data(lexrates.dat)
    lexrates.dat = as.timeSeries(lexrates.dat, format = "%m/%d/%Y")
    head(lexrates.dat)
    end(lexrates.dat)
    uscn.id = 100*(lexrates.dat[, "USCNF"] - lexrates.dat[, "USCNS"])
    head(uscn.id)
    ###
    
    
    # Example 7: 
    # p. 78-80
    # Estimate ARMA(1,1) Model for [uscn.id]:
    # Get the data ...
    uscn.id.dm = uscn.id - mean(seriesData(uscn.id))
    # > arma11.mod = list(ar = 0.75, ma = 0)
    # > arma11.fit = arima.mle(uscn.id.dm, model = arma11.mod)
    # Use "arima" for "arima.mle" ...
    arma11.fit = arima(seriesData(uscn.id.dm), order = c(1, 0, 1), 
        include.mean = FALSE)
    # Print the components of the fit ...
    names(arma11.fit)
    # Print he basic fit results ...
    arma11.fit
    # Print the Standard Errors ...
    std.errs = sqrt(diag(arma11.fit$var.coef))
    names(std.errs) = colnames(arma11.fit$var.coef)
    std.errs
    ###
    
    
    # Note, Rmetrics has a function named 'armaFit', which uses a
    #   formula specification as input, which wraps several 
    #   estimation procedures and which returns a S4 object
    #   with all the information for model specification and 
    #   parameter estimation, try it ...
    args(armaFit)
    class(uscn.id.dm)
    fit = armaFit(uscn.id.dm ~ arma(1, 1), include.mean = FALSE)
    # For your information ...
    class(fit)
    slotNames(fit)
    # Print method:
    print(fit)
    # Plot method:
    par(mfrow = c(3, 2), cex = 0.7)
    plot(fit)
    # Summary method:
    summary(fit, doplot = FALSE)
    # Left to the reader, try different estimation methods!
    ###
   
       
    # Estimate ARMA(1,1) with a constant:
    # p. 80
    # > arma11.fit2 = arima.mle(uscn.id, model = arma11.mod, xreg = 1)
    # Again, use "arima" for "arima.mle" ...
    arma11.fit2 = arima(seriesData(uscn.id.dm), order = c(1, 0, 1))
    # Print Method:
    arma11.fit2
    # Graphical Diagnostics - Figure 3.9:
    # Use "tsdiag" for "plot"  ...
    tsdiag(arma11.fit2)
    ###
    
    
    # In Rmetrics this would simply read:
    # The default setting already includes a constant term.
    arma11.fit2 = armaFit(uscn.id.dm ~ arma(1, 1))
    par(mfrow = c(2, 2), cex = 0.7)
    summary(arma11.fit2)
    ###
    
        
    # Produce h-step Ahead Forecasts:
    # p. 81
    end(uscn.id.dm)
    fcst.dates = timeSequence(from = "7/1/1996", to = "6/1/1997", 
        by = "months", format = "%m/%d/%Y")
    fcst.dates
    # Predict:
    uscn.id.dm.fcst = predict(arma11.fit2, n.ahead = 12)
    # Convert to data.frame Object:
    uscn.id.dm.fcst.df = data.frame(
        date = as.character(fcst.dates),
        pred = round(uscn.id.dm.fcst$pred, 4))
    uscn.id.dm.fcst.df
    # Convert to time Series Object:
    as.timeSeries(uscn.id.dm.fcst.df)
    ### ERROR
    
        
    # Names - Forecast and Standard Errors:
    names(uscn.id.dm.fcst)
    # Print Forecasts:
    pred = as.vector(uscn.id.dm.fcst[[1]])
    # Sorry, at the moment we have no better plot facilities,
    # the poor man's approach ...
    # Subsample Data:
    smpl = cutSeries(uscn.id.dm, from = "6/1/1995", to = "12/31/1999")
    c(start(smpl), end(smpl))
    time.smpl = 1995.5 + (0:12)/12
    data.smpl = as.vector(smpl@Data)
    par(mfrow = c(2, 1), cex = 0.5)
    plot(time.smpl, data.smpl, type = "l", 
        xlim = c(1995.5, 1997.5), ylim = c(-0.3, 0.35),
        main = "US/CA 30 day interest rate differential")
    grid(lty=1)
    # Predicted Data:
    time.pred = 1996.5 + (0:11)/12
    data.pred = as.vector(pred)
    lines(time.pred, data.pred, lty = 3, col = "steelblue4")
    # Standard Errors:
    se = as.vector(uscn.id.dm.fcst[[2]])
    lines(time.pred, data.pred+2*se, lty = 3, col = "steelblue4")
    lines(time.pred, data.pred-2*se, lty = 3, col = "steelblue4")
    abline(h = 1996.5, col = "steelblue4")
    ### ERROR

    
    # Some words about OLS ...
    # R and Rmetrics have no special function for OLS, ...
    # Usually, we can use the function "lm" or OLS functions
    # from the packages "Hmisc" and "systemfit". Here we
    # use "lm" and compare the results with those from
    # OLS given in the book, to use OLS estimators from "Hmisc"
    # and "systemfit" is left to the reader ...
    ###
    
    
    # Estimate AR(2) using OLS:
    # p. 82
    # > ar2.fit = OLS(USCNID ~ ar(2), data = uscn.id)
    # > ar2.fit
    # > abs(polyroot(c(1,-ar2.fit$coef[2:3])))
    # Use "lm", unfortunately "lm" cannot handle the formula 
    # expression ~ ar(2), so her comes a workaround ...
    uscn.id.ts = mergeSeries(
        x = uscn.id, y = cbind(
            as.matrix(lagSeries(uscn.id, k=1)),
            as.matrix(lagSeries(uscn.id, k=2))))
    head(uscn.id.ts)
    # Change column names:
    colnames(uscn.id.ts@Data) = c("USCNID", "LAG1", "LAG2")
    head(uscn.id.ts)    
    # Remove NAs:
    uscn.id.ts = uscn.id.ts[-(1:2), ]
    head(uscn.id.ts)    
    # Fit by OLS/lm:
    ar2.fit = lm(USCNID ~ LAG1 + LAG2, data = uscn.id.ts)
    ar2.fit
    abs(polyroot(c(1,-ar2.fit$coef[2:3])))
    # Note, you can also use directly the R function "ar".
    ###
    
    
################################################################################
# Section 3.2.7 -  Martingales and Martingale Difference Sequences


    # Example 8: 
    # p. 84
    # Simulate ARCH(1) process
    # For Bollerslev's "garch" modeling you can find functions to 
    # model garch processes in the contributed R-package 'tseries', 
    # but unfortunately there is no function to simulate a garch
    # process. So we will write a simple one ...
    garch.sim = function(model = list(omega = 1e-06, alpha = 0.1, 
        beta = 0.8, mu = 0), n = 100, innov = NULL, n.start = 100, 
        start.innov = NULL, rand.gen = rnorm, ...) {
            # All entries in the model list must be specified!
            max.order = max(length(model$alpha), length(model$beta))
            if (is.null(start.innov)) start.innov = rand.gen(n.start, ...)
            if (is.null(innov)) innov = rand.gen(n, ...)
            h = x = z = c(start.innov, innov)
            for (i in 1:max.order) {
                h[i] = model$omega/(1-sum(model$alpha)-sum(model$beta))
                x[i] = sqrt(h[i])*z[i]+model$mu }
            n.alpha = length(model$alpha)
            n.beta = length(model$beta)
            for (i in (max.order + 1):(n.start + n)) {
                h[i] = model$omega + sum(model$alpha*x[i-(1:n.alpha)]^2) + 
                    sum(model$beta*h[i-(1:n.beta)])
                x[i] = sqrt(h[i])*z[i] + model$mu }
            list(et = x[-(1:n.start)], sigma.t = h[-(1:n.start)]) }
    # > rt = simulate.garch(model = list(a.value = 0.1, arch = 0.8), ...
    # Use "garch.sim" for "simulate.garch" ...
    set.seed(853)
    rt = garch.sim(
        model = list(omega = 0.1, alpha = 0.8, beta = 0, mu = 0), n = 250)
    # Note, omega seems unrealistic large to me ...
    class(rt)
    names(rt)
    # Plot - Figure 3.11:
    par(mfrow = c(2, 1), cex = 0.75)
    ts.plot(rt$et, ylim = c(-2,2), 
        xlab = "", ylab = "", col = "steelblue4",
        main = "Simulated returns")
    grid()
    ts.plot(rt$sigma.t, xlab = "", ylab = "", 
        col = "steelblue4",
        main = "Simulated volatility")
    grid()
    ### ???
    
    
################################################################################
# Chapter 3.2.8 -  Long-Run Variance


    # Example 9
    # p. 87
    # Long-run variance of AR(1)    
    set.seed(101)
    e = rnorm(n = 100, sd = 1)
    # use 'arima.Sim' for Splus like 'arima.sim'
    y.ar1 = 1 + arima.sim(n = 100, model = list(ar = 0.75), innov = e)
    y.ar1
    # Use "lm" to fit ...
    # > ar1.fit = OLS(y.ar1 ~ ar1, data = ar1)
    N = length(y.ar1)
    ar1 = data.frame(y.ar1 = y.ar1[-1], ar1 = y.ar1[-N])
    ar1.fit = lm(y.ar1 ~ ar1, data = ar1)
    ar1.fit
    rho.hat = coef(ar1.fit)[2]
    sig2.hat = sum(residuals(ar1.fit)^2)/ar1.fit$df.resid
    # Long range Variance:
    lrv.ar1 = sig2.hat/(1-rho.hat)^2
    as.numeric(lrv.ar1)
    # Standard Error:
    se.ybar.ar1 = sqrt(lrv.ar1/100)
    as.numeric(se.ybar.ar1)
    ###
    

    # Newey-West Estimate
    # p. 88
    # Sorry not yet available ...
    # > args(asymp.var)
    # > lrv.nw = asymp.var(y.ar1, bandwidth = 4)
    # > lrv.nw
    # > sqrt(lrv.nw/100)
    ###


################################################################################
# Chapter 3.3 - Univariate Nonstationary Time Series


    # Example 10 
    # p. 88/89
    # Trend Stationary AR(1) ...
    # with mu = 1, delta = 0.25, phi = 0.75 and sigma = 1
    # > set.seed(101)
    y.tsar1 = 1 + 0.25*seq(100) + arima.sim(model = list(ar = 0.75), n = 100)
    # Figure 3.12
    # Plot:
    par(mfrow = c(2,1), cex = 0.7)
    ts.plot(y.tsar1, xlab = "Index", ylab =" y", 
      main = "Trend Stationary AR(1)")
    abline(a = 1, b = 0.25, col = "steelblue4")
    ###
    

    # Example 11
    # p. 91
    # Simulated I(1) processes ...
    # > set.seed(101)
    set.seed(931)
    u.ar1 = arima.sim(model = list(ar = 0.75), n = 100)
    y1 = cumsum(u.ar1)
    y1.d = 1 + 0.25*seq(100) + y1
    y2 = rep(0, 100)
    for (i in 3:100) { y2[i] = 2*y2[i-1] - y2[i-2] + u.ar1[i] }
    # Figure 3.13
    # Plot:
    par(mfrow = c(3, 2), cex = 0.5)
    ts.plot(u.ar1, main = "I(0) innovations", col = "steelblue4")
    ts.plot(y1, main = "I(1) process", col = "steelblue4")
    ts.plot(y1.d, main = "I(1) process with drift", col = "steelblue4")
    abline(a = 1, b = 0.25)
    ts.plot(y2, main = "I(2) process", col = "steelblue4")
    ###


    # Example 12
    # p 91/92
    # Financial time series ...
    data(lexrates.dat)
    lexrates.dat = as.timeSeries(lexrates.dat)
    uscn.spot = lexrates.dat[, "USCNS"]
    data(singleIndex.dat)
    singleIndex.dat = as.timeSeries(singleIndex.dat)
    sp500 = singleIndex.dat[, "SP500"]
    # Figure 3.14
    # Plot:
    par(mfrow = c(3, 2), cex = 0.7)
    plot(uscn.spot, main = "Log US/CA spot exchange rate",
        reference.grid = FALSE, type = "l", col = "steelblue4")
    # R/Rmetrics has no log for timeSeries objects,
    # Use the workaround ...
    sp500.log = sp500
    sp500.log@Data = log(sp500@Data)
    plot(sp500.log, main = "Log S&P 500 index", 
       reference.grid = FALSE, type = "l", col = "steelblue4")
    # 30-Day Tbills:    ) 
    data(rf.30day) 
    # Convert to timeSeries object:
    rf.30day = as.timeSeries(rf.30day)      
    plot(rf.30day, main = "Nominal 30 day T-bill rate", ylab = "Rate",
        reference.grid = FALSE, type = "l", col = "steelblue4")
    # CPI:
    data(CPI.dat)
    CPI.dat = as.timeSeries(CPI.dat, format = "%d-%B-%Y")
    CPI.dat.log = CPI.dat
    CPI.dat.log@Data = log(CPI.dat@Data)
    plot(CPI.dat, main = "Log of US CPI", ylab = "log CPI.dat", 
        reference.grid = FALSE, type = "l", col = "steelblue4")
    ###
    

################################################################################
# Chapter 3.4 - Long Memory Time Series


    # Example 13
    # p. 93 
    # Simulated fractional white noise
    # In R you need the contributed package "fracdiff" ...
    # > require(fracdiff)   
    # > y.fwn = simulate.FARIMA(model = list(ar = 0, ma = 0, d = 0.3), n = 500)
    # Use alternatively ...
    # > args(fracdiff.sim)
    # > y.fwn = fracdiff.sim(n = 500, ar = 0, ma = 0, d = 0.3, mu = 0)$series 
    # In Rmetrics 'fracdiff' is builtin in 
    ?armaModelling
    args(armaSim)
    # Simulate:
    y.fwn = armaSim(model = list(ar = 0, d = 0.3, ma = 0), n = 500) 
    # Figure 3.15
    # Plot:
    par(mfrow = c(2, 1), cex = 0.7)
    ts.plot(y.fwn, main = "Series y.fwn", col = "steelblue4")
    grid()
    tmp = acf(y.fwn, lag.max = 50)
    ###


    # Example 14: 
    # p. 93/94
    # Long memory in financial time series
    # 'abs' does't work on 'timeDate' objects, use 'seriesData':
    msft.aret = abs(seriesData(returnSeries(DowJones30[, "MSFT"])))
    uscn.id = seriesData(100*(lexrates.dat[,"USCNF"]-lexrates.dat[,"USCNS"]))
    # Plot - Figure 3.16:
    par(mfrow = c(2, 1), cex = 0.7)
    tmp = acf(msft.aret, lag.max = 100, main = "MSFT Annual Returns")
    tmp = acf(uscn.id, lag.max = 50, main = "Monthly 30-day Interest Rates")
    ###
    
    
################################################################################
# Section 3.5.1 - Stationary and Ergodic Multivariate Time Series


    # Example 15
    # p 96/97
    # System of asset returns:
    Y = returnSeries(DowJones30[, 1:4], type = "continuous")
    colnames(seriesData(Y))
    Y@units
    # Compute means and variances and correlations
    y = seriesData(Y)
    colMeans(y) 
    var(y)                  
    cor(y)
    # Add functions for "colVars" and "colStdevs" ...
    colVars = function(x) { apply(x, MARGIN = 2, FUN = var) }
    colStdevs = function(x) { sqrt(apply(x, MARGIN = 2, FUN = var)) }
    colVars(y)
    colStdevs(y)
    ###
    
    
    # Example 16
    # p. 98/99
    # Lead-lag covariances and correlations among asset returns ...
    y = seriesData(Y)
    Ghat = acf(y[, 1:2], lag.max = 5, type = "covariance", plot = FALSE)
    Rhat = acf(y[, 1:2], lag.max = 5, plot = FALSE)
    Rhat    
    par(mfrow = c(2, 2), cex = 0.7)
    acf.plot(Rhat)
    # Use the same number of digits as in the book ...
    round(Ghat$acf[1, , ], digits = 8)
    round(Rhat$acf[1, , ], digits = 4)
    Ghat$acf[2, , ]
    round(Rhat$acf[2, , ], digits = 5)
    ###


################################################################################
# Section 3.5.2 - Multivariate Wold Representation


    # Simulate VAR(1) model: 
    # Note, can't use filter() for efficiency
    nobs = 250
    # Note, the multivariate normal distribution function 'mvrnorm' 
    #   can be found in library MASS, or we can simulate normal random 
    #   variates directly ...
    # For 
    # > e.var = rmvnorm(nobs, sd = c(1, 1), rho = 0.5)
    # we use:
    set.seed(4711)
    rho = 0.5
    X =  matrix(rnorm(2 * nobs), nobs)
    Sigma = matrix(c(1, rho, rho, 1), 2)
    eS = eigen(Sigma, sym = TRUE)
    e.var = t(eS$vectors %*% diag(sqrt(pmax(eS$values, 0)), 2) %*% t(X))
    # Show:
    e.var
    # Test:
    cov(e.var)
    # VAR(1) coef Matrix:
    pi1 = matrix(c(0.7, 0.2, 0.2, 0.7), 2, 2, byrow = TRUE)  
    # Mean vector :                            
    mu.vec = c(1, 2)                                   
    # Intercept in VAR:
    c.vec = as.vector((diag(2)-pi1)%*%mu.vec)      
    y.var = matrix(rep(0, 2*nobs), 2)
    # Initial Value is mean:
    y.var[, 1] = mu.vec                             
    for (i in 2:nobs) 
        y.var[, i] = c.vec + pi1 %*% y.var[, i-1] + e.var[i, ]
    y.var = t(y.var)
    # Show:
    dimnames(y.var) = list(NULL, c("y1", "y2"))
    as.data.frame(y.var)
    colMeans(y.var)
    # Plot:
    par(mfrow = c(2, 1), cex = 0.7)
    ts.plot(y.var, col = c("red", "blue"), ylab = "AR", 
        main = "Two correlated AR(1) processes, rho=0.5")
    ###
    
    
################################################################################
# Section 3.5.3 - Long Run Variance


    # Example 17
    # Newey-West estimate of long-run variance matrix for stock returns
    Y = returnSeries(DowJones30[, 1:4], type = "continuous")
    M.T = floor(4*(nrow(as.data.frame(Y))/100)^(2/9)) # 8
    M.T
    # Note, sorry a function for 'asymp.var' is not yet 
    #   implemented ...
    # lrv.nw = asymp.var(Y, bandwidth = M.T)
    # lrv.nw
    ### TODO
       

################################################################################

