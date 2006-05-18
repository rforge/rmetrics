#
# Examples from the forthcoming Monograph:
#   Rmetrics - Financial Engineering and Computational Finance
#   A Practitioner's Guide for R and SPlus Programmers
#   written by Diethelm Wuertz
#   ISBN to be published
#
# Details:
#   Chapter 3.1
#   Stationary Time Series: ARMA Modelling
#
# List of Examples, Exercises and Code Snippets:
#
#   3.1.1  Example: Linear Filter Representation
#   3.1.2  Example: AR/MA Filter Representation
#   3.1.3  Example: Simulate and Plot ARMA Series
#   3.1.4  Example: Compute ARMA Sample ACF and PACF
#   3.1.5  Example: Compute True ARMA ACF/PACF
#   3.1.6  Example: Create Figures 3.1.1 and 3.1.2
#   3.1.7  Example: Sample ACF and PACF for UK Interest Rate Spread
#   3.1.8  Example: Check ARMA Stationarity and Invertibility
#   3.1.9  Example: Identify ARMA Order through the PACF
#   3.1.10 Code Snippet: .arolsFit Function
#   3.1.11 Example: Estimate ARMA Model Parameters
#   3.1.12 Example: Estimate US/CA Interest Rate Differential
#   3.1.13 Example: Select Best ARMA Model
#   3.1.14 Example: Best ARMA Model for FTA Return Series
#   3.1.15 Example: ARMA Diagnostic Analysis
#   3.1.16 Example: ARMA Diagnostics for US/CA IR Differential
#   3.1.17 Example: ARMA Forecasting from FT30 Index
#
#
# Author:
#   (C) 1997-2005, Diethelm Wuertz, GPL
#     www.rmetrics.org
#     www.itp.phys.ethz.ch
#     www.finance.ch
#


################################################################################
# Stationary ARMA Processes


### Load package:

    require(fSeries)
    ###
    
    
# ------------------------------------------------------------------------------


### 3.1.1 Example: Linear Filter Representation

    # Create 100 Innovations:
    set.seed(4711)
    u = rnorm(100)
    ###
    
    # Convolution Filter: mu = 0.1, phi0 = 1, phi1 = 0.1,
    #   phi2 = 0.1, and the remaining phi's are zero
    mu = 0.1
    phi = c(1, 0.1, 0.1)
    x = mu + filter(x = u, filter = phi, sides = 1)
    ###
    
    # Check:
    x[21]
    mu + u[21] + 0.1*(u[20]+u[19])
    ###
       
  
    
# ------------------------------------------------------------------------------


### 3.1.2 Example: AR/MA Filter Representation

    # Example: 100 Innovations:
    x = rnorm(100)
    ###
    
    # Example: Simulating an MA(2) process:
    ma = c(0.2, 0.5)
    filter(x, c(1, ma), sides = 1)
    ###
    
    # Example: Simulating an AR(2) process:
    ar = c(0.5, -0.5)
    filter(x, ar, method = "recursive")
    ###
    

# ------------------------------------------------------------------------------


### 3.1.3 Example: Simulate and Plot ARMA Series

    # Use armaSim to simulate an AR(1) with phi = 0.5, and
    # an ARMA(2,1) process with phi1 = 0.5, phi2 = -0.5,
    # and theta = 0.8.
    ###
    
    # Graphics Frame:
    par(mfrow = c(2, 1), cex = 0.7)
    ###
    
    # AR(1) Simulation:
    x = armaSim(model = list(ar = 0.5))
    plot(x, main = "AR(1): ar = 0.5")
    ###
    
    # ARMA(2,1) Simulation:
    x = armaSim(model = list(ar = c(0.5, -0.5), ma = 0.8))
    plot(x, main = "ARMA(2,1): ar = (0.5, -0.5), ma = 0.8")
    ###
    
    
# ------------------------------------------------------------------------------
    

### 3.1.6 Example: Plot ARMA Sample ACF and PACF

    # Graphics Frame:
    par(mfrow = c(2, 2), cex = 0.7)
    ###
    
    # AR(1) Sample Correlations:
    x = armaSim(model = list(ar = 0.5))
    acf(x, lag.max = 12, main = "ACF: AR(1)")
    pacf(x, lag.max = 12, main = "PACF: AR(1)")
    ###
    
    # ARMA(2,1) Sample Correlations:
    x = armaSim(model = list(ar = c(0.5, -0.5), ma = 0.8))
    acf(x, lag.max = 12, main = "ACF: ARMA(2,1)")
    pacf(x, lag.max = 12, main = "PACF: ARMA(2,1)")
    ###
    
    # SUBSET AR(5) Model 
    # Show that the PACF is the last regression coefficient ...
    set.seed(4711)
    x = armaSim(model = list(ar = c(0.5, 0.1, 0, 0, 0.2)), n = 5000)
    PACF = NULL
    for ( order in 1:7 ) {
        OLS = ar.ols(x, AIC = FALSE, order.max = order, 
            demean = TRUE, intercept = TRUE)
        PACF = c(PACF, OLS$ar[,,1][order] )
    }
    round(rbind(PACF, pacf = pacf(x, lag.max = 7)$acf[,,1]), 2)
    ###
    
    
# ------------------------------------------------------------------------------    
    
   
### 3.1.5 Example: Compute True ARMA ACF/PACF

    # Specify Model and Simulate Series:
    model = list(ar = c(0.5, 0.3))
    x = armaSim(model)
    ###
    
    # Empirical ACF - Print as data frame:
    data.frame(lag = 0:12, acf = acf(x, lag.max = 12)$acf)
    ###

    # True ACF:
    armaTrueacf(model, lag.max = 12)
    ###

    # Do the same for the PACF:
    ###
    
    # Empirical PACF - Print as data frame:
    data.frame(lag = 1:12, pacf = pacf(x, lag.max = 12)$acf)
    ###
    
    # True ACF:
    armaTrueacf(model, lag.max = 12, type = "partial")
    ###
 
    
# ------------------------------------------------------------------------------


### 3.1.6 Example: Create Figures 3.1.1 and 3.1.2

    # Create all Graphs from Figures 3.1.1 and 3.1.2
    # The solution can be found in the fSeries/demo directory
    # ...  
    
    # Create Figure 3.1.1
    par(mfrow = c(4, 3), cex = 0.6)
    # Simulate ARIMA Time Series Processes:
    set.seed(471)
    # AR(1) and AR(2) Examples:  
    x = armaSim(model = list(ar = 0.5, d = 0, ma = 0), n = 1000)
      ts.plot(x, main = "AR(1): +0.5")
      acf(x, lag.max = 12)
      acf(x, type = "partial", lag.max = 12)
    x = armaSim(model = list(ar = -0.5, d = 0, ma = 0), n = 1000)
      ts.plot(x, main = "AR(1): -0.5")
      acf(x, lag.max = 12)
      acf(x, type = "partial", lag.max = 12)
    x = armaSim(model = list(ar = c(0.5, 0.3), d = 0, ma = 0), n = 1000)
      ts.plot(x, main = "AR(2): +0.5, +0.3")
      acf(x, lag.max = 12)
      acf(x, type = "partial", lag.max = 12)    
    x = armaSim(model=list(ar = c(-0.5, 0.3), d = 0, ma = 0), n = 1000)
      ts.plot(x, main = "AR(2): -0.5, +0.3")
      acf(x, lag.max = 12)
      acf(x, type = "partial",lag.max = 12)
    ###
       
    # Create Figure 3.1.1 cont.
    par(mfrow = c(4, 3), cex = 0.6)
    # Investigate Correlations:
    set.seed(471)
    # MA and ARMA Examples:
    x = armaSim(n = 1000, model = list(ar = 0, d = 0, ma = 0.8))
      ts.plot(x, main = "MA(1): +0.8")
      acf(x, lag.max = 12)
      acf(x, type = "partial", lag.max = 12)
    x = armaSim(n = 1000, model = list(ar = 0, d = 0, ma = -0.8))
      ts.plot(x, main = "MA(1): -0.8")
      acf(x, lag.max = 12)
      acf(x, type = "partial", lag.max = 12)     
    x = armaSim(n = 1000, model = list(ar = 0.5, d = 0, ma = 0.8))
      ts.plot(x, main = "ARMA(1,1): +0.5, +0.8")
      acf(x, lag.max = 12)
      acf(x, type = "partial", lag.max = 12)   
    x = armaSim(n = 1000, model = list(ar = -0.5, d = 0, ma = 0.8))
      ts.plot(x, main = "ARMA(1,1): -0.5, +0.8")
      acf(x, lag.max = 12)
      acf(x, type = "partial", lag.max = 12)
    ###
    
    # Example: Create Figure 3.1.2
    # Compute ACF of the True Time Series Model
    par(mfcol = c(5, 2), cex = 0.6)   
    # First Model:
    model = list(ar = c(+0.5, 0.3), d = 0, ma = 0)
    x = armaSim(model, n = 1000)
    ts.plot(x, main = "AR(2): +0.5, 0.3")
    acf(x, lag.max = 12)
    armaTrueacf(model, lag.max = 12)
    acf(x, lag.max = 12, type = "partial")
    armaTrueacf(model, lag.max = 12, type = "partial")  
    # Second Model:
    model = list(ar = c(-0.5, 0.3), d = 0, ma = 0)
    x = armaSim(model, n = 1000)
    ts.plot(x, main = "AR(2): -0.5, 0.3")
    acf(x, lag.max = 12)
    armaTrueacf(model, lag.max = 12)
    acf(x, lag.max = 12, type = "partial")
    armaTrueacf(model, lag.max = 12, type = "partial")
    ###
    
    
# ------------------------------------------------------------------------------


### 3.1.7 Example: Sample ACF and PACF for UK Interest Rate Spread

    # Example 2.2 from Mills' Textbook - Load Data:
    data(R20); data(RS); x = (R20-RS)[, 1]
    ###
    
    # Compute Sample ACF and Sample PACF:
    N = length(x)
    ACF = acf(x, lag.max = 12)$acf[,,1]
    seACF = sqrt(1/N)
    for (k in 2:12) seACF =
       c(seACF, sqrt((1+2*sum(ACF[2:k]^2))/N))
    PACF = pacf(x, lag.max = 12)$acf[,,1]
    sePACF = rep(sqrt(1/N), times = 12)
    ###
    
    # Reproduce Table 2.2 in Mills' Textbook:
    data.frame(round(cbind(
       "r_k" = ACF[-1], "seACF" = seACF,
       "phi_kk" = PACF, "sePACF" = sePACF ), 3) )
   ###

    # Compute 95% Confidence Level:
    round(2/sqrt(length(x)), 3)
    ###

    # Create Graphs:
    par(mfrow = c(2,2), cex = 0.7)
    acfPlot(x, 12, main = "ACF: UK IR Spread" )
    points(0:12, c(0,seACF), pch=19, col = "steelblue")
    pacfPlot(x, 12,main = "PACF: UK IR Spread")
    ###
    
    
# ------------------------------------------------------------------------------

### 3.1.8 Example: Check ARMA Stationarity and Invertibility

    # Graphics Frame:
    par(mfcol = c(2, 3), cex = 0.7)
    ###
    
    # Compute the Roots of 1 - 1.6 B - 0.4 B^2 :
    coefficients = c(1.6, -0.4)
    root = polyroot(c(1, -coefficients))
    re = Re(root)
    im = Im(root)
    dist = sqrt(re^2 + im^2)
    # Nice Printing:
    data.frame(round(cbind(re, im, dist), digits = 3))
    ###

    # The function also allows to plot the root circles.
    # Find the roots of the polynomials
    # ($1-1.6B -0.4B^2$),
    # ($1-0.5B-0.1B^2$), and
    # ($1-0.5B+0.9B^2-0.1B^3-0.5B^4$).
    armaRoots(c(1.6, 0.4))
    armaRoots(c(0.5, -0.1))
    ###
    
    # Next graph:
    armaRoots(c(0.5, -0.9, 0.1, 0.5))
    ###
    

# ------------------------------------------------------------------------------

    
### 3.1.9 Example: Identify Order through the PACF  

    # Settings:
    par (mfrow = c(2, 2), cex = 0.7)
    set.seed(4711)
    ###
    
    # Simulate a Subset AR(5) Process:
    x = armaSim(model = list(ar = c(-0.4, 0.1, 0, 0, 0.1), 
      d = 0, ma = 0), n = 5000)
    ###
    
    # Plot:
    ts.plot(x, xlab = "t", col = "steelblue")
    title(main = "Subset AR(5) Time Series") 
    grid()
    abline(h = 0, col = "grey")
    ###
    
    # Indentify the model with the help of the PACF ...
    # PACF: Note, the PACF is consistent with a subset AR(5)  
    # model with nonzero parameters phi at positions 1, 2 and 5.
    acf(x, type = "partial", lag.max = 12, main = "Subset AR(5) PACF")
    grid()
    ###
   
  
# ------------------------------------------------------------------------------


### 3.1.10 Code Snippet: .arolsFit Function

    # Write Function:
    .arolsFit =
    function (formula = x ~ ar(1))
    {
        # Keep Call:
        Call = match.call()

        # Inspect Formula:
        if (length(formula) != 3) stop("Misspecified Formula")
        tsmodel = substr(as.character(formula[3]), 1, 2)
        if (tsmodel != "ar") stop("Misspecified Model")

        # Extract Order:
        order = substr(as.character(formula[3]), 4, nchar(formula[3])-1)
        order = as.integer(order)

        # Get time Series Data:
        ts = eval(formula[[2]], +sys.parent())
        if (class(ts) == "timeSeries")x = as.vector(ts)
        if (class(ts) == "ts") x = as.vector(ts)

        # Fit the AR Model:
        fit = ar.ols(x = x, aic = FALSE, order.max = order,
            demean = TRUE, intercept = TRUE, newdata = x)
        class(fit) = "list"

        # Add $residuals, $fitted.values, $coef, and $sigma2
        # according to Rmetrics naming conventions:
        fit$tstitle = paste("AR(", order, ") with method: OLS", sep = "")
        fit$tsmodel = "ar"
        fit$residuals = fit$resid
        fit$fitted.values = ts - fit$resid
        fit$resid = NULL
        fit$coef = c(fit$ar[,,1], fit$x.intercept)
        fit$se.coef = unlist(c(fit$asy.se.coef$ar, fit$asy.se.coef[1]))
        fit$sigma2 = fit$var.pred[[1]]
        names(fit$coef) = names(fit$se.coef) =
            c(paste("ar", 1:order, sep = ""), "intercept")

        # Return Value:
        new("fARMA",
            call = Call,
            formula = formula,
            method = fit$tstitle,
            parameter = list(order = order),
            data = list(x = ts),
            fit = fit,
            residuals = as.vector(fit$residuals),
            fitted.values = as.vector(fit$fitted.values),
            title = as.character("AR OLS Modelling"),
            description = as.character(date())
        )
    }
    ###

    # Graph Frame:
    par(mfrow = c(2, 2), cex = 0.7)
    ###
    
    # Try:
    set.seed(4711)
    x = armaSim()
    fit = .arolsFit(x ~ ar(3))
    print(fit)
    plot(fit)
    summary(fit)
    ###
    
    
# ------------------------------------------------------------------------------


### 3.1.11 Example: Parameter Estimation

    # Simulate Time Series Process:
    x = armaSim(model = list(ar = c(0.5, -0.5), ma = 0.8))
    ###
    
    # Fit Model Parameters:
    armaFit(x ~ arima(2, 0, 1))
    ###
    
    # Fitting another type of model we have to specify
    # the model trough the formula argument:
    y = armaSim(model = list(ar = 0.5), n = 2000)
    armaFit(formula = y ~ ar(1))
    ###

    # To exclude the intercept from estimation use:
    armaFit(x ~ arma(2, 1), include.mean = FALSE)
    ###

    # To estimate a subset model, fix the coefficients.
    # Since AR and ARMA don't support this option we fit
    # an ARIMA(5, 0, 0) model. Note, fixed has 6 elements,
    # 5 AR coefficients and one for the mean.
    z = armaSim(model = list(ar = c(0.5, -0.4, 0, 0 , 0.2)))
    fit = armaFit(formula = z ~ arima(5, 0, 0), fix = c(NA, NA, 0, 0, NA, NA))
    # Print the result, i.e. title, call, model, method
    # and estimated coefficients
    summary(fit)
    ###
    

# ------------------------------------------------------------------------------


### 3.1.12 Example: Estimate US/CA Interest Rate Differential

    # Load Data:
    data(lexrates.dat)
    uscn.id = 100*(lexrates.dat[,"USCNF"]-lexrates.dat[,"USCNS"])
    ###
    
    # Plot:
    plot(uscn.id, type = "l", main = "US/CA Interest Rate Differential")
    grid()
    abline(h = 0, col = "grey")
    ###
    
    # Case (i) - Demeaned Series:
    uscn.id.dm  = uscn.id - mean(uscn.id)
    fit = armaFit(uscn.id.dm ~ arima(1, 0, 1), method = "CSS",
        include.mean = FALSE)@fit
    # Estimated Parameters - S Plus ZW: 0.82913 | 0.11008
    round(fit$coef, 5)
    # Standard Errors - S-Plus: 0.04523 | 0.08041
    round(fit$se.coef, 5)
    ###
    
    # Case (ii) - Including Mean:
    fit = armaFit(uscn.id ~ arima(1, 0, 1), method = "CSS")@fit
    # Estimated Parameters - S Plus ZW: 0.82934 | 0.11065 | -0.1347
    round(fit$coef, 5)
    # Standard Errors - S-Plus: 0.04523 | 0.08041
    round(fit$se.coef, 5)
    ###     
     
# ------------------------------------------------------------------------------


### 3.1.13 Example: Select Best ARMA Model

    # Settings:
    par (mfrow = c(2, 1), cex = 0.7)
    set.seed(4732) 
    ###
    
    # Simulate time series:
    x = armaSim(n = 10000,
        model = list(ar = c(-0.4, 0.1, 0, 0, 0.1), d = 0, ma = 0)) 
    ###
    
    # Estimate the Parameters:
    fit52 = armaFit(x ~ arima(5, 0, 2),
      fixed = c(NA, NA, NA, NA, NA, NA, NA, 0))@fit
    fit51 = armaFit(x ~ arima(5, 0, 2),
      fixed = c(NA, NA, NA, NA, NA,  0, NA, 0))@fit
    fit41 = armaFit(x ~ arima(5, 0, 2),
      fixed = c(NA, NA, NA,  0, NA,  0, NA, 0))@fit
    fit31 = armaFit(x ~ arima(5, 0, 2),
      fixed = c(NA, NA,  0,  0, NA,  0, NA, 0))@fit
    fit30 = armaFit(x ~ arima(5, 0, 2),
      fixed = c(NA, NA,  0,  0, NA,  0,  0, 0))@fit
    fit20 = armaFit(x ~ arima(5, 0, 2),
      fixed = c(NA,  0,  0,  0, NA,  0,  0, 0))@fit
    fit10 = armaFit(x ~ arima(5, 0, 2),
      fixed = c(NA,  0,  0,  0,  0,  0,  0, 0))@fit
    ###
    
    # Print and Plot Results:  
    p = c(5, 5, 4, 3, 3, 2, 1)
    q = c(2, 1, 1, 1, 0, 0, 0)
    sigma2 = c(fit52$sigma2, fit51$sigma2, fit41$sigma2, 
      fit31$sigma2, fit30$sigma2, fit20$sigma2, fit10$sigma2)
    aic = c(fit52$aic, fit51$aic, fit41$aic, fit31$aic, fit30$aic,
      fit20$aic, fit10$aic)
    plot(rev(p+q), rev(aic), xlab = "p+q", ylab = "aic", 
      main = "AIC Statistics")
    cbind(p, q, sigma2, aic)
    ###
    
    
# ------------------------------------------------------------------------------


### 3.1.14 Example: Best ARMA Model for FTA Return Series

    # Example 2.3 from Mills - Load FTA Share Return Data:
    data(FTARET)
    x = FTARET[,1]
    ###
    
    # Plot:
    plot(x, type = "l", main = "FTA Return Series")
    grid()
    abline(h = 0, col = "grey")
    ###
    
    # BIC Statistics:
    N = length(x)
    n = 4
    BIC = matrix(rep(0, n^2), n)
    for (p in 0:(n-1) ) {
      for (q in 0:(n-1) ) {
       Formula = paste("x ~arima(", p, ",0,", q, ")", sep = "")
       fit = armaFit(as.formula(Formula), method = "CSS-ML")@fit
       BIC[p+1, q+1] = log(fit$sigma2) + log(N)*(p+q) / N } }
    rownames(BIC) = colnames(BIC) = as.character(0:(n-1))
    ###
    
    # Print Deviations from the Largest Value:
    print(round(max(BIC) - BIC, 3))
    #       0     1     2     3
    # 0 0.025 0.039 0.031 0.021
    # 1 0.033 0.032 0.017 0.009
    # 2 0.026 0.017 0.000 0.008
    # 3 0.029 0.015 0.003 0.005
    ###
    
    
# ------------------------------------------------------------------------------


### 3.1.15 Example: ARMA Diagnostic Analysis

    # Simulate AR(2) Time Series:
    set.seed(4711)
    model = list(ar = c(-0.40, 0.1))
    x = armaSim(n = 1000, model = model)
    ###
    
    # Estimate Parameters:
    fit = armaFit(x ~ arima(2, 0, 0))
    ###
    
    # Diagnostic Analysis:
    # The graphical output is displayed in Figure 3.1.6.
    summary(fit)
    ###
    
    
# ------------------------------------------------------------------------------


### 3.1.16 Example: ARMA Diagnostics for US/CA IR Differential

    Repeat Example 3.1.11 from Zivot and Wang [2001]:
    fit = armaFit(uscn.id ~ arima(1, 0, 1), method = "CSS")
    plot(fit)
    ###
    
    
# ------------------------------------------------------------------------------


### 3.1.17 Example: ARMA Forecasting from FT30 Index

    # Use FT30 Index Data from T. Mills:
    data(FT30)
    x = log(FT30[,1])
    fit = armaFit(x ~ arima(2, 1, 0))
    ###
    
    # Compute Forecasts and Forecast Errors:
    forecast = predict(fit, 25)
    title(main = "\n\nlog Index FT30", cex = 0.5)
    forecast
    ###
    
    # AR(2) log Returns:
    x = diff(x)
    fit = armaFit(x ~ arima(2, 0, 0))
    forecast = predict(fit, 25)
    title(main = "\n\n\log Return FT30", cex = 0.5)
    forecast
    ###

    
################################################################################    

