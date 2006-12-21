#
# Examples:
#   A Compendium for R and Rmetrics users to the book 
#     "The Econometric Modelling of Financial Time Series" 
#     written by T.C. Mills
#   ISBN 0-521-62492-4
#
# Description:
#   See Chapter 2 of the book.
#
# Author:
#   (C) Rmetrics 1997-2005, Diethelm Wuertz, GPL
#     www.rmetrics.org
#     www.itp.phys.ethz.ch
#     www.finance.ch
#


################################################################################    


### Mills 2.1: 

    # Load Data:
    data(SP500R)
    x = SP500R[,1]
    
    # Sample Autocorrelation Function:
    rk = round(acf(x, 12)$acf[,,1][-1], 3)
    names(rk) =  paste("lag", 1:12, sep = "")
    rk
    
    # 95% Confidence Interval:
    ci95 = round(2/sqrt(length(x)), 3)
    ci95
    
    # Q(k) Statistics:
    Qk = NULL
    for (lag in 1:12 ) 
        Qk = c(Qk, Box.test(x, lag, "Ljung-Box")$statistic)
    names(Qk) =  paste("lag", 1:12, sep = "")
    round(Qk, 2)
    
    
# ------------------------------------------------------------------------------
    
    
### Mills 2.2
        
    # Time Series Data:
    data(R20); data(RS)
    x = (R20-RS)[, 1]
    time = 1952+(1:length(x))/12
    plot(time, x, type = "o", pch = 19, cex = 0.5)
    
    # Parameter Estimation:
    fit = armaFit(x ~ ar(2), "ols")@fit
    
    # Estimated Parameters and Standard Errors:
    round(rbind(coef = fit$coef, se.coef = fit$se.coef), 3)
    
    # sigma:
    round(sqrt(fit$sigma2[[1]]), 3)
    
    # Compute Sample ACF and Sample PACF:
    N = length(x)
    ACF = acf(x, lag.max = 12)$acf[,,1]
    seACF = sqrt(1/N)
    for (k in 2:12) seACF =
        c(seACF, sqrt((1+2*sum(ACF[2:k]^2))/N))
    PACF = pacf(x, lag.max = 12)$acf[,,1]
    sePACF = rep(sqrt(1/N), times = 12)
        
    # Reproduce Table 2.2 in Mills' Textbook:
    data.frame(round(cbind(
       "r_k" = ACF[-1], "se" = seACF,
       "phi_kk" = PACF, "se" = sePACF ), 3) )
       
    # Create Graphs:
    par(mfrow = c(2,2), cex = 0.7)
    acfPlot(x, 12, main = "ACF: UK IR Spread" )
    points(0:12, c(0,seACF), pch=19, col = "steelblue")
    pacfPlot(x, 12,main = "PACF: UK IR Spread")

    
# ------------------------------------------------------------------------------

    
### Mills 2.3   
    
    # Load Data:
    data(FTARET)
    x = FTARET[,1]
    
    # Sample Autocorrelation Function:
    round(acf(x, 12)$acf[,,1], 3)[-1]
    
    # IC Statistics:
    N = length(x)
    n = 4
    BIC = AIC = matrix(rep(0, n^2), n)
    for (p in 0:(n-1) ) {
        for (q in 0:(n-1) ) {
            Formula = paste("x ~arima(", p, ",0,", q, ")", sep = "")
            fit = armaFit(as.formula(Formula), method = "ML")@fit
            AIC[p+1, q+1] = log(fit$sigma2) + 2*(p+q) / N
            BIC[p+1, q+1] = log(fit$sigma2) + log(N)*(p+q) / N
        }
    }
    rownames(AIC) = colnames(AIC) = as.character(0:(n-1))
    print(round(max(AIC) - AIC, 3))
    rownames(BIC) = colnames(BIC) = as.character(0:(n-1))
    print(round(max(BIC) - BIC, 3))
    
    
# ------------------------------------------------------------------------------


### Mills 2.4: Modelling the UK Spread as an Integrated Process 

    # Load Data:
    data(R20); data(RS)
    x = diff((R20-RS)[, 1])
    
    # Sample ACF and Sample PACF:
    data.frame(round(rbind( 
        "r(k)" = acf(x, lag.max = 12)$acf[,,1][-1], 
        "phi(kk)" = pacf(x, lag.max = 12)$acf[,,1]), 3))
        
    # Fit AR(1):
    fit = arima(x, order = c(1, 0, 0))
    fit 
    
    # Portmanteau Statistic of Q(12):
    Box.test(x = fit$residuals, lag = 12, "Ljung-Box")$statistic

    
# ------------------------------------------------------------------------------


### Mills 2.5: Modelling the Dollar/Sterling Exchange Rate

    par (mfrow = c(2, 2), cex = 0.7)
    data(EXCHD)
    x = EXCHD[,1]
    ts.plot(x)
    
    x = diff(x)
    plot(x, type = "l", ylim = c(-0.1, 0.1))
    grid()
    
    
# ------------------------------------------------------------------------------


### Mills 2.6: Modelling the FTA ALL Share Index


    data(FTAPRICE)
    x = FTAPRICE[,1]
    par (mfrow = c(2, 2), cex = 0.7)
    ts.plot(x)
    ts.plot(x, log = "y")
    
    x = diff(log(x))
    arima(x, order = c(3, 0, 0))
    
    # Sample ACF and Sample PACF:
    data.frame(round(rbind( 
        "r(k)" = acf(x, lag.max = 12)$acf[,,1][-1], 
        "phi(kk)" = pacf(x, lag.max = 12)$acf[,,1]), 3))
                
    
# ------------------------------------------------------------------------------


### Mills 2.7: ARIMA Forecasting of Financial Time Series

    # Time Series Data:
    data(R20); data(RS)
    x = (R20-RS)[, 1]
    x[525:526]
    
    fit = armaFit(x ~ ar(2), "ols", include.mean = FALSE)   
    pred = predict(fit, 5)
    
    fit = armaFit(x ~ ar(2), "ols", include.mean = TRUE)    
    pred = predict(fit, 5)
    

################################################################################

