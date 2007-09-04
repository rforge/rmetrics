
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General
# Public License along with this library; if not, write to the
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston,
# MA  02111-1307  USA

# Copyrights (C)
# for this R-port: 
#   1999 - 2007, Diethelm Wuertz, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# METHODS:                DESCRIPTION:
#  show.fGARCH             S4 print method for an object of class 'fGARCH'
#  summary.fGARCH          S3 summary method for an object of class 'fGARCH'
#  plot.fGARCH             S3 plot method for an object of class 'fGARCH'
#  .interactiveGarchPlot   Utility Function
#  residuals.fGARCH        S3 residuals method for an object of class 'fGARCH'
#  fitted.fGARCH           S3 fitted values for an object of class 'fGARCH'
#  predict.fGARCH          S3 prediction method for an object of class 'fGARCH'
# STATISTICS:             Description:
#  .truePersistence        Computes persistence
################################################################################


show.fGARCH = 
function(object) 
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Print method for an object of class "fGARCH"
    
    # Arguments:
    #   object - an object of class 'fGARCH'
    
    # FUNCTION:
     
    # Title:
    cat("\nTitle:\n ")
    cat(object@title, "\n")
    
    # Call:
    cat("\nCall:\n ")
    cat(paste(deparse(object@call), sep = "\n", collapse = "\n"), "\n")
    
    # Mean Equation:
    cat("\nMean and Variance Equation:\n ")
    cat(as.character(object@formula[1]), "+", 
        as.character(object@formula[2]), "\n")
        
    # Conditional Distribution:
    cat("\nConditional Distribution:\n ")
    cat(object@fit$params$cond.dist, "\n")
  
    # Coefficients:
    cat("\nCoefficient(s):\n")
    digits = max(6, getOption("digits") - 4)
    print.default(format(object@fit$par, digits = digits), print.gap = 2, 
         quote = FALSE)    
    
    # Error Analysis:
    digits = max(4, getOption("digits") - 5)
    fit = object@fit 
    signif.stars = getOption("show.signif.stars")
    cat("\nError Analysis:\n")
    printCoefmat(fit$matcoef, digits = digits, signif.stars = signif.stars) 
    
    # Log Likelihood:
    cat("\nLog Likelihood:\n ")
    LLH = object@fit$value
    N = length(object@data$x)
    cat(LLH, "   normalized: ", LLH/N, "\n")
        
    # Description:
    cat("\nDescription:\n ")
    cat(object@description, "\n")

    # Return Value:
    cat("\n")
    invisible()
}


setMethod("show", "fGARCH", show.fGARCH)


# ------------------------------------------------------------------------------


summary.fGARCH = 
function(object, ...) 
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Print method for an object of class "fGARCH"
    
    # Arguments:
    #   object - an object of class 'fGARCH'
    
    # FUNCTION:
     
    # Title:
    cat("\nTitle:\n ")
    cat(object@title, "\n")
    
    # Call:
    cat("\nCall:\n ")
    cat(paste(deparse(object@call), sep = "\n", collapse = "\n"), "\n")
    
    # Mean Equation:
    cat("\nMean and Variance Equation:\n ")
    cat(as.character(object@formula[1]), "+", 
        as.character(object@formula[2]), "\n")
        
    # Conditional Distribution:
    cat("\nConditional Distribution:\n ")
    cat(object@fit$params$cond.dist, "\n")
  
    # Coefficients:
    cat("\nCoefficient(s):\n")
    digits = max(6, getOption("digits") - 4)
    print.default(format(object@fit$par, digits = digits), print.gap = 2, 
         quote = FALSE)    
    
    # Error Analysis:
    digits = max(4, getOption("digits") - 5)
    fit = object@fit 
    # fit$cvar = solve(fit$hessian)
    # fit$se.coef = sqrt(diag(fit$cvar))
    # fit$tval = fit$coef/fit$se.coef
    # fit$matcoef = cbind(fit$coef, fit$se.coef, 
    #     fit$tval, 2*(1-pnorm(abs(fit$tval))))
    # dimnames(fit$matcoef) = list(names(fit$tval), c(" Estimate", 
    #    " Std. Error", " t value", "Pr(>|t|)"))
    signif.stars = getOption("show.signif.stars")
    cat("\nError Analysis:\n")
    printCoefmat(fit$matcoef, digits = digits, signif.stars = signif.stars) 
    
    # Log Likelihood:
    cat("\nLog Likelihood:\n ")
    LLH = object@fit$value
    N = length(object@data$x)
    cat(LLH, "   normalized: ", LLH/N, "\n")
        
    # Lagged Series:
    .tslagGarch = function (x, k = 1) {
        ans = NULL
        for (i in k) ans = cbind(ans, .tslag1Garch(x, i))
        indexes = (1:length(ans[, 1]))[!is.na(apply(ans, 1, sum))]
        ans = ans[indexes, ]
        if (length(k) == 1) ans = as.vector(ans)
        ans }
    .tslag1Garch = function (x, k) {
        c(rep(NA, times = k), x[1:(length(x) - k)]) }
        
    # Statistical Tests:
    cat("\nStandadized Residuals Tests:\n")
    r.s = object@residuals/sqrt(object@h.t)
    ans = NULL
    # Normality Tests:
    jbtest = jarqueberaTest(r.s)@test
    ans = rbind(ans, c(jbtest[1], jbtest[2]))
    if (length(r.s) < 5000) {
        swtest = shapiro.test(r.s)
        if (swtest[2] < 2.6e-16) swtest[2] = 0
        ans = rbind(ans, c(swtest[1], swtest[2]))
    } else {
        ans = rbind(ans, c(NA, NA))
    }
    # Ljung-Box Tests:
    box10 = Box.test(r.s, lag = 10, type = "Ljung-Box")
    box15 = Box.test(r.s, lag = 15, type = "Ljung-Box")
    box20 = Box.test(r.s, lag = 20, type = "Ljung-Box")
    ans = rbind(ans, c(box10[1], box10[3]))
    ans = rbind(ans, c(box15[1], box15[3]))
    ans = rbind(ans, c(box20[1], box20[3]))
    box10 = Box.test(r.s^2, lag = 10, type = "Ljung-Box")
    box15 = Box.test(r.s^2, lag = 15, type = "Ljung-Box")
    box20 = Box.test(r.s^2, lag = 20, type = "Ljung-Box")
    ans = rbind(ans, c(box10[1], box10[3]))
    ans = rbind(ans, c(box15[1], box15[3]))
    ans = rbind(ans, c(box20[1], box20[3]))
    # Ljung-Box Tests - tslag required 
    lag.n = 12
    x.s = as.matrix(r.s)^2
    n = nrow(x.s)
    tmp.x = .tslagGarch(x.s[, 1], 1:lag.n)
    tmp.y = x.s[(lag.n + 1):n, 1]
    fit = lm(tmp.y ~ tmp.x)
    stat = (n-lag.n) * summary.lm(fit)$r.squared
    ans = rbind(ans, c(stat, p.value = 1 - pchisq(stat, lag.n)) )
    # Add Names:
    rownames(ans) = c(
        " Jarque-Bera Test   R    Chi^2 ",
        " Shapiro-Wilk Test  R    W     ",
        " Ljung-Box Test     R    Q(10) ",
        " Ljung-Box Test     R    Q(15) ",
        " Ljung-Box Test     R    Q(20) ",
        " Ljung-Box Test     R^2  Q(10) ",
        " Ljung-Box Test     R^2  Q(15) ",
        " Ljung-Box Test     R^2  Q(20) ",
        " LM Arch Test       R    TR^2  ")
    colnames(ans) = c("Statistic", "p-Value")
    print(ans)
    
    # Information Criterion Statistics:
    cat("\nInformation Criterion Statistics:\n")
    print(object@fit$ics)
        
    # Description:
    cat("\nDescription:\n ")
    cat(object@description, "\n")

    # Return Value:
    cat("\n")
    invisible()
}


# ------------------------------------------------------------------------------


plot.fGARCH =
function(x, which = "ask", ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Plot method for an object of class 'fGARCH'
    
    # Note:
    #   This method can also be used for plotting graphs fitted by 
    #   the function 'garch' from the contributed R package 'tseries'.
    
    # FUNCTION:
        
    # Plot:
    .interactiveGarchPlot(
        x,
        choices = c(
            "Time Series",
            "Conditional SD",
            "Series with 2 Conditional SD Superimposed",
            "ACF of Observations",
            "ACF of Squared Observations",
            "Cross Correlation",
            "Residuals",
            "Conditional SDs",
            "Standardized Residuals",
            "ACF of Standardized Residuals",
            "ACF of Squared Standardized Residuals",
            "Cross Correlation between r^2 and r",
            "QQ-Plot of Standardized Residuals"),
        plotFUN = c(
            ".plot.garch.1", ".plot.garch.2", ".plot.garch.3", 
            ".plot.garch.4", ".plot.garch.5", ".plot.garch.6",  
            ".plot.garch.7", ".plot.garch.8", ".plot.garch.9", 
            ".plot.garch.10",".plot.garch.11", ".plot.garch.12", 
            ".plot.garch.13"),
        which = which, ...) 
            
    # Return Value:
    invisible(x)
}


.plot.garch.1 <- 
function(x, ...) 
{
    # 1. Time Series:
    xseries = x@data$x
    plot(xseries, type = "l", col = "steelblue", ylab = "x",
        main = "Time Series")
    abline(h = 0, col = "grey", lty = 3)
    grid()
}    

   
.plot.garch.2 <- 
function(x, ...) 
{
    # 2. Conditional SD:
    xcsd = x@sigma.t
    plot(xcsd, type = "l", col = "steelblue", ylab = "x",
        main = "Conditional SD")
    abline(h = 0, col = "grey", lty = 3)
    grid()
}   


.plot.garch.3 <- 
function(x, ...) 
{           
    # 3. Series with 2 Conditional SD Superimposed:
    xseries = x@data$x
    xcsd = x@sigma.t
    ci = 2
    plot(xseries, type = "l", col = "steelblue", ylab = "x",
        main = "Series with 2 Conditional SD Superimposed")
    lines(+ci * xcsd, col = "grey")
    lines(-ci * xcsd, col = "grey")
    abline(h = 0, col = "grey", lty = 3)
    grid()
}     

      
.plot.garch.4 <- 
function(x, ...) 
{        
    # 4. ACF of the Observations:
    xseries = x@data$x
    n = length(xseries)
    lag.max = as.integer(10*log10(n))
    acf(xseries, lag.max = lag.max, xlab = "Lags", col = "steelblue", 
        main = "ACF of Observations", plot = TRUE)
}   


.plot.garch.5 <- 
function(x, ...) 
{       
    # 5. ACF of the Squared Observations:
    xseries = x@data$x
    xseries2 = xseries^2
    n = length(xseries)
    lag.max = as.integer(10*log10(n))
    acf(xseries2, lag.max = lag.max, xlab = "Lags", col = "steelblue", 
        main = "ACF of Squared Observations", plot = TRUE)
} 

          
.plot.garch.6 <- 
function(x, ...) 
{
    # 6. Cross Correlation between x^2 and x:
    xseries = x@data$x
    xseries2 = xseries^2
    n = length(xseries)
    lag.max = as.integer(10*log10(n))
    ccf(xseries2, xseries, lag.max = lag.max, xlab = "Lags", 
        main = "Cross Correlation", plot = TRUE, col = "steelblue")
}


.plot.garch.7 <- 
function(x, ...) 
{
    # 7. Residuals:
    res = residuals(x, standardize = FALSE)
    plot(res, type = "l", main = "Residuals", col = "steelblue", ...)
    abline(h = 0, lty = 3)
    grid()
}  

 
.plot.garch.8 <- 
function(x, ...) 
{
    # 8. Conditional SDs:
    xcsd = x@sigma.t
    plot(xcsd, type = "l", main = "Conditional SD's", 
        col = "steelblue", ...)
    abline(h = 0, lty = 3)
    grid()
}   


.plot.garch.9 <- 
function(x, ...) 
{
    # 9. Standardized Residuals:
    sres = residuals(x, standardize = FALSE)
    plot(sres, type = "l", main = "Standardized Residuals", 
        col = "steelblue", ...)
    abline(h = 0, lty = 3)
    grid()
} 

      
.plot.garch.10 <- 
function(x, ...) 
{
    # 10. ACF of Standardized Residuals:
    sres = residuals(x, standardize = FALSE)
    n = length(sres)
    lag.max = as.integer(10*log10(n))
    acf(sres, lag.max = lag.max, xlab = "Lags", col = "steelblue", 
        main = "ACF of Standardized Residuals", plot = TRUE)
}  

         
.plot.garch.11 <- 
function(x, ...) 
{
    # 11. ACF of Squared Standardized Residuals:
    sres2 = residuals(x, standardize = FALSE)^2
    n = length(sres2)
    lag.max = as.integer(10*log10(n))
    acf(sres2, lag.max = lag.max, xlab = "Lags", col = "steelblue", 
        main = "ACF of Standardized Residuals", plot = TRUE)
}    

       
.plot.garch.12 <- 
function(x, ...) 
{      
    # 12. Cross Correlation between r^2 and r:
    sres = residuals(x, standardize = FALSE)
    sres2 = sres^2
    n = length(sres)
    lag.max = as.integer(10*log10(n))
    ccf(sres2, sres, lag.max = lag.max, xlab = "Lags", 
        main = "Cross Correlation", plot = TRUE, col = "steelblue")
}   


.plot.garch.13 <- 
function(x, ...) 
{
    # 13. QQ-Plot of Standardized Residuals:
    sres = residuals(x, standardize = FALSE)
    cond.dist = x@fit$params$cond.dist
    nc = nchar(x@fit$params$cond.dist)
    cond.dist = paste("q", substr(cond.dist, 2, nc), sep = "")
    skew = x@fit$params$skew
    shape = x@fit$params$shape
    if (cond.dist == "qnorm")
        .qqDist(sres, dist = cond.dist)
    if (cond.dist == "qstd" | cond.dist == "qged")
        .qqDist(sres, dist = cond.dist, nu = shape)
    if (cond.dist == "qsnorm")
        .qqDist(sres, dist = cond.dist, xi = skew)
    if (cond.dist == "qsstd" | cond.dist == "qsged")
        .qqDist(sres, dist = cond.dist, xi = skew, nu = shape)
}



# ------------------------------------------------------------------------------


.qqDist = 
function (y, dist = "qnorm", ylim = NULL, main = paste(dist, "- QQ Plot"), 
xlab = "Theoretical Quantiles", ylab = "Sample Quantiles", doplot = TRUE, 
datax = FALSE, ...) 
{   # A function implemented by Diethelm Wuertz
    
    # Description
    #   QQ Plot for arbitray distribution
    
    # FUNCTION:
    # print(dist)
    
    # Match Function :
    qDist = match.fun(dist)
    
    # Check Arguments:
    if (substr(dist, 1, 1) != "q") stop("dist is misspecified")
    # test = class(test = try(qDist(0.5, ...), silent = TRUE))
    # if (test == "try-error") stop("dist does not exist")
    
    # Transform to Vector Mode:
    y = as.vector(y)
    
    # Compute Data:
    if (has.na <- any(ina <- is.na(y))) {
        yN = y
        y = y[!ina]
    }
    if (0 == (n <- length(y))) stop("y is empty or has only NAs")
    x <- qDist(ppoints(n,), ...)[order(order(y))]
    if (has.na) {
        y = x
        x = yN
        x[!ina] = y
        y = yN
    }
    
    # Create QQ Plot:
    if (doplot) { 
        if (is.null(ylim)) ylim = range(y)
        if (datax) {
            plot(y, x, main = main, xlab = ylab, ylab = xlab, xlim = ylim,
                col = "steelblue", cex = 0.7)
        } else {
            plot(x, y, main = main, xlab = xlab, ylab = ylab, ylim = ylim,
                col = "steelblue", cex = 0.7)
        }
        .qqLine(y = y, dist = dist, datax = datax, ...)
        grid()
    }
    
    # Return Value:
    invisible(if (datax) list(x = y, y = x) else list(x = x, y = y))
}


# ------------------------------------------------------------------------------


.qqLine = 
function (y, dist = "qnorm", datax = FALSE, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description
    #   Add slope to QQ Plot for arbitray distribution
    
    # FUNCTION:
    
    # Match Function :
    qDist = match.fun(dist)
    
    # Check Arguments:
    if (substr(dist, 1, 1) != "q") stop("dist is misspecified")
    # test = class(test = try(qDist(0.5, ...), silent = TRUE))
    # if (test == "try-error") stop("dist does not exist")
    
    # Transform to Vector Mode:
    y = as.vector(y)
    
    # Compute Data:
    y = quantile(y[!is.na(y)], c(0.25, 0.75))
    x = qDist(c(0.25, 0.75), ...)
    
    # Add Slope:
    if (datax) {
        slope <- diff(x)/diff(y)
        int <- x[1] - slope * y[1]
    } else {
        slope <- diff(y)/diff(x)
        int <- y[1] - slope * x[1]
    }
    
    # Return Value:
    abline(int, slope)
}


# ------------------------------------------------------------------------------


.interactiveGarchPlot = 
function(x, choices = paste("Plot", 1:19), 
plotFUN = paste("plot.", 1:19, sep = ""), which = "all", ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Plot method for an object of class "template".
    
    # Arguments:
    #   x - an object to be plotted
    #   choices - the character string for the choice menu
    #   plotFUN - the names of the plot functions
    #   which - plot selection, which graph should be 
    #     displayed. If a character string named "ask" the 
    #     user is interactively asked which to plot, if
    #     a logical vector of length N, those plots which
    #     are set "TRUE" are displayed, if a character string
    #     named "all" all plots are displayed.
    
    # Note:
    #   At maximum 19 plots are supported.

    # FUNCTION:
    
    # Some cecks:
    if (length(choices) != length(plotFUN)) 
        stop("Arguments choices and plotFUN must be of same length.")
    if (length(which) > length(choices)) 
        stop("Arguments which has incorrect length.")
    if (length(which) > length(plotFUN)) 
        stop("Arguments which has incorrect length.")
    if (length(choices) > 19)
        stop("Sorry, only 19 plots at max are supported.")
                  
    # Plot:
    if (is.numeric(which)) {
        Which = rep(FALSE, times = length(choices))
        Which[which] = TRUE
        which = Which
    }
    if (which[1] == "all") {
        which = rep(TRUE, times = length(choices))
    }
    if (which[1] == "ask") {
        .multGarchPlot(x, choices, ...) 
    } else {
        for ( i in 1:length(which) ) {
            FUN = match.fun(plotFUN[i])
            if (which[i]) FUN(x) 
        } 
    }
            
    # Return Value:
    invisible(x)
}


.multGarchPlot = function (x, choices, ...) 
{    
    # Match Functions, up to 19 ...
    if (length(plotFUN) < 19) plotFUN = 
        c(plotFUN, rep(plotFUN[1], times = 19 - length(plotFUN)))
    plot.1  = match.fun(plotFUN[1]);  plot.2  = match.fun(plotFUN[2]) 
    plot.3  = match.fun(plotFUN[3]);  plot.4  = match.fun(plotFUN[4]) 
    plot.5  = match.fun(plotFUN[5]);  plot.6  = match.fun(plotFUN[6]) 
    plot.7  = match.fun(plotFUN[7]);  plot.8  = match.fun(plotFUN[8]) 
    plot.9  = match.fun(plotFUN[9]);  plot.10 = match.fun(plotFUN[10])
    plot.11 = match.fun(plotFUN[11]); plot.12 = match.fun(plotFUN[12]) 
    plot.13 = match.fun(plotFUN[13]); plot.14 = match.fun(plotFUN[14]) 
    plot.15 = match.fun(plotFUN[15]); plot.16 = match.fun(plotFUN[16]) 
    plot.17 = match.fun(plotFUN[17]); plot.18 = match.fun(plotFUN[18]) 
    plot.19 = match.fun(plotFUN[19])        
    pick = 1
    while (pick > 0) { 
        pick = menu (
            ### choices = paste("plot:", choices),
            choices = paste(" ", choices), 
            title = "\nMake a plot selection (or 0 to exit):")
        # up to 19 plot functions ...
        switch (pick, 
            plot.1(x),  plot.2(x),  plot.3(x),  plot.4(x),  plot.5(x), 
            plot.6(x),  plot.7(x),  plot.8(x),  plot.9(x),  plot.10(x),
            plot.11(x), plot.12(x), plot.13(x), plot.14(x), plot.15(x), 
            plot.16(x), plot.17(x), plot.18(x), plot.19(x)) 
    } 
}
                

# ------------------------------------------------------------------------------


residuals.fGARCH = 
function(object, ...) 
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   S3 Residuals method for an object of class fGARCH

    # FUNCTION:
    
    # Return Value:
    .residuals.fGARCH(object = object, ...) 
}


# ------------------------------------------------------------------------------


.residuals.fGARCH = 
function(object, standardize = FALSE) 
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   S3 Residuals method for an object of class fGARCH

    # FUNCTION:
    
    # Residuals:
    if (standardize) {
        ans = object@residuals/object@sigma.t
    } else {
        ans = object@residuals
    }
    
    # Return Value:
    ans
    
}

    
# ------------------------------------------------------------------------------


fitted.fGARCH = 
function(object, ...) 
{   # A function implemented by Diethelm Wuertz
    
    # Description:  
    #   S3 Fitted values method for an object of class fGARCH
    
    # FUNCTION:
    
    # Fitted Values:
    ans = object@fitted
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


predict.fGARCH = 
function(object, n.ahead = 10, trace = FALSE, ...)
{   # A function implemented by Diethelm Wuertz
    
    # Description:  
    #   S3 Prediction method for an object of class fGARCH
    
    # Arguments:
    #   object - an object of class fGARCH as returned by the
    #       function garchFit().
    #   n.ahead - number of steps to be forecasted, an integer
    #       value, by default 10)
    #   trace - should the prediction be traced? A logical value,
    #       by default FALSE)
    
    # FUNCTION:
    
    # Retrieve "fit" from Parameter Estimation:
    fit = object@fit
    
    # Get ARMA(u,v)-GARCH(p,q) Order:
    u = fit$series$order[1]
    v = fit$series$order[2]
    p = fit$series$order[3]
    q = fit$series$order[4]
    max.order = max(u, v, p, q)
    
    # Get Start Conditions:
    h.start = fit$series$h.start
    llh.start = fit$series$llh.start  
    index = fit$params$index
    params = fit$params$params
    par = fit$par
    Names = names(index)
    for (Name in Names) params[Name] = par[Name]
    Names = names(params)
    
    # Retrieve From Initialized Parameters:
    cond.dist = fit$params$cond.dist
    
    # Extract the Parameters by Name:
    leverage = fit$params$leverage
    mu = params["mu"]
    if (u > 0) {
        ar = params[substr(Names, 1, 2) == "ar"] 
    } else {
        ar = c(ar1 = 0)
    }
    if (v > 0) {
        ma = params[substr(Names, 1, 2) == "ma"] 
    } else {
        ma = c(ma1 = 0)
    }
    omega = params["omega"]
    if (p > 0) {
        alpha = params[substr(Names, 1, 5) == "alpha"] 
    } else {
        alpha = c(alpha1 = 0)
    }
    if (p > 0 & leverage) {
        gamma = params[substr(Names, 1, 5) == "gamma"] 
    } else {
        gamma = c(gamma1 = 0)
    }
    if (q > 0) {
        beta  = params[substr(Names, 1, 4) == "beta"] 
    } else {
        beta = c(beta1 = 0)
    }
    delta = params["delta"]
    skew = params["skew"]
    shape = params["shape"]
    
    # Trace Parameters:
    if (trace) {
        cat("\nModel Parameters:\n")
        print(c(mu, ar, ma, omega, alpha, gamma, beta, delta, skew, shape))
    }
    
    # Retrieve Series Lengths:
    M = n.ahead
    N = length(object@data$x)
    
    # Get and Extend Series:
    x = c(object@data$x, rep(mu, M))
    h = c(object@h.t, rep(0, M))
    z = c(fit$series$z, rep(mu, M))
    
    # Forecast and Optionally Trace Mean Model:
    # Note we set maxit=0 to get an object of class Arima with fixed
    #   init parameters ...
    ARMA = arima(x = object@data$x, order = c(max(u, 1), 0, max(v, 1)), 
        init = c(ar, ma, mu), transform.pars = FALSE, optim.control = 
        list(maxit = 0))
    prediction = predict(ARMA, n.ahead)
    meanForecast = as.vector(prediction$pred)
    meanError = as.vector(prediction$se)
    if (trace) {
        cat("\nForecast ARMA Mean:\n") 
        print(ARMA)
        cat("\n")
        print(prediction)
    }
    
    # Forecast and Optionally Trace Variance Model:
    var.model = fit$series$model[2] 
    # Forecast GARCH Variance:
    if (var.model == "garch") {
        if (trace) cat("\nForecast GARCH Variance:\n")
        for (i in 1:M) {
            h[N+i] = omega  + sum(beta*h[N+i-(1:q)])
            for (j in 1:p) {
                if (i-j > 0) {
                    s = h[N + i - j]
                } else { 
                    s = z[N + i - j]^2
                }
                h[N+i] = h[N+i] + alpha[j] * s
            }
        }
    }    
    # Forecast APARCH Variance:
    if (var.model == "aparch") {
        if (trace) cat("\nForecast APARCH Variance:\n")
        for (i in 1:M) {
            h[N+i] = omega  + sum(beta*h[N+i-(1:q)])
            for (j in 1:p) {
                kappa = garchKappa(cond.dist = "dnorm", gamma = gamma[j],
                    delta = delta, skew = skew, shape = shape)
                if (i-j > 0) {
                    s = kappa * h[N + i - j]
                } else { 
                    s = kappa 
                }
                h[N+i] = h[N+i] + alpha[j] * s
            }
        }
    }
    
    # Standard Deviations:
    standardDeviation = h^(1/delta)
        
    # Result:
    forecast = data.frame(
        meanForecast = meanForecast, 
        meanError = meanError, 
        standardDeviation = standardDeviation[-(1:N)])
    
    # Return Value:
    forecast
}


# ------------------------------------------------------------------------------


.truePersistence =
function(fun = "dnorm", alpha = 1, gamma = 0, beta = 0, delta = 1, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes persistence for an APARCH process
    
    # Arguments:
    #   fun - name of density functions of APARCH innovations
    #   alpha, gamma - numeric value or vector of APARCH coefficients,
    #       must be of same length  
    #   beta - numeric value or vector of APARCH coefficients
    #   delta - numeric value of APARCH exponent
    
    # Note:
    #   fun is one of: dnorm, dsnorn, dstd, dsstd, dged, dsged
    
    # FUNCTION:
    
    # Match Density Function:
    fun = match.fun(fun)
    
    # Persisgtence Function: E(|z|-gamma z)^delta
    e = function(x, gamma, delta, ...) {
        (abs(x)-gamma*x)^delta * fun(x, ...)
    }
        
    # Compute Persistence by Integration:
    persistence = sum(beta)
    for (i in 1:length(alpha)) {
        I = integrate(e, -Inf, Inf, subdivisions = 1000, 
            rel.tol = .Machine$double.eps^0.5, 
            gamma = gamma[i], delta = delta, ...)
        persistence = persistence + alpha[i] * I[[1]]
    }
    
    # Warning:
    if (persistence >= 1) {  
        p = as.character(round(persistence, digits = 3))
        warning(paste("Divergent persistence p =", p))
    }
    
    # Return Value:
    persistence
}


################################################################################

