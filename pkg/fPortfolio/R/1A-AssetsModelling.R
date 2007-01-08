
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
# FUNCTION:             ASSETS SELECTION:
#  assetsSelect          Selects individual assets from a set of assets
#   method = "hclust"     hierarchical clustering of returns
#   method = "kmeans"     k-means clustering of returns
#  assetsHistPlot        Displays histograms of individual assets 
#  assetsQQNormPlot      Displays normal qq-plots of individual assets
#  assetsPairsPlot       Displays pairs of scatterplots of individual assets
#  assetsCorTestPlot     Displays and tests pairwise correlations of assets
# FUNCTION:             SIMULATION AND PARAMETER ESTIMATION:
#  'fASSETS'             Class representation for "fASSETS" Objects
#  assetsSim             Simulates a set of artificial assets
#  assetsFit             Estimates the parameters of set of assets
#   method = "norm"       assuming a multivariate Normal distribution
#   method = "snorm"      assuming a multivariate skew-Normal distribution
#   method = "st"         assuming a multivariate skew-Student-t  
#  print.fASSETS         S3: Print method for an object of class fASSETS
#  plot.fASSETS          S3: Plot method for an object of class fASSETS
#  summary.fASSETS       S3: Summary method for an object of class fASSETS
# FUNCTION:             ASSETS STATISTICS:
#  assetsStats           Computes basic statistics of a set of asset  
#  assetsMeanCov         Estimates mean and variance for a set of assets
#   method = "cov"        uses standard covariance estimation
#   method = "mve"        uses
#   method = "mcd"        uses
#   method = "nne"        uses
#   method = "shrink"     uses shrinkage
#   method = "bagged"     uses bagging
# FUNCTION:             ASSETS NORMALITY TESTS:
#  assetsTest            Test for multivariate Normal Assets
#   method = "shapiro"    calling Shapiro test
#   method = "energy"     calling E-Statistic (energy) test
#  .mvenergyTest         Multivariate Energy Test
#   .mvnorm.etest         Internal Function used by assetsTest
#   .mvnorm.e             Internal Function used by assetsTest
#   .normal.e             Internal Function used by assetsTest
#   .mvnormBoot           Internal Function used by assetsTest
#  .mvshapiroTest        Multivariate Shapiro Test
# REQUIREMENTS:         DESCRIPTION:
#  .msn.quantities       Function from R package sn [in fMultivar]      
#  copcor                R contributed package copcor
#  covRobust             R contributed package covRobust
################################################################################


assetsSim =
function(n, dim = 2, model = list(mu = rep(0, dim), Omega = diag(dim), 
alpha = rep(0, dim), df = Inf), assetNames = NULL) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Simulates a multivariate set of asset log-returns distributed
    #   according to a Normal, skew-Normal, or skew Student-t Distribution 
    #   Function.
    
    # Arguments:
    #   n - the number of data records to be simulated
    #   dim - the dimension number, i.e. the number of assets to be simulated
    #   model - a list with the model parameters:
    #       mu - the numeric vector of mean values of each asset time series
    #       Omega - the covariance matrix of assets
    #       alpha - the skewness vector
    #       df - the degrees of freedom, a measures for the kurtosis
    #   assetNames - a string vector of asset names, by default NULL
    #       which creates asset names as "V1", "V2", ..., "Vd", where
    #       d denotes the dimension
    
    # Notes: 
    #   Requires function "msn.quantities" from R's GPL licensed 
    #     contributed package "sn", (C) 1998-2004 A. Azzalini.
    #   The model can also be the value returned by model slot from
    #     function assetsFit().
    
    # Example: 
    #   assetsSim(n=25)
    #   assetsSim(n=25, assetNames = c("RETURN-1", "RETURN-2")
    #   assetsSim(n=25, list(mu=c(0,0), Omega=diag(2), alpha=c(0,0), df=4)) 
    
    # FUNCTION:
    
    # Dimensions:
    d = length(model$alpha)
    if ( length(model$mu) != d | any(dim(model$Omega) != c(d, d))) 
        stop("dimensions of arguments do not match")
    
    # Adapted from contributed R package "sn:rmsn"
    Z = .msn.quantities(model$mu, model$Omega, model$alpha)
    y = matrix(rnorm(n * d), n, d) %*% chol(Z$Psi)
    abs.y0 = matrix(rep(abs(rnorm(n)), d), ncol = d)
    z = Z$delta * t(abs.y0) + sqrt(1 - Z$delta^2) * t(y)
    
    # Select:
    if (model$df == Inf) {      
        ans = t(model$mu + Z$omega * z) 
    } else {
        x = rchisq(n, model$df)/model$df
        z = t(model$mu + Z$omega * z)
        ans = t(model$mu + t(sqrt(x) * z)) 
    }
        
    # Dimnames:
    dimnames(ans)[[2]] = assetNames 
    
    # Return Value:
    as.data.frame(ans)
}


# ------------------------------------------------------------------------------


assetsSelect = 
function(x, method = c("hclust", "kmeans"),
kmeans.centers = 5, kmeans.maxiter = 10, doplot = TRUE, ...)
{   # A function implemented by Diethelm Wuertz

    # Description: 
    #   Clusters a set of assets
    
    # FUNCTION:

    # Transform to matrix:
    if (class(x) == "timeSeries") {
        x = as.matrix(x)
    }
    
    # stats::hclust
    # Hierarchical cluster analysis on a set of dissimilarities 
    # and methods for analyzing it.     
    if (method[1] == "hclust") {
        ans = hclust(dist(t(x)), ...)
        if (doplot) {
            plot(ans)   
        }
    }
    
    # stats::kmeans
    # Perform k-means clustering on a data matrix   
    if (method[1] == "kmeans") {
        ans = kmeans(x = t(x), centers = kmeans.centers, 
            iter.max = kmeans.maxiter, ...)
        if (doplot) {
            plot(t(x), col = ans$cluster)
            points(ans$centers, col = 1:(kmeans.centers), pch = 8, cex = 2)

        }
    }
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------

    
.assetsHistPlot =
function(x, method = c("mve", "mcd", "classical"), which = 1:dim(x)[2],
labels = TRUE, xlim = NULL, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Displays histograms of individual assets 
    
    # Arguments:
    #   x - a timeSeries object or any other rectangular object
    #       which can be transformed by the function as. matrix
    #       into a numeric matrix.
    #   method - the method to be used. Minimum volume ellipsoid
    #       "mve", minimum covariance determinant "mcd", or 
    #       classical product-moment.
    #   which - an integer value or vector specifying the number(s)
    #       of the assets which are selected to be plotted. 
    #   labels - a logical flag. Should default labels be printed?
    
    # FUNCTION:
    
    # Settings:
    x = as.matrix(x)
    if (is.null(xlim)) xlim = c(min(x), max(x))
    covRob = cov.rob(x, method = method)
    dvar = diag(covRob$cov)
    index = which.min(dvar)[[1]]
    MEAN = covRob$center[[index]]
    SD = sqrt(min(dvar))
    ylim = c(0, dnorm(MEAN, MEAN, SD))
    
    # Plot:
    for (N in which) {
        hist(x[, N], probability = TRUE, 
            col = "steelblue", border = "white", 
            xlim = xlim, ylim = ylim,
            main = "", xlab = "", ylab = "", ...)
        if (labels) {
            title(
                main = colnames(x)[N],
                xlab = "Return",
                ylab = "Density")
        }

        # Classical:
        covCla = cov.rob(x, method = "classical")
        u = seq(xlim[1], xlim[2], length.out = 501)
        v = dnorm(u, mean = covCla$center[N], sd = sqrt(covCla$cov[N, N]))
        print(cbind(u,v))
        lines(u, v, col = "brown")
        abline(v = covRob$center[N], col = "brown")
        
        # Robust:
        v = dnorm(u, mean = covRob$center[N], sd = sqrt(covRob$cov[N, N]))
        abline(v = covRob$center[N], col = "orange")
        lines(u, v, col = "orange")
    }
    
    # Control:
    control = list(fun = assetsHistPlot, x = x, method = method,
        which = which, labels = labels)
        
    # Return Value:
    invisible(control)
}


# ------------------------------------------------------------------------------


.assetsQQNormPlot =
function(x, which = 1:dim(x)[2], labels = TRUE, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Displays normal qq-plots of individual assets
    
    # Arguments:
    #   x - a timeSeries object or any other rectangular object
    #       which can be transformed by the function as. matrix
    #       into a numeric matrix.
    #   method - the method to be used. Minimum volume ellipsoid
    #       "mve", minimum covariance determinant "mcd", or 
    #       classical product-moment. 
    #   which - an integer value or vector specifying the number(s)
    #       of the assets which are selected to be plotted. 
    #   labels - a logical flag. Should default labels be printed?
    
    # FUNCTION:
    
    # Settings:
    x = as.matrix(x)
    ylim = c(min(x), max(x))
    
    # Plot:
    for (i in which) {
        X = qqnorm(y = x[, i], plot.it = FALSE)
        if (labels) {
            plot(X$x, X$y, main = "", xlab = "", ylab = "",
                col = "steelblue", pch = 19, ylim = ylim, ...)
            grid() 
        } else {
            plot(X$x, X$y, main = "", xlab = "", ylab = "", ...)
        }
        qqline(x[, i]) 
        fit = ltsreg(X$x, X$y)
        abline(fit, col = "brown")
        fit = ltsreg(X$x, X$y)
        abline(fit, col = "orange")  
        # Add Labels:
        if (labels) {
            title(main = colnames(x)[i], 
                xlab = "Theoretical Quantiles",
                ylab = "Sample Quantiles")
        }
    }
    
    # Control:
    control = list(fun = assetsHistPlot, x = x, which = which, 
        labels = labels)
        
    # Return Value:
    invisible(control)
}


# ------------------------------------------------------------------------------


.assetsPairsPlot =
function(x, labels = TRUE)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Displays pairs of scatterplots of individual assets
    
    # Arguments:
    #   x - a timeSeries object or any other rectangular object
    #       which can be transformed by the function as. matrix
    #       into a numeric matrix.
    #   labels - a logical flag. Should default labels be printed? 
    
    # FUNCTION:
    
    # Settings:
    x = as.matrix(x)
    
    # Plot:
    pairs(x, col = "steelblue", pch = 19, cex = 0.7)
    
    # Control:
    control = list(fun = assetsPairPlot, x = x, labels = labels)
        
    # Return Value:
    invisible(control)
}


# ------------------------------------------------------------------------------


.assetsCorTestPlot = 
    function(x, scale = 1, labels = TRUE, ...)
    {   # A function implemented by Diethelm Wuertz
    
        # Description:
        #   Displays and tests pairwise correlations of assets
        
        # Arguments:
        #   x - a timeSeries object or any other rectangular object
        #       which can be transformed by the function as. matrix
        #       into a numeric matrix.
        #   method - the method to be used. Minimum volume ellipsoid
        #       "mve", minimum covariance determinant "mcd", or 
        #       classical product-moment. 
        #   labels - a logical flag. Should default labels be printed?
        
        # FUNCTION:
        
        # Settings:
        x = as.matrix(x)
     
        # Upper Plot Function:
        cortestPanel =
        function(x, y, cex, ...)
        {
            usr = par("usr"); on.exit(par(usr))
            par(usr = c(0, 1, 0, 1))
            r = abs(cor(x, y))
            txt = format(c(r, 0.123456789), digits = 3)[1]
            test = cor.test(x, y)
            Signif = symnum(test$p.value, corr = FALSE, na = FALSE,
                cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                symbols = c("*** ", "** ", "* ", ". ", "  "))
            text(0.5, 0.5, txt, cex = cex)
            text(0.8, 0.8, Signif, cex = cex, col = "steelblue")
        }
        
        # Lower Plot Function:
        lowessPanel = 
        function (x, y, ...) 
        {
            points(x, y, pch = 19, col = "steelblue", ...)
            ok <- is.finite(x) & is.finite(y)
            if (any(ok)) lines(lowess(x[ok], y[ok]), col = "brown")
        }
    
        # Plot:
        pairs(x, lower.panel = lowessPanel, upper.panel = cortestPanel , 
            cex.labels = 2*scale, cex = 2*scale, cex.axis = 1.5*scale,
            ...)
            
        # Control:
        control = list(fun = assetsCorTestPlot, x = x, scale = scale)
            
        # Return Value:
        invisible(control)
    }


################################################################################


setClass("fASSETS", 
    representation(
        call = "call",              # call: The matched function call
        method = "character",       # method: One of "mn", "msn", "mst"
        model = "list",             # model: A list(mu, Omega, alpha, df)    
        data = "data.frame",        # Data: The data records 
        fit = "list",               # fit: Results parameter estimation     
        title = "character",        # title: A short title string      
        description = "character")  # description: A brief description
)


# ------------------------------------------------------------------------------
                

assetsFit =
function(x, method = c("st", "snorm", "norm"), title = NULL, 
description = NULL, fixed.df = NA, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Fits the parameters of a multivariate data set of assets
    #   and returns a list with the values for the mean, the covariance,
    #   the skewness, and the fatness of tails.
    
    # Arguments:
    #   x - A multivariate time series, a data frame, or any other
    #       rectangular object of assets which can be converted into
    #       a matrix by the function as.matrix. Optional Dates are 
    #       rownames, instrument names are column names.
    #   type - Which type of distribution should be fitted?
    #       a) norm - multivariate Normal
    #       b) snorm - multivariate skew-Normal
    #       c) st - multivariate skew-Student-t        
    
    # Value:
    #   The function returns a list with the following entries:
    #   mu - Mean values of each asset time series
    #   Omega - Covariance matrix of assets
    #   alpha - Skewness vector
    #   df - Degrees of freedom, measures kurtosis

    # Notes: 
    #   Requires function "msn.mle" ans "mst.mle" from R's GPL licensed 
    #     contributed package "sn", (C) 1998-2004 A. Azzalini.
    #   The list returned by this function can serve as input for the 
    #     function assetsSim().
    
    # FUNCTION:
    
    # Settings:
    assets = as.matrix(x)
    method = method[1]
    colNames = colnames(x)
    
    # Normal Distribution:
    if (method == "norm") {
        # Fit Normal:
        fit = list()
        mu = apply(assets, 2, mean)
        Omega = cov(assets)
        alpha = rep(0, times = length(mu))
        df = Inf 
    }
        
    # Skew-Normal Distribution:
    if (method == "snorm") {
        # Fit skew-Normal:
        fit = mvFit(assets, method = "snorm", ...)
        mu = as.vector(fit@fit$dp$beta)
        Omega = fit@fit$dp$Omega
        alpha = as.vector(fit@fit$dp$alpha)
        df = Inf 
        fit = fit@fit
    }
        
    # Skew-Student-t Distribution:
    if (method == "st") {
        # Fit skew-Student:
        fit = mvFit(assets, method = "st", fixed.df = fixed.df, ...)
        mu = as.vector(fit@fit$beta)
        Omega = fit@fit$dp$Omega
        alpha = as.vector(fit@fit$dp$alpha)
        df = fit@fit$dp$df 
        fit = fit@fit
    }
        
    # Add names:   
    names(mu) = colNames
    names(alpha) = colNames
    rownames(Omega) = colNames
    colnames(Omega) = colNames
    
    # Title:
    if (is.null(title)) 
        title = paste("Fitted Asset Data Model: ", method)
        
    # Description:
    if (is.null(description)) 
        description = as.character(date())
        
    # Return Value: 
    new("fASSETS", 
        call = as.call(match.call()), 
        method = as.character(method), 
        model = list(mu = mu, Omega = Omega, alpha = alpha, df = df),
        data = as.data.frame(x), 
        fit = as.list(fit),
        title = as.character(title), 
        description = as.character(description)
        )       
}


# ------------------------------------------------------------------------------


print.fASSETS =
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    # Descriptions:
    #   Print Method for an object of class fASSETS
    
    # Arguments:
    #   x - an object of class fASSETS
      
    # FUNCTION:
    
    # Print:
    object = x
    
    # Title:
    cat("\nTitle:\n")
    cat(as.character(object@title), "\n")
    
    # Call:
    cat("\nCall:\n")
    cat(paste(deparse(object@call), sep = "\n", collapse = "\n"), 
        "\n", sep = "")
          
    # Model Parameters:
    cat("\nModel Parameters:\n")
    print(x@model)
    
    # Description:
    cat("Description:\n")
    print(x@description)
    cat("\n")
    
    # Return Value:
    invisible(object)
}


# ------------------------------------------------------------------------------


plot.fASSETS =  
function(x, which = "ask", ...) 
{   # A function implemented by Diethelm Wuertz

    # Descriptions:
    #   Plots a fit from an assets data set or a model
    
    # Arguments:
    #   x - an object of class fASSETS
    #   ... - arguments to be passed
    
    # Notes:
    #   Library 'sn', is version  0.32-2 (2004-03-13), (C) 1998-2004 
    #     A. Azzalini
    
    # FUNCTION:
    
    # Transform to a S4 object of class "fASSETS":
    object = new("fASSETS", call = x@call, method = x@method, 
        model = x@model, data = x@data, fit = x@fit, title = x@title, 
        description = x@description)         
    # Use plot method for objects of class "fmV"
    plot(object, which = which, xlab = "Time", ylab = "Value", ...)
    
    # Return value:
    invisible(x)
}


# ------------------------------------------------------------------------------


summary.fASSETS =
function(object, which = "all", ...)
{   # A function implemented by Diethelm Wuertz

    # Descriptions:
    #   Summarizes a fit from an assets data set or a model
    
    # Print:
    print(object, ...)
    
    # Plot:
    plot(object, which = which, ...)
    
    # Return value:
    invisible(object)
}


# ******************************************************************************

    
assetsStats =
function(x)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Compute benchmark statistics for a data set of assets with
    #   monthly data records. 
    
    # Details:
    #   The computed statistics values are:
    #       records - number of records (length of time series)
    #       paMean - annualized (pa, per annum) Mean of Returns
    #       paAve - annualized Average of Returns
    #       paVola - annualized Volatility (standard Deviation)
    #       paSkew - Skewness of Returns
    #       paKurt - Kurtosis of Returns
    #       maxDD - maximum Drawdown
    #       TUW - Time under Water
    #       mMaxLoss - Monthly maximum Loss
    #       mVaR - Monthly 99% Value-at-Risk 
    #       mModVaR - Monthly 99% Modified Value-at-Risk 
    #       mSharpe - Monthly Sharpe Ratio
    #       mModSharpe - Monthly Modified Sharpe Ratio
    #       skPrice - Skewness/Kurtosis Price
    #   The statistics are implemented based on the formulas from
    #   "Extreme Metrics". They reflect risk measures as used in 
    #   the hedge fund software from "www.AlternativeSoft.com".

    # Arguments:
    #   x - asset data set, a matrix (or vector) where the rows
    #       are numbered by "time", and the columns belong to the
    #       individual assets. Monthly values are expected.
    
    # Value:
    #   The function returns a data frame with the values of the
    #   12 statistics for each asset.
    
    # Reference:
    #   "ExtremeMetrics Software", Help Document, Alternative Software,
    #   March 2003, 4 pages.
    
    # FUNCTION:
    
    # If x is a vector, make it a matrix:
    statistics = 14
    if (is.null(dim(x))) {
        n = 1 
        x = matrix(x, length(x)) 
        result = matrix(rep(0, times = statistics), ncol = 1) }
    else {
        n = dim(x)[2] 
        result = matrix(rep(0, times = statistics*n), ncol = n) }
    
    # Give Names to Result Matrix:  
    stat.names = c(
        "Records",      "paMean",   "paAve",    "paVola",
        "paSkew",       "paKurt",   "maxDD",    "TUW",
        "mMaxLoss",     "mVaR",     "mModVaR",  "mSharpe",
        "mModSharpe",   "skPrice")
    dimnames(result) = list(stat.names, dimnames(x)[[2]])   

    # Loop over all Assets:
    for (i in 1:n) {
        r = x[, i]
        # Number of Records:
        result[1, i] = length(r)
        # Annualized mean from monthly returns:
        result[2, i] = annualizedMean = (1 + mean(r))^12 - 1
        # Annualized mean from monthly returns:
        result[3, i] = annualizedAverage = mean(r)*sqrt(12)
        # Annualized volatility from monthly returns:
        result[4, i] = annualizedVolatility = sqrt(var(r))
        # Annualized skewness from monthly returns:
        result[5, i] = annualizedSkewness = skewness(r) 
        # Annualized Kurtosis from monthly returns:
        result[6, i] = annualizedKurtosis = kurtosis(r) 
        # Maximum Drawdown of of monthly returns:
        result[7, i] = maxDrawdown = max(cummax(cumsum(r)) - cumsum(r))
        # Time-Under-Water of monthly returns:
        result[8, i] = timeUnderWater = 
            max(diff(which (diff(cummax(cumsum(r))) != 0)))
        # Maximum Loss of monthly returns:
        result[9, i] = maxMonthlyLoss = min(r)  
        # Monthly Value at Risk:
        zc = 2.33
        result[10, i] = monthlyVaR = annualizedMean - 
            zc * annualizedVolatility   
        # Monthly Modified Value at Risk:
        p = 0.99; s = annualizedSkewness; k = annualizedKurtosis    
        zcf = zc + (zc*zc-1)*s/6 + zc*(zc*zc-3)*k/24 + zc*(2*zc*zc-5)*s*s/36
        result[11, i] = monthlyModVaR = annualizedMean - 
            zcf * annualizedVolatility  
        # Monthly Sharpe Ratio:
        result[12, i] = monthlySharpeRatio = 
            annualizedMean/annualizedVolatility 
        # Monthly Modified Sharpe Ratio:
        result[13, i] = monthlyModSharpeRatio = annualizedMean/monthlyModVaR    
        # Skewness Kurtosis Price:
        result[14, i] = skewnesskurtosisPrice = annualizedMean * 
            ( monthlyModVaR/monthlyVaR - 1) }
    
    # Result:
    ans = as.data.frame(round(result, digits = 3))    
    
    # Return Value:
    ans
} 


################################################################################
# Mean and Covariance


assetsMeanCov = 
function(x, method = c("cov", "mve", "mcd", "nnve", "shrink", "bagged"), 
check = TRUE, force = TRUE, baggedR = 100, ...)
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Compute mean and variance from multivariate time series
    
    # Arguments:
    #   x - a multivariate time series, a data frame, or any other
    #       rectangular object of assets which can be converted into
    #       a matrix by the function 'as.matrix'. Optional Dates are 
    #       rownames, instrument names are column names.
    #   method - Which method should be used to compute the covarinace?
    #       cov - standard covariance computation
    #       mve -
    #       mcd -
    #       nnve - 
    #       shrink - estimation with shrinkage method
    #       bagged - estimation with bagging
    
    # Note:
    #   The output of this function can be used for portfolio
    #   optimization.
    
    # FUNCTION:
    
    # Transform Input:
    x.mat = as.matrix(x)
    method = match.arg(method)
    N = dim(x)[1]
       
    # Attribute Control List:
    control = c(method = method[1])
    
    # Compute Covariance:
    method = match.arg(method)
    if (method == "cov") {
        # Classical Covariance Estimation:
        mu = colMeans(x.mat)
        Sigma = cov(x.mat)
        return(list(mu = mu, Sigma = Sigma))
    } else if (method == "mve") {
        # require(MASS)
        return(.assetsRobustCov(x, method = "mve"))
    } else if (method == "mcd") {
        # require(MASS)
        return(.assetsRobustCov(x, method = "mcd"))    
    } else if (method == "shrink") {
        fit = cov.shrink(x = x.mat, ...)
        mu = colMeans(x.mat)
        Sigma = fit 
    } else if (method == "bagged") {
        fit = cov.bagged(x = x.mat, R = baggedR, ...)
        mu = colMeans(x.mat)
        Sigma = fit 
        control = c(control, R = as.character(baggedR))
    } else if (method == "nnve") {
        # Nearest Neighbour Variance Estimation:
        fit = .cov.nnve(datamat = x.mat, ...)
        mu = colMeans(x.mat)
        Sigma = fit$cov
        return(list(mu = mu, Sigma = Sigma))
    }
       
    # Add Size to Control List:
    control = c(control, size = as.character(N))
    
    # Add Names for Covariance Matrix to Control List:
    names(mu) = colnames(x)
    colnames(Sigma) = rownames(Sigma) = colNames = colnames(x)
    
    # Check Positive Definiteness:
    if (check) {
        result = isPositiveDefinite(Sigma)
        if(result) {
            control = c(control, posdef = "TRUE")
        } else {
            control = c(control, posdef = "FALSE")
        }
    }
    
    # Check Positive Definiteness:
    control = c(control, forced = "FALSE")
    if (force) {
        control = c(control, forced = "TRUE")
        if (!result) Sigma = makePositiveDefinite(m = Sigma)       
    }
    
    # Result:
    ans = list(mu = mu, Sigma = Sigma)
    attr(ans, "control") = control
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.assetsRobustCov =
function(x, method = c("mve", "mcd")) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Resistant Estimation of Multivariate Location and Scatter
    
    # Arguments:
    #   x - a timeSeries object or any other rectangular object
    #       which can be transformed by the function as. matrix
    #       into a numeric matrix.
    #   method - the method to be used. Minimum volume ellipsoid
    #       "mve", minimum covariance determinant "mcd", or 
    #       classical product-moment. 
    
    # FUNCTION:
    
    # Settings:
    x = as.matrix(x)
    method = match.arg(method)
    covRob = cov.rob(x, method = method)
    
    # Result:
    ans = list(mu = covRob$center, Omega = covRob$cov)
    attr(ans, "control") = c(control = method)
    
    # Return Value:
    ans

}


# ******************************************************************************


.cov.nnve =
function(datamat, k = 12, pnoise = 0.05, emconv = 0.001, bound = 1.5, 
extension = TRUE, devsm = 0.01)
{   # A (modified) copy from coontributed R package covRobust

    # Description:
    #   Function to perform Nearest Neighbor Variance Estimation
    
    # Arguments:
    #   cov - robust covariance estimate
    #   mu - mean
    #   postprob - posterior probability
    #   classification - classification (0 = noise,  
    #       otherwise 1) (obtained by rounding postprob)
    #   innc - list of initial nearest-neighbor results (components 
    #       are the same as above)     
    
    # Details:
    #   Package: covRobust
    #   Title: Robust Covariance Estimation via Nearest Neighbor Cleaning
    #   Version: 1.0
    #   Author: Naisyin Wang <nwang@stat.tamu.edu> and
    #       Adrian Raftery <raftery@stat.washington.edu>
    #       with contributions from Chris Fraley <fraley@stat.washington.edu>
    #   Description: The cov.nnve() function for robust covariance estimation
    #       by the nearest neighbor variance estimation (NNVE) method
    #       of Wang and Raftery (2002,JASA)
    #   Maintainer: Naisyin Wang <nwang@stat.tamu.edu>
    #   License: GPL version 2 or newer
    #   Notes:
    #       Wang and Raftery(2002), "Nearest neighbor variance estimation (NNVE):
    #           Robust covariance estimation via nearest neighbor cleaning
    #           (with discussion)", 
    #           Journal of the American Statistical Association 97:994-1019
    #       Available as Technical Report 368 (2000) from
    #           http://www.stat.washington.edu/www/research/report
    
    # FUNCTION:
    
    # Settings:
    datamat = as.matrix(datamat)
    d = dim(datamat)[2]
    n = dim(datamat)[1]
    pd = dim(datamat)[2]
    S.mean = apply(datamat, 2, median)
    S.sd = apply(datamat, 2, mad)

    #  NNC based on original data
    orgNNC = .cov.nne.nclean.sub(datamat, k, convergence = 0.001, 
        S.mean = S.mean, S.sd = S.sd)
    nnoise = min(c(sum(1 - orgNNC$z), round(pnoise * n)))
    knnd = orgNNC$kthNND
    ord = (n + 1) - rank(knnd)
    muT = orgNNC$mu1
    SigT = orgNNC$Sig1
    SigT = (SigT + t(SigT))/2.
    SigTN = diag(orgNNC$sd1^2)
    if (nnoise > 6) {
        ncho = nnoise
        ncho1 = floor(ncho/2)
        ncho2 = ncho - ncho1
        cho = (1:n)[ord <= ncho1]
        xcho = datamat[cho, ]
        ev = eigen(SigT)
        evv = ev$values
        minv = max((1:d)[evv > 9.9999999999999998e-13])
        if (minv > 2) {
            vv1 = ev$vectors[, (minv - 1)]
            vv2 = ev$vectors[, minv]
        } else {
            vv1 = ev$vectors[, 1]
            vv2 = ev$vectors[, 2]
        }
        ot = acos(sum(vv1 * vv2)/(sum(vv1^2) * sum(vv2^2))^0.5)
        for (kk1 in 1:(ncho2)) {
            pseg = 1/(ncho2 + 1) * kk1 * ot
            xcho = rbind(xcho, (sin(pseg) * vv1 + cos(pseg) * vv2 + muT))
        }
    } else {
        nnoise = 3
        cho = (1:n)[ord <= nnoise]
        xcho = datamat[cho, ]
    }
    
    n2 = (dim(xcho))[1]
    schox = mahalanobis(xcho, muT, SigTN)
    Nc = matrix(rep(muT, n2), nrow = n2, byrow = TRUE)
    Ndir = (xcho - Nc)/(schox^0.5)

    # initial set up
    ch1 = c(qchisq(0.99, pd), qchisq(1 - 10^(-4), pd))
    Xa = seq(ch1[1], ch1[2], length = 6)
    gap = Xa[2] - Xa[1]
    initv = diag(orgNNC$Sig1)
    xa = Xa[1]
    SaveM = c(xa, orgNNC$mu1, .cov.nne.Mtovec(orgNNC$Sig1))
    OldP = orgNNC$probs
    SaveP = OldP
    Np = Nc - Ndir * (xa^0.5)
    updNNC = .cov.nne.nclean.sub(rbind(datamat, Np), k, convergence = 0.001, 
        S.mean = S.mean, S.sd = S.sd)
    SaveM = rbind(SaveM, c(xa, updNNC$mu1, .cov.nne.Mtovec(updNNC$Sig1)))
    SaveP = rbind(SaveP, (updNNC$probs)[1:n])
    
    # sda <- .cov.nne.Mtovec(orgNNC$Sig1)  
    # sda save the results corresponding to xa = qchisq(.99, pd)
    stopv = diag(updNNC$Sig1)
    time1 = 2

    while ((time1 <= 6) && (all(stopv < (1 + bound) * initv))) {
        xa = Xa[time1]
        Np = Nc - Ndir * (xa^0.5)
        updNNC = .cov.nne.nclean.sub(rbind(datamat, Np), k, convergence = 0.001, 
            S.mean =  S.mean, S.sd = S.sd)
        SaveM = rbind(SaveM, c(xa, updNNC$mu1, .cov.nne.Mtovec(updNNC$Sig1)))
        SaveP = rbind(SaveP[2, ], (updNNC$probs)[1:n])
        time1 = time1 + 1
        stopv = diag(updNNC$Sig1)
        NULL
    }

    # Procedure stop if the added noise cause a "surge" within 
    # the range sdb save the results within the given "range"
    if (all(stopv < (1 + bound) * initv)) {
        dSaveM = dim(SaveM)[1]
        ans = SaveM[dSaveM, ]
        sdb = SaveM[dSaveM, ]
        NewP = SaveP[2, ]

        #  adding extension
        if (extension) {
            time2 = 1
            Fstop = FALSE
            tpv = stopv
            while ((time2 < 2) && (all(stopv < (1 + bound) * initv))
                ) {
                xa = xa + gap
                startv = stopv
                Np = Nc - Ndir * (xa^0.5)
                updNNC = .cov.nne.nclean.sub(rbind(datamat, Np), k, convergence = 
                    0.001, S.mean = S.mean, S.sd = S.sd)
                SaveM = rbind(SaveM, c(xa, updNNC$mu1, .cov.nne.Mtovec(
                    updNNC$Sig1)))
                SaveP = rbind(SaveP[2, ], (updNNC$probs)[
                    1:n])
                stopv = apply(rbind((startv * 2 - tpv), diag(
                    updNNC$Sig1)), 2, mean)
                tpv = diag(updNNC$Sig1)
                Fstop = all((abs(stopv - startv) <= ((1+abs(startv)) *
                    devsm)))
                if (Fstop)
                    time2 = time2 + 1
                else time2 = 1
                NULL
            }
            
            # Checking the stop criterior at the end of extension
            if (all(stopv < (1 + bound) * initv)) {
                dSaveM = dim(SaveM)[1]
                ans = SaveM[dSaveM, ]
                NewP = SaveP[2, ]
            } else {
                dSaveM = dim(SaveM)[1]
                ans = SaveM[dSaveM - 1, ]
                NewP = SaveP[1, ]
            }
        }
    } else {
        dSaveM = dim(SaveM)[1]
        ans = SaveM[dSaveM - 1, ]
        sdb = ans[-1]
        NewP = SaveP[1, ]
    }
    nncvar = .cov.nne.vectoM(ans[ - (1:(1 + pd))], pd)
    mu = ans[2:(1 + pd)]
    
    # Return Value:
    list(cov = nncvar, mu = mu, postprob = NewP, classification = round(NewP), 
        innc = list(cov = orgNNC$Sig1, mu = orgNNC$mu1, postprob = OldP, 
        classification = round(OldP)))
}


# ------------------------------------------------------------------------------


 
.cov.nne.nclean.sub = 
function(datamat, k, distances = NULL, convergence = 0.001, S.mean = NULL, 
S.sd = NULL) 
{   # A (modified) copy from coontributed R package covRobust

    # Description:
    #   Internal Function called by .cov.nne()
    
    # FUNCTION:
    
    #  The Re-scale NNC function:
    d = dim(datamat)[2]
    n = dim(datamat)[1]
    kthNND = .cov.nne.splusNN(t((t(datamat) - S.mean)/S.sd), k = k)
    alpha.d = (2 * pi^(d/2))/(d * gamma(d/2))
    
    # Now use kthNND in E-M algorithm, first get starting guesses.
    delta = rep(0, n)
    delta[kthNND > (min(kthNND) + diff(range(kthNND))/3)] = 1
    p = 0.5
    lambda1 = k/(alpha.d * mean((kthNND[delta == 0])^d))
    lambda2 = k/(alpha.d * mean((kthNND[delta == 1])^d))
    loglik.old = 0
    loglik.new = 1
    
    # Iterator starts here ...
    while (abs(loglik.new - loglik.old)/(1+abs(loglik.new)) > convergence) 
    {
        # E - step
        delta = (p * .cov.nne.dDk(kthNND, lambda1, k = k, d = d, 
            alpha.d = alpha.d)) / (p * .cov.nne.dDk(kthNND, lambda1,
            k = k, d = d, alpha.d = alpha.d) + (1 - p) *
            .cov.nne.dDk(kthNND, lambda2, k = k, d = d, alpha.d = alpha.d))
        # M - step
        p = sum(delta) / n
        lambda1 = (k * sum(delta))/(alpha.d * sum((kthNND^d) * delta))
        lambda2 = (k * sum((1 - delta)))/(alpha.d * 
            sum((kthNND^d) * (1 - delta)))
        loglik.old = loglik.new
        loglik.new = sum( - p * lambda1 * alpha.d * ((kthNND^d) * delta) - 
            (1 - p) * lambda2 * alpha.d * ((kthNND^d) * (1 - delta)) + 
            delta * k * log(lambda1 * alpha.d) + (1 - delta) * k * 
            log(lambda2 * alpha.d))
    }

    # z will be the classifications. 1 = in cluster. 0 = in noise.
    probs = .cov.nne.dDk(kthNND, lambda1, k = k, d = d, alpha.d = alpha.d) /
        (.cov.nne.dDk(kthNND, lambda1, k = k, d = d, alpha.d = alpha.d) +
        .cov.nne.dDk(kthNND, lambda2, k = k, d = d, alpha.d = alpha.d))
    mprob = 1. - probs
    mu1 = apply((probs * datamat), 2, sum)/sum(probs)
    mu2 = apply((mprob * datamat), 2, sum)/sum(mprob)
    tpsig1 = t(datamat) - mu1
    tpsig2 = t(datamat) - mu2
    Sig1 = tpsig1 %*% (probs * t(tpsig1))/sum(probs)
    Sig2 = tpsig2 %*% (mprob * t(tpsig2))/sum(mprob)
    sd1 = sqrt(diag(Sig1))
    sd2 = sqrt(diag(Sig2))
    ans = rbind(mu1, sd1, mu2, sd2)
    
    zz = list(z = round(probs), kthNND = kthNND, probs = probs,
        p = p, mu1 = mu1, mu2 = mu2, sd1 = sd1, sd2 = sd2,
        lambda1 = lambda1, lambda2 = lambda2, Sig1 = Sig1,
        Sig2 = Sig2, ans = ans)
     
    # Return Value:   
    return(zz)
}


# ------------------------------------------------------------------------------


.cov.nne.dDk = 
function(x, lambda, k, d, alpha.d) 
{   # A (modified) copy from coontributed R package covRobust

    # Description:
    #   Internal Function called by .cov.nne()
    
    # FUNCTION:
    
    # Function to perform the Nearest Neighbour cleaning of
    # find the density of D_k
    ans = (exp( - lambda * alpha.d * x^d + log(2) + k * log(
        lambda * alpha.d) + log(x) * (d * k - 1) - log(
        gamma(k))))
     
    # Return Value:    
    ans
}


# ------------------------------------------------------------------------------


.cov.nne.splusNN = 
function(datamat, k)
{   # A (modified) copy from coontributed R package covRobust

    # Description:
    #   Internal Function called by .cov.nne()
    
    # FUNCTION:
    
    # Nearest-neighbor in S-PLUS
    n = nrow(datamat)
    distances = dist(datamat)
    
    #  This next part sorts through the Splus distance object 
    #  and forms kNNd, kth nearest neighbour distance, for each 
    #  point.
    kNNd = rep(0, n)
    N = (n - 1):0
    I = c(0, cumsum(N[-1]))
    J = c(0, I + n - 1)
    a = z = NULL
    for (j in 1:n) {
        if (j > 1)
            a = i + I[1:i]
        if (j < n)
            z = J[j] + 1:N[j]
        kNNd[j] = sort(distances[c(a, z)])[k]
        i = j
    }
    
    # Return Value: 
    kNNd
}


# ------------------------------------------------------------------------------



.cov.nne.Mtovec = 
function(M) 
{   # A (modified) copy from coontributed R package covRobust

    # Description:
    #   Internal Function called by .cov.nne()
    
    # FUNCTION:
    
    # Two procedures to link between a symmetric matrix and its vec(.)
    n = dim(M)[1]
    d = dim(M)[2]
    if (abs(n - d) > 0.01) {
        cat ("The input has to be a square matrix")
    } else {
        vec = rep(0, 0)
        for (i in (1:n)) {
            for (j in (i:d)) {
                vec = c(vec, M[i, j])
            }
        }
        vec
    }
}


# ------------------------------------------------------------------------------


.cov.nne.vectoM = 
function(vec, d) 
{   # A (modified) copy from coontributed R package covRobust

    # Description:
    #   Internal Function called by .cov.nne()
    
    # FUNCTION:
    
    n = length(vec)
    M = matrix(rep(0, d * d), d, d)
    L = 1
    for (i in 1:d) {
        for (j in i:d) {
            M[i, j] = vec[L]
            L = L + 1
            M[j, i] = M[i, j]
        }
    }
    
    # Return Value: 
    M
}
        

################################################################################
# S4 Normality Test:


assetsTest =
function(x, method = c("shapiro", "energy"), Replicates = 100, 
title = NULL, description = NULL)
{
    # Example:
    #   .mvnormTest(x = assetsSim(100))
    #   .mvnormTest(x = assetsSim(100), method = "e", Replicates = 99)
    
    # FUNCTION:
    
    # Test:
    method = method[1]
    if (method == "shapiro" | method == "s") {
        test = .mvshapiroTest(x)
    } else if (method == "energy" | method == "e") {
        test = .mvenergyTest(x, Replicates = Replicates)
    } else {
        stop("No valid method specified")
    }
    
    # Return Value:
    test    
}


################################################################################


.mvenergyTest =
function(x, Replicates = 99, title = NULL, description = NULL)
{
    # Example:
    #   .mvenergyTest(x = assetsSim(100), 99)
    
    # FUNCTION:
    
    # Transform:
    if (class(x) == "timeSeries") x = seriesData(x)
    x = as.matrix(x)
    
    # Test:
    test = .mvnorm.etest(x = x, R = Replicates)
    names(test$p.value) = ""
    class(test) = "list"
    
    # Add:
    if (is.null(title)) title = test$method
    if (is.null(description)) description = date()
    
    # Return Value:
    new("fHTEST",
        call = match.call(),
        data = list(x = x),
        test = test,
        title = title,
        description = description)
}


################################################################################
# Internal Utility Functions - mvnorm Test:


# Package: energy
# Title: E-statistics (energy statistics) tests of fit, independence, 
#   clustering
# Version: 1.0-3
# Date: 2005-10-02
# Author: Maria L. Rizzo <rizzo@math.ohiou.edu> and 
#   Gabor J. Szekely <gabors@bgnet.bgsu.edu>
# Description: E-statistics (energy) tests for comparing distributions: 
#   multivariate normality, multivariate k-sample test for equal 
#   distributions, hierarchical clustering by e-distances, multivariate 
#   independence test, Poisson test. Energy-statistics concept based on 
#   a generalization of Newton's potential energy is due to Gabor J. Szekely.
# Maintainer: Maria Rizzo <rizzo@math.ohiou.edu>
# License: GPL 2.0 or later
# Packaged: Sun Oct  2 16:55:19 2005; rizzo


.mvnorm.etest = 
function(x, R) 
{
    # Description:
    #   Parametric bootstrap E-test for multivariate normality
    
    # Author: 
    #   Maria L. Rizzo <rizzo@math.ohiou.edu> and 
    #   Gabor J. Szekely <gabors@bgnet.bgsu.edu>

    # FUNCTION:
    
    # Test:
    if (is.vector(x)) {
        n = length(x)
        d = 1
        bootobj = .mvnormBoot(x, statistic = .normal.e, R = R,  
            ran.gen = function(x, y) {return(rnorm(n)) })
    } else {
        n = nrow(x)
        d = ncol(x)
        bootobj = .mvnormBoot(x, statistic = .mvnorm.e, R = R,  
            ran.gen = function(x, y) { return(matrix(rnorm(n * d), 
            nrow = n, ncol = d)) })
    }
    p = 1 - mean(bootobj$t < bootobj$t0)
    names(bootobj$t0) = "E-statistic"
    e = list(statistic = bootobj$t0,
        p.value = p,
        method = "Energy Test of Multivariate Normality",
        data.name = paste("x, sample size ", n, ", dimension ", 
            d, ", replicates ", R, sep = ""))
    class(e) = "htest"    
    
    # Return Value:    
    e                 
}


# ------------------------------------------------------------------------------


.mvnorm.e = 
function(x) 
{
    # Description:
    #   E-statistic for multivariate normality
    
    # Author: 
    #   Maria L. Rizzo <rizzo@math.ohiou.edu> and 
    #   Gabor J. Szekely <gabors@bgnet.bgsu.edu>
    
    # FUNCTION:
    
    # Statistic:
    if (is.vector(x)) return(normal.e(x))
    n = nrow(x)
    d = ncol(x)
    if (n < 2) return(normal.e(x))
    # subtract column means and
    z = scale(x, scale = FALSE) 
    # compute S^(-1/2)    
    ev = eigen(var(x), symmetric = TRUE)    
    P = ev$vectors
    lambda = ev$values    
    y = z %*% (P %*% diag(1 / sqrt(lambda)) %*% t(P))
    if (any(!is.finite(y))) return (NA)
    stat = 0
    e = .C("mvnEstat", y = as.double(t(y)), byrow = as.integer(TRUE),
        nobs = as.integer(n), dim = as.integer(d), 
        stat = as.double(stat), PACKAGE = "fPortfolio")$stat
            
    # Return Value:
    e
}


# ------------------------------------------------------------------------------


.normal.e = 
function(x) 
{
    # Description:
    #   Statistic for univariate Normality
    
    # Authors: 
    #   Maria L. Rizzo <rizzo@math.ohiou.edu> and 
    #   Gabor J. Szekely <gabors@bgnet.bgsu.edu>
    
    # FUNCTION:
    
    # Statistic:
    x = as.vector(x)
    y = sort(x)
    n = length(y)
    if (y[1] == y[n]) return (NA)
    y = scale(y) 
    K = seq(1 - n, n - 1, 2)
    e = 2 * (sum(2 * y * pnorm(y) + 2 * dnorm(y)) - n / sqrt(pi) - mean(K * y))
   
    # Return Value:
    e
}


################################################################################
# Internal Utility Functions - Bootstrap:


# Package: boot
# Priority: recommended
# Version: 1.2-24
# Date: 2005-12-09
# Author: S original <http://statwww.epfl.ch/davison/BMA/library.html>
#   by Angelo Canty <cantya@mcmaster.ca>.  
#   R port by  Brian Ripley <ripley@stats.ox.ac.uk>.
# Maintainer: Brian Ripley <ripley@stats.ox.ac.uk>
# Description: functions and datasets for bootstrapping from the
#   book "Bootstrap Methods and Their Applications" by A. C. Davison and 
#   D. V. Hinkley (1997, CUP).
# Title: Bootstrap R (S-Plus) Functions (Canty)
# Depends: R (>= 2.0.0), graphics, stats
# Suggests: survival
# LazyData: yes
# License: Unlimited distribution.
# Packaged: Thu Dec  8 21:19:17 2005; ripley


.mvnormBoot = 
function(data, statistic, R, strata = rep(1, n), L = NULL, m = 0, 
weights = NULL, ran.gen=function(d, p) d, mle = NULL, ...)
{    
    # Author: 
    #   S original <http://statwww.epfl.ch/davison/BMA/library.html>
    #   by Angelo Canty <cantya@mcmaster.ca>  
    #   R port by  Brian Ripley <ripley@stats.ox.ac.uk>

    # R replicates of bootstrap applied to  statistic(data)
    # Various auxilliary functions find the indices to be used for the
    # bootstrap replicates and then this function loops over those replicates.
     
    isBootMatrix = function(x) {length(dim(x)) == 2}
    call = match.call()
    if (!exists(".Random.seed", envir=.GlobalEnv, inherits = FALSE)) runif(1)
    seed = get(".Random.seed", envir=.GlobalEnv, inherits = FALSE)
    if (isBootMatrix(data)) n = nrow(data) else n = length(data)
    temp.str = strata
    strata = tapply(1:n,as.numeric(strata))
    if ((n == 0) || is.null(n)) stop("no data in call to boot")  
    t0 = statistic(data,...)
    lt0 = length(t0)
    t.star = matrix(NA, sum(R), lt0)
    pred.i = NULL
    for(r in 1:R) t.star[r,] = statistic(ran.gen(data, mle),...)      
    dimnames(t.star) = NULL
    if (is.null(weights)) weights = 1/tabulate(strata)[strata]
    out = list(t0 = t0, t = t.star, R = R, data = data, seed = seed,
        statistic = statistic, call = call)
     out = c(out, list(ran.gen = ran.gen, mle=mle) )
    class(out) = "boot"
    out
}

  
################################################################################


# Package: mvnormtest
# Version: 0.1-6
# Date: 2005-04-02
# Title: Normality test for multivariate variables
# Author: Slawomir Jarek
# Maintainer: Slawomir Jarek <slawomir.jarek@gallus.edu.pl>
# Description: Generalization of shapiro-wilk test for multivariate variables.
# License: GPL
# Depends: stats


.mvshapiroTest = 
function(x, title = NULL, description = NULL)
{   
    # Description:
    #   Computes Shapiro's normality test for multivariate variables
    
    # Author: 
    #   Slawomir Jarek

    # Example:
    #   .mvshapiroTest(x = assetsSim(100))
    
    # FUNCTION:
    
    # Transform:
    U = t(as.matrix(x))

    # Test:
    n = ncol(U)
    if (n < 3 || n > 5000) stop("sample size must be between 3 and 5000")
    rng = range(U)
    rng = rng[2] - rng[1]
    if (rng == 0)
    stop("all `U[]' are identical")
    Us = apply(U,1,mean)
    R = U-Us
    M.1 = solve(R%*%t(R),tol=1e-18)
    Rmax = diag(t(R)%*%M.1%*%R)
    C = M.1%*%R[,which.max(Rmax)]
    Z = t(C)%*%U
    test = shapiro.test(Z)
    names(test$p.value) = ""
    class(test) = "list"
    
    # Add title and description:
    if (is.null(title)) title = "Multivariate Shapiro Test"
    if (is.null(description)) description = as.character(date())
    
    # Return Value:
    new("fHTEST", 
        call = match.call(), 
        data = list(x = x), 
        test = test, 
        title = title, 
        description = description)

}


################################################################################

