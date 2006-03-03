
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
# FUNCTION:             SIMULATION AND ASSET PICKING:
#  assetsSim             Simulates a set of artificial assets
#  assetsSelect          Clusters a set of assets
#   method = "hclust"    hierarchical clustering
#   method = "kmeans"    k-means clustering
# FUNCTION:             PARAMETER ESTIMATION:
#  fASSETS               Class representation for "fASSETS" Objects
#  assetsFit             Estimates the parameters of set of assets
#   method = "norm"       assuming a multivariate Normal distribution
#   method = "snorm"      assuming a multivariate skew-Normal distribution
#   method = "st"         assuming a multivariate skew-Student-t  
# METHODS:              DESCRIPTION:
#  print.fASSETS         S3: Print method for an object of class fASSETS
#  plot.fASSETS          S3: Plot method for an object of class fASSETS
#  summary.fASSETS       S3: Summary method for an object of class fASSETS
# FUNCTION:             STATISTICS AND TESTS:
#  assetsStats           Computes basic statistics of asset sets  
# FUNCTION:             MEAN AND VARIANCE ESTIMATION:
# .assetsMV              Estimates mean and variance for a set of assets
#   method = "cov"        Standard Covariance Estimation
#   method = "shrink"     using Shrinkage
#   method = bagged"      using bagging
#  .isPD                 Checks if the matrix x is positive definite
#  .makePD               Forces the matrix x to be positive definite
# FUNCTION:             NORMALITY TESTS:
# .assetsTest            Test for multivariate Normal Assets
#   method = "shapiro"    calling Shapiro test
#   method = "energy"     calling Energy test
# .mvenergyTest          Multivariate Energy Test
#  .mvnorm.etest          Internal Function used by .assetsTest
#  .mvnorm.e              Internal Function used by .assetsTest
#  .normal.e              Internal Function used by .assetsTest
#  .mvnormBoot            Internal Function used by .assetsTest
# .mvshapiroTest         Multivariate Shapiro Test
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


################################################################################


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
    
    # Transform to a A4 object of class "fMV":
    object = new("fMV", call = x@call, method = x@method, 
        model = x@model, data = x@data, fit = x@fit, title = x@title, 
        description = x@description)  
        
    # Use plot method for objects of class "fmV"
    plot(object, which = which, ...)
    
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


.assetsMV = 
function(x, method = c("cov", "shrink", "bagged"), check = TRUE,
force = FALSE, R.bagged = 100, lambda.shrink = 0.1, ...)
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
    #       shrink - estimation with shrinkage method
    #       bagged - estimation with bagging
    
    # Note:
    #   The output of this function can be used for portfolio
    #   optimization.
    
    # FUNCTION:
    
    # Transform Input:
    x.mat = as.matrix(x)
    N = dim(x)[1]
       
    # Attribute Control List:
    control = c(method = method[1])
    
    # Compute Mean:
    mu = colMeans(x)
    
    # Compute Covariance:
    method = match.arg(method)
    if (method == "cov") {
        Sigma = cov(x.mat)
    } else if (method == "shrink") {
        fit = cov.shrink(x = x.mat, lambda = lambda.shrink)
        Sigma = fit 
        attr(Sigma, "lambda") = NULL
        control = c(control, lambda = as.character(lambda.shrink))
    } else if (method == "bagged") {
        fit = cov.bagged(x = x.mat, R = R.bagged)
        Sigma = fit 
        control = c(control, R = as.character(R.bagged))
    } else if (method == "nnve") {
        # Nearest Neighbour Variance Estimation:
        fit = cov.nnve(datamat = x.mat, ...)
        Sigma = fit$cov
    }
       
    # Add Size to Control List:
    control = c(control, size = as.character(N))
    
    # Add Names for Covariance Matrix to Control List:
    names(mu) = colnames(x)
    colnames(Sigma) = rownames(Sigma) = colNames = colnames(x)
    
    # Check Positive Definiteness:
    if (check) {
        result = .isPD(Sigma)
        if(result) {
            control = c(control, posdef = "TRUE")
        } else {
            control = c(control, posdef = "FALSE")
        }
    }
    
    # Check Positive Definiteness:
    control = c(control, forced = "FALSE")
    if (!result & force) {
        Sigma = make.positive.definite(m = Sigma)
        control = c(control, forced = "TRUE")
    }
    
    # Result:
    ans = list(mu = mu, Sigma = Sigma)
    attr(ans, "control") = control
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.isPD =
function(x)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Checks if the matrix x is positive definite
    
    # Arguments:
    #   x - a symmetric matrix or any other rectangular object
    #       describing a covariance matrix which can be converted into
    #       a matrix by the function 'as.matrix'. 
    
    # FUNCTION:
    
    # Transform:
    x = as.matrix(x)
    
    # Check if matrix is positive definite:
    ans = is.positive.definite(m = x)\
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.makePD =
function(x)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Forces the matrix x to be positive definite
    
    # Arguments:
    #   x - a symmetric matrix or any other rectangular object
    #       describing a covariance matrix which can be converted into
    #       a matrix by the function 'as.matrix'. 
    
    # FUNCTION:
    
    # Make Positive Definite:
    ans = make.positive.definite(m = x)
    
    # Return Value:
    ans
}
        

################################################################################
# S4 Normality Test:


.assetsTest =
function(x, method = c("shapiro", "energy"), Replicates = 99, 
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


#*******************************************************************************
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
    
    # Author: 
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


#*******************************************************************************
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

