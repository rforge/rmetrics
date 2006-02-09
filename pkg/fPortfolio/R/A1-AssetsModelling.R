
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
# FUNCTION:             DESCRIPTION:
#  assetsSim             Simulates a set of artificial assets
#  assetsSelect          Clusters a set of assets
#   method = hclust       hierarchical clustering
#   method = kmeans       k-means clustering
#  fASSETS               Class representation for "fASSETS" Objects
#  assetsFit             Estimates the parameters of set of assets
#   method = norm         assuming a multivariate Normal distribution
#   method = snorm        assuming a multivariate skew-Normal distribution
#   method = st           assuming a multivariate skew-Student-t  
#  assetsStats           Computes basic statistics of asset sets 
# METHODS:              DESCRIPTION:
#  print.fASSETS         S3: Print method for an object of class fASSETS
#  plot.fASSETS          S3: Plot method for an object of class fASSETS
#  summary.fASSETS       S3: Summary method for an object of class fASSETS
################################################################################


################################################################################
# BUILTIN:
#  .msn.quantities       Internal function copied from R package sn
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
    #   	mu - the numeric vector of mean values of each asset time series
    #   	Omega - the covariance matrix of assets
    #   	alpha - the skewness vector
    #   	df - the degrees of freedom, a measures for the kurtosis
    #	assetNames - a string vector of asset names, by default NULL
    #		which creates asset names as "V1", "V2", ..., "Vd", where
    #		d denotes the dimension
    
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


# ******************************************************************************


assetsSelect = 
function(x, method = c("hclust", "kmeans"),
kmeans.centers = 5, kmeans.maxiter = 10, doplot = TRUE, ...)
{	# A function implemented by Diethelm Wuertz

	# Description: 
	# 	Clusters a set of assets
	
	# FUNCTION:

	# Transform to matrix:
	if (class(x) == "timeSeries") x = as.matrix(x)
	
	# stats::hclust
	# Hierarchical cluster analysis on a set of dissimilarities 
	# and methods for analyzing it. 
	
	if (method[1] == "hclust") {
		ans = hclust(dist(t(x)))
		if (doplot) plot(ans, ...)	
	}
	
	# stats::kmeans
	# Perform k-means clustering on a data matrix
	
	if (method[1] == "kmeans") {
		ans = kmeans(t(x), kmeans.centers, kmeans.maxiter)
		if (doplot) plot(t(x), col = ans$cluster, ...)
	}
	
	# Return Value:
	ans
}


# ******************************************************************************


setClass("fASSETS", 
    representation(
        call = "call",			    # call: The matched function call
        method = "character",    	# method: One of "mn", "msn", "mst"
        model = "list",      		# model: A list(mu, Omega, alpha, df)    
        data = "data.frame", 		# Data: The data records 
        fit = "list",     			# fit: Results parameter estimation     
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
    #   x - A data frame of assets, dates are rownames,
    #       instrument names are column names.
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
{	# A function implemented by Diethelm Wuertz

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

