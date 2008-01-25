
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
# FUNCTION:                 MEAN-COVARIANCE ESTIMATION:
#  assetsMeanCov             Estimates mean and variance for a set of assets
#   method = "cov"            uses standard covariance estimation
#   method = "mve"            uses "cov.mve" from [MASS]
#   method = "mcd"            uses "cov.mcd" from [MASS]
#   method = "Mcd"            requires "covMcd" from [robustbase]  
#   method = "OGK"            requires "covOGK" from [robustbase] 
#   method = "nnve"           uses builtin from [covRobust]
#   method = "shrink"         uses builtin from [corpcor]
#   method = "bagged"         uses builtin from [corpcor]
# FUNCTION:                 DESCRIPTION:
#  .assetsCorrelationPlot    Plots classical/robust correlation ellipsoids
#  .assetsOutlierDetection   Detects outliers in a multivariate set of assets
################################################################################


assetsMeanCov <- 
    function(x, 
    method = c("cov", "mve", "mcd", "MCD", "OGK", "nnve", "shrink", "bagged"), 
    check = TRUE, force = TRUE, baggedR = 100, sigmamu = scaleTau2, 
    alpha = 1/2, ...)
{   
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Computes mean and variance from multivariate time series
    
    # Arguments:
    #   x - a multivariate time series, a data frame, or any other
    #       rectangular object of assets which can be converted into
    #       a matrix by the function 'as.matrix'. Optional Dates are 
    #       rownames, instrument names are column names.
    #   method - Which method should be used to compute the covarinace?
    #       method = "cov"        standard covariance computation
    #       method = "shrink"     uses "shrinkage" from [corpcor]
    #       method = "MCD"        uses "MCD" from [robustbase]
    #       method = "OGK"        uses "OGK" from [robustbase]
    #       method = "mve"        uses "mve" from [MASS]
    #       method = "mcd"        uses "mcd" from [MASS]
    #       method = "nnve"       uses "nnve" from [covRobust]
    #       method = "bagged"     uses "bagging" [corpcor]
    #   alpha - MCD: numeric parameter controlling the size of the subsets 
    #       over which the determinant is minimized, i.e., alpha*n observations 
    #       are used for computing the determinant. Allowed values are between 
    #       0.5 and 1 and the default is 0.5.
    #   sigma.mu - OGK: a function that computes univariate robust location 
    #       and scale estimates. By default it should return a single numeric 
    #       value containing the robust scale (standard deviation) estimate. 
    #       When mu.too is true, sigmamu() should return a numeric vector of 
    #       length 2 containing robust location and scale estimates. See 
    #       scaleTau2, s_Qn, s_Sn, s_mad or s_IQR for examples to be used as 
    #       sigmamu argument.
    
    
    # Note:
    #   The output of this function can be used for portfolio
    #   optimization.
    
    # FUNCTION:
    
    # Transform Input:
    x.mat = as.matrix(x)
    # method = match.arg(method)
    method = method[1]
    N = ncol(x)
    assetNames = colnames(x.mat)
       
    # Attribute Control List:
    control = c(method = method[1])
    
    # Compute Classical Covariance:
    if (method == "cov") {
        # Classical Covariance Estimation:
        mu = colMeans(x.mat)
        Sigma = cov(x.mat)
    }
        
    # From R Package "robustbase":
    if (method == "MCD" | method == "Mcd") {
        estimate = robustbase::covMcd(x.mat, alpha = alpha, ...)
        mu = estimate$center
        Sigma = estimate$cov
    }   
    if (method == "OGK" | method == "Ogk") {
        estimate = robustbase::covOGK(x.mat, sigmamu = scaleTau2, ...)
        mu = estimate$center
        Sigma = estimate$cov     
    }
    
    # [MASS] mve and mcd Routines:
    if (method == "mve") {
        # require(MASS)
        ans = MASS::cov.rob(x = x.mat, method = "mve")
        mu = ans$center
        Sigma = ans$cov
    }
    if (method == "mcd") {
        # require(MASS)
        ans = MASS::cov.rob(x = x.mat, method = "mcd") 
        mu = ans$center
        Sigma = ans$cov
    }    
        
    # [corpcor] Shrinkage and Bagging Routines 
    if (method == "shrink") {
        fit = .cov.shrink(x = x.mat, ...)
        mu = colMeans(x.mat)
        Sigma = fit 
    } 
    if (method == "bagged") {
        fit = .cov.bagged(x = x.mat, R = baggedR, ...)
        mu = colMeans(x.mat)
        Sigma = fit 
        control = c(control, R = as.character(baggedR))
    }
        
    # Nearest Neighbour Variance Estimation:
    if (method == "nnve") {
        fit = .cov.nnve(datamat = x.mat, ...)
        mu = colMeans(x.mat)
        Sigma = fit$cov
    }
       
    # Add Size to Control List:
    control = c(control, size = as.character(N))
    
    # Add Names for Covariance Matrix to Control List:
    names(mu) = assetNames
    colnames(Sigma) = rownames(Sigma) = colNames = assetNames
    
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
        if (!result) Sigma = makePositiveDefinite(Sigma)       
    }
    
    # Result:
    ans = list(center = mu, cov = Sigma, mu = mu, Sigma = Sigma)
    attr(ans, "control") = control
    
    # Return Value:
    ans
}


################################################################################


.assetsCorrelationPlot <- 
    function (x, y = NULL, method = c("mcd", "mve", "Mcd", "OGK", "shrink"), 
    labels = TRUE, ...) 
{   
    # An adapted copy from contributed R package mvoutlier

    # Description:
    #   Plots classical/robust bivariate corrleation ellipsoids
    
    # Arguments:
    #   x, y -
    #   method -
    #   ... -
    
    # Details:
    #   The function  plots the (two-dimensional) data and adds two 
    #   correlation ellipsoids, based on classical and robust estimation 
    #   of location and scatter. Robust estimation can be thought of as 
    #   estimating the mean and covariance of the 'good' part of the data.
   
    # Source:
    #   Contributed R package "mvoutlers"
    #   Moritz Gschwandtner <he0125439@student.tuwien.ac.ati>
    #   Peter Filzmoser <hP.Filzmoser@tuwien.ac.ati> 
    
    # References:
    #   P. Filzmoser, R.G. Garrett, and C. Reimann (2005). 
    #   Multivariate Outlier Detection in Exploration Geochemistry. 
    #   Computers & Geosciences.
    
    # FIUNCTION:
    
    # Settings:
    quan = 1/2
    alpha = 0.025
    
    # Match Arguments:
    method = match.arg(method)
    
    # Allow for 'timeSeries' Input:
    if (is.null(y)) {
        x = as.matrix(x)
    } else {    
        x = as.matrix(cbind(as.vector(x), as.vector(y)))
    }
     
    # Robust Estimation:
    if (method == "mcd") {
        cov = MASS::cov.mcd(x, cor = TRUE)
    } else if (method == "mve") {
        cov = MASS::cov.mve(x, cor = TRUE)
    } else if (method == "Mcd") {
        covr = robustbase::covMcd(x, cor = TRUE, alpha = quan)
    } else if (method == "OGK") {
        covr = robustbase::covOGK(x, cor = TRUE, sigmamu = scaleTau2)
    } else if (method == "shrink") {
        covr = assetsMeanCov(x, "shrink")
        covr$cov = covr$Sigma
        covr$cor = cov2cor(covr$Sigma)
        covr$center = covr$mu
    }
    
    # Singular Value Decomposition:
    cov.svd  = svd(cov(x), nv = 0)
    covr.svd = svd(covr$cov, nv = 0)
    r  =  cov.svd[["u"]] %*% diag(sqrt( cov.svd[["d"]]))
    rr = covr.svd[["u"]] %*% diag(sqrt(covr.svd[["d"]]))
    e = cbind(
        cos(c(0:100)/100 * 2 * pi) * sqrt(qchisq(1 - alpha, 2)), 
        sin(c(0:100)/100 * 2 * pi) * sqrt(qchisq(1 - alpha, 2)))
    tt  = t(r  %*% t(e)) + rep(1, 101) %o% apply(x, 2, mean)
    ttr = t(rr %*% t(e)) + rep(1, 101) %o% covr$center
    
    # Plot Correlation:
    plot(x, 
        xlim = c(min(c(x[, 1], tt[, 1], ttr[, 1])), max(c(x[, 1], 
            tt[, 1], ttr[, 1]))), 
        ylim = c(min(c(x[, 2], tt[, 2], ttr[, 2])), max(c(x[, 2], tt[, 2], 
            ttr[, 2]))), pch = 19, ...)
    if (labels) {
        title(main = list( paste(
            "Classical Cor =", round(cor(x)[1, 2], 2), " | ",
            "Robust Cor =", round(covr$cor[1, 2], 2)) ))
    }
    lines( tt[, 1],  tt[, 2], type = "l", col = 4, lty = 3)
    lines(ttr[, 1], ttr[, 2], type = "l", col = 2)   
    grid() 
    
    # Add Legend:
    legend("topleft", legend = c("Classical", "Robust"), 
        text.col = c(2, 4), bty = "n")
    
    # Result:
    ans = list(
        classicalCor = cor(x)[1, 2], 
        robCor = covr$cor[1,  2],
        data = x)
    attr(ans, "control") = c(method = method)
    
    # Return Value:
    invisible(ans)
}
 

# ------------------------------------------------------------------------------


.assetsOutlierDetection <- 
    function (x, method = c("cov", "mcd", "mve", "Mcd", "OGK", "shrink"))
{   
    # An adapted copy from contributed R package mvoutlier

    # Description:
    #   Detects outliers in a multivariate set of assets
    
    # Arguments:
    
    # Source:
    #   The code concerned with the outliers is from R package "mvoutliers"
    #   Moritz Gschwandtner <he0125439@student.tuwien.ac.ati>
    #   Peter Filzmoser <hP.Filzmoser@tuwien.ac.ati> 
    
    # References:
    #   P. Filzmoser, R.G. Garrett, and C. Reimann (2005). 
    #   Multivariate Outlier Detection in Exploration Geochemistry. 
    #   Computers & Geosciences.
       
    # FUNCTION:
    
    # Match Arguments:
    method = match.arg(method)
    
    # Allow for 'timeSeries' Input:
    x = as.matrix(x)
    
    # Critical Values:
    n = nrow(x)
    p = ncol(x)
    if (p <= 10) pcrit = (0.240 - 0.0030 * p)/sqrt(n)
    if (p  > 10) pcrit = (0.252 - 0.0018 * p)/sqrt(n)
    delta = qchisq(0.975, p)
    
    # Compute Robust Covariance Estimates:
    if (method == "cov") {
        # Standard Method:
        center = colMeans(x)
        cov = cov(x)
    } else if (method == "mcd") {
        # MCD from MASS Package
        mean.cov = MASS::cov.mcd(x)
        center = mean.cov$center
        cov = mean.cov$cov
    } else if (method == "mve") {
        # MVE from MASS Package
        mean.cov = MASS::cov.mve(x)
        center = mean.cov$center
        cov = mean.cov$cov
    } else if (method == "Mcd") {
        mean.cov = covMcd(x)
        center = mean.cov$center
        cov = mean.cov$cov
    } else if (method == "OGK") {
        mean.cov = robustbase::covOGK(x, cor = TRUE, sigmamu = scaleTau2)
        center = mean.cov$center
        cov = mean.cov$cov 
    } else if (method == "shrink") {
        # Shrinkage from contributed package "corpcor":
        mean.cov = assetsMeanCov(x, "shrink")
        center = mean.cov$mu
        cov = mean.cov$Sigma
    }
    
    # Compute Mahalanobis Squared Distances:
    d2 = mahalanobis(x, center, cov)
    
    # Detect Outliers:
    d2ord = sort(d2)
    dif = pchisq(d2ord, p) - (0.5:n)/n
    i = (d2ord >= delta) & (dif > 0)
    if (sum(i) == 0) alfan = 0 else alfan = max(dif[i])
    if (alfan < pcrit) alfan = 0
    if (alfan > 0) cn = max(d2ord[n-ceiling(n*alfan)], delta) else cn = Inf
    w = d2 < cn
    m = apply(x[w, ], 2, mean)
    c1 = as.matrix(x - rep(1, n) %*% t(m))
    c = (t(c1) %*% diag(w) %*% c1)/sum(w)
    
    # Identify Outliers:
    outliers = (1:dim(x)[1])[!w]
    if (length(outliers) == 0) outliers = NA
    
    # Compose Result:
    ans = list(center = m, cov = c, cor = cov2cor(c), 
        quantile = cn, outliers = outliers)
    attr(ans, "control") = c(method = method)
    
    # Return Value:
    ans
}


################################################################################

