
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
# FUNCTION:                 DESCRIPTION:
#  .assetsOutlierDetection   Detects outliers in a multivariate set of assets
################################################################################


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

