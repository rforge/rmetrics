
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
#  .assetsCorrelationPlot    Plots classical/robust correlation ellipsoids
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


################################################################################

