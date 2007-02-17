
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
#  .assetsSeriesPlot     Displays time series of individual assets
#  .assetsHistPlot       Displays histograms of individual assets 

#  .assetsQQNormPlot     Displays normal qq-plots of individual assets
# FUNCTION:
#  .assetsPairsPlot      Displays pairs of scatterplots of individual assets
#  .assetsCorTestPlot    Displays and tests pairwise correlations of assets
################################################################################


.assetsSeriesPlot =
function(x, which = 1:dim(x)[2], ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Displays histograms of individual assets 
    
    # Arguments:
    #   x - a timeSeries object or any other rectangular object
    #       which can be transformed by the function as. matrix
    #       into a numeric matrix.
    #   which - an integer value or vector specifying the number(s)
    #       of the assets which are selected to be plotted. 
    
    # FUNCTION:
    
    # Settings:
    method = match.arg(method)

    # Plot:
    for (i in which) {
        seriesPlot(x[, i], ...)
    }
        
    # Return Value:
    invisible()
}



# ------------------------------------------------------------------------------


.assetsHistPlot =
function(x, method = c("cov", "mve", "mcd", "nnve", "shrink", "bagged"), 
which = 1:dim(x)[2], xlim = NULL, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Displays histograms of individual assets 
    
    # Arguments:
    #   x - a timeSeries object or any other rectangular object
    #       which can be transformed by the function as. matrix
    #       into a numeric matrix.
    #   method - the method to be used. 
    #       "cov",
    #       "mve", minimum volume ellipsoid,
    #       "mcd", minimum covariance determinant method,  
    #       "nnve", 
    #       "shrink", 
    #       "bagged" .
    #   which - an integer value or vector specifying the number(s)
    #       of the assets which are selected to be plotted. 
    
    # FUNCTION:
    
    # Settings:
    method = match.arg(method)
    
    # Robust Estimation:
    covRob = assetsMeanCov(x, method, ...)
    
    # Plot:
    for (i in which) {
        # Classical Histogram:
        histPlot(x[, i], ...)
        
        # Robust Gaussian Fit:
        xlim = range(x[, i])
        u = seq(xlim[1], xlim[2], length = 201)
        v = dnorm(u, mean = covRob$mu[i], sd = sqrt(covRob$Sigma[i, i]))
        abline(v = covRob$mu[i], col = "darkgreen")
        lines(u, v, col = "darkgreen", lwd = 2)
    }
        
    # Return Value:
    invisible()
} 


# ------------------------------------------------------------------------------


.assetsDensityPlot =
function(x, method = c("cov", "mve", "mcd", "nnve", "shrink", "bagged"), 
which = 1:dim(x)[2], ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Displays density plots of individual assets
    
    # Arguments:
    #   x - a timeSeries object or any other rectangular object
    #       which can be transformed by the function as. matrix
    #       into a numeric matrix. 
    #   method - the method to be used. 
    #       "cov",
    #       "mve", minimum volume ellipsoid,
    #       "mcd", minimum covariance determinant method,  
    #       "nnve", 
    #       "shrink", 
    #       "bagged" .
    #   which - an integer value or vector specifying the number(s)
    #       of the assets which are selected to be plotted. 
    
    # FUNCTION:
    
    # Settings:
    method = match.arg(method)
    
    # Robust Estimation:
    covRob = assetsMeanCov(x, method, ...)
    
    # Plot:
    for (i in which) {
        densityPlot(x[, i], ...)
                
        # Robust Gaussian Fit:
        xlim = range(x[, i])
        u = seq(xlim[1], xlim[2], length = 201)
        v = dnorm(u, mean = covRob$mu[i], sd = sqrt(covRob$Sigma[i, i]))
        abline(v = covRob$mu[i], col = "darkgreen")
        lines(u, v, col = "darkgreen", lwd = 2)
    }
        
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


.assetsQQNormPlot =
function(x, which = 1:dim(x)[2], ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Displays normal qq-plots of individual assets
    
    # Arguments:
    #   x - a timeSeries object or any other rectangular object
    #       which can be transformed by the function as. matrix
    #       into a numeric matrix. 
    #   which - an integer value or vector specifying the number(s)
    #       of the assets which are selected to be plotted. 
    
    # FUNCTION:
    
    # Plot:
    for (i in which) {
        qqnormPlot(x[, i], ...)
    }
        
    # Return Value:
    invisible()
}


################################################################################


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
        
    # Return Value:
    invisible()
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
        cex.labels = 2*scale, cex = 2*scale, cex.axis = 1.5*scale, ...)
        
    # Return Value:
    invisible()
}


################################################################################

