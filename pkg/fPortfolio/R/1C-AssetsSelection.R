
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

