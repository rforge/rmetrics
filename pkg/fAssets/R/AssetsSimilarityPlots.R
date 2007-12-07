
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
# FUNCTION:                   SIMILARITY PLOTS:
#  assetsDendrogramPlot        Displays hierarchical clustering dendrogram
#  assetsCorEigenPlot          Displays ratio of the largest two eigenvalues
#  assetsTreePlot              Displays a minimum spanning tree of assets
################################################################################


assetsDendrogramPlot =
function(x, method = c(dist = "euclidian", clust = "complete"))
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Displays hierarchical clustering dendrogram
    
    # FUNCTION:
    
    # Compute Distance Matrix:
    if (class(x) == "dist") {
        DIST = x
    } else {
        X = t(seriesData(x))
        DIST = dist(X, method[1])
    }

    # Hierarchical Clustering:
    ans = hclust(DIST, method = method[2]) 
    
    # Plot Dendrogram:
    # main = substitute(x)
    plot(ans, xlab = "", main = "", sub = "")
    mtext(paste(
        "Distance Method:", method[1], " | ",
        "Clustering Method:", method[2]),
        side = 4, line = 0.1, adj = 0, col = "darkgrey", cex = 0.7)  
    box()
    
    # Return Value:
    invisible(list(dist = DIST, hclust = ans))
}


# ------------------------------------------------------------------------------


assetsCorEigenPlot =
function(x, method = c("pearson", "kendall", "spearman"), ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Displays ratio of the largest two eigenvalues
    
    # Arguments:
    #   x - a timeSeries object or any other rectangular object
    #       which can be transformed by the function as. matrix
    #       into a numeric matrix.
    
    # Example:
    #   assetsCorEigenPlot(x=100*as.timeSeries(data(LPP2005REC)))
    
    # FUNCTION:
    
    # Settings:
    stopifnot(is.timeSeries(x))
    x = seriesData(x)
    method = match.arg(method)
       
    # Plot:
    x.cor = cor(x, use = 'pair', method = method)
    x.eig = eigen(x.cor)$vectors[, 1:2]
    e1 = x.eig[, 1]
    e2 = x.eig[, 2]
    plot(e1, e2, col = 'white', 
        xlim = range(e1, e2), ylim = range(e1, e2), ...)
    abline(h = 0, lty = 3, col = "grey")
    abline(v = 0, lty = 3, col = "grey")
    arrows(0, 0, e1, e2, cex = 0.5, col = "steelblue", length = 0.1)
    text(e1, e2, rownames(x.cor))
    mtext(method, side = 4, adj = 0, cex = 0.7, col = "grey")
    
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


assetsTreePlot = 
function(x, method = "euclidian", seed = NULL)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Displays a minimum spanning tree of assets
    
    # FUNCTION:
    
    # Settings:
    Main = substitute(x)
    
    # Compute Distance Matrix:
    Order = NULL
    if (class(x) == "dist") {
        DIST = x
    } else {
        # Rank Seed:
        x = seriesData(x)
        if (is.null(seed)) {
            Order = sample(1:ncol(x))
            x = x[, Order]
        }
        DIST = dist(t(x), method[1])
    }
    method = attr(DIST, "method")
       
    # Compute Minimum Spanning Tree"
    MST = .mst(DIST)
      
    # Plot Tree:
    .mstPlot(MST, ".nsca", main = Main)
    mtext(paste("Distance Method:", method), 
        side = 4, line = 0.1, adj = 0, col = "darkgrey", cex = 0.7)
    
    # Return Value:
    invisible(list(mst = MST, dist = DIST, order = Order))
}


################################################################################

