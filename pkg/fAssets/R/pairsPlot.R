
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
# FUNCTION:                   PAIRS PLOTS:
#  assetsPairsPlot             Displays pairs of scatterplots of assets
#  assetsCorgramPlot           Displays correlations between assets
#  assetsCorTestPlot           Displays and tests pairwise correlations
#  assetsCorImagePlot          Displays an image plot of a correlations
################################################################################


assetsPairsPlot <- 
    function(x, labels = TRUE, ...)
{   
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Displays pairs of scatterplots of individual assets
    
    # Arguments:
    #   x - a timeSeries object or any other rectangular object
    #       which can be transformed by the function as. matrix
    #       into a numeric matrix.
    #   labels - a logical flag. Should default labels be printed? 
    #       Not implemented.
    
    # FUNCTION:
    
    # Settings:
    x = as.matrix(x)
    
    # Plot:
    pairs(x, ...)
        
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


assetsCorgramPlot <-
    function(x, labels = TRUE, method = c("pie", "shade", "hist"), ...)
{   
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Displays correlations between assets
    
    # Arguments:
    #   x - a timeSeries object or any other rectangular object
    #       which can be transformed by the function as. matrix
    #       into a numeric matrix.
    #   labels - a logical flag. Should default labels be printed?
    #       Not implemented.
    
    # Example:
    #   assetsCorgramPlot(x=100*as.timeSeries(data(LPP2005REC)))

    # FUNCTION:
    
    # Settings:
    method <<- match.arg(method)
    stopifnot(is.timeSeries(x))
    x = seriesData(x)
    
    # Internal Function:
    .panel.lower = function(x, y, ...) 
    {
        if (method[1] == "pie") {
            .panel.pie(x, y, ...)
            .panel.pts(x, y, ...) 
        } else if (method[1] == "shade") {
            .panel.shade(x, y, ...)
            .panel.pts(x, y, ...) 
        } else if (method[1] == "hist") {
            .panel.shade(x, y, ...)
            .panel.hist(x, y, ...)
        }
    } 
    .panel.upper = function(x, y, ...) 
    {
        .panel.ellipse(x, y, ...)
    }
        
    # Plot Corellogram - Pies and Ellipses:    
    .corrgram(x, 
        lower.panel = .panel.lower,
        upper.panel = .panel.upper, 
        text.panel = .panel.txt, ...)
        
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


assetsCorTestPlot <- 
    function(x, labels = TRUE, ...)
{   
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Displays and tests pairwise correlations of assets
    
    # Arguments:
    #   x - a timeSeries object or any other rectangular object
    #       which can be transformed by the function as. matrix
    #       into a numeric matrix.
    #   labels - a logical flag. Should default labels be printed?
    #       Not implemented.
    
    # FUNCTION:
    
    # Settings:
    x = as.matrix(x)
 
    # Upper Plot Function:
    cortestPanel <-
    function(x, y, cex, col, ...)
    {
        if (missing(col)) col = NULL
        usr = par("usr"); on.exit(par(usr))
        par(usr = c(0, 1, 0, 1))
        r = abs(cor(x, y))
        txt = format(c(r, 0.123456789), digits = 3)[1]
        test = cor.test(x, y)
        Signif = symnum(test$p.value, corr = FALSE, na = FALSE,
            cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
            symbols = c("*** ", "** ", "* ", ". ", "  "))
        text(0.5, 0.5, txt, cex = 1, col = NULL, ...)
        text(0.8, 0.8, Signif, cex = 1.5, col = col, ...)
    }
    
    # Lower Plot Function:
    lowessPanel =  
    function (x, y, ...) 
    {
        points(x, y, ...)
        ok = is.finite(x) & is.finite(y)
        if (any(ok)) lines(lowess(x[ok], y[ok]), col = "brown")
    }

    # Plot:
    pairs(x, 
        lower.panel = lowessPanel, 
        upper.panel = cortestPanel, ...)
        
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------

   
assetsCorImagePlot <-
    function(x, labels = TRUE, 
    show = c("cor", "test"), use = c("pearson", "kendall", "spearman"),
    abbreviate = 3, ...)
{   
    # A function implemented by Diethelm Wuertz
    #   @author Sandrine Dudoit, sandrine@stat.berkeley.edu, from "SMA" library
    #   @author modified by Peter Carl
    #   @author extended by Diethelm Wuertz

    # Description:
    #   Creates an image plot of a correlations

    # Arguments:
    #   R - data to be evaluated against its own members

    # Details:
    #   uses relative colors to indicate the strength of the pairwise 
    #   correlation.

    # Examples:
    #   R = as.timeSeries(data(edhec))
    #   palette(.rgPalette(NCOL(edhec)))
    #   correlationImage(edhec)

    # FUNCTION:
    
    # Settings:
    R = x
    
    # Match Arguments:
    show = match.arg(show)
    use = match.arg(use)
    
    # Handle Missing Values:
    R = na.omit(R, ...)
    
    # Abbreviate Instrument Names:
    Names = colnames(R) = substring(colnames(R), 1, abbreviate)  

    # Compute Correlation Matrix:
    R = as.matrix(R)
    n = NCOL(R)
    if (show == "cor") {
        corr <- cor(R, method = use) 
        if (show == "test") {
            test = corr*NA 
            for ( i in 1:n)
                for (j in 1:n)
                    test[i,j] = cor.test(R[,i], R[,j], method = use)$p.value
        }
    } else if (show == "robust") {
        stop("robust: Not Yet Implemented")  
    } else if (show == "shrink") {
        stop("robust: Not Yet Implemented")  
    }
    
    # Plot Image:
    image(x = 1:n, y = 1:n, z = corr[, n:1], col = 1:n,
        axes = FALSE, main = "", xlab = "", ylab = "", ...)
    
    # Add Text Values:
    if (show == "cor") X = t(corr) else X = t(test)
    coord = grid2d(1:n, 1:n)   
    for (i in 1:(n*n)) {
        text(coord$x[i], coord$y[n*n+1-i], 
            round(X[coord$x[i], coord$y[i]], digits = 2), 
            col = "white", cex = 0.7)
    }
        
    # Add Axis Labels:
    if(labels) {
        axis(2, at = n:1, labels = Names, las = 2)
        axis(1, at = 1:n, labels = Names, las = 2)
        Names = c(
            pearson = "Pearson", kendall = "Kendall", spearman = "Spearman")
        if (show == "test") Test = "Test" else Test = ""
        title(main = 
            paste(Names[use], "Corrleation ", Test, " Image", sep = ""))
        mText = paste("Method:", show)
        mtext(mText, side = 4, adj = 0, col = "grey", cex = 0.7)
    } 
    
    # Add Box:
    box()
       
    # Return Value:
    invisible()
}


################################################################################

