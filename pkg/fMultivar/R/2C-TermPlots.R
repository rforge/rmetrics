
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# A copy of the GNU General Public License is available via WWW at
# http://www.gnu.org/copyleft/gpl.html.  You can also obtain it by
# writing to the Free Software Foundation, Inc., 59 Temple Place,
# Suite 330, Boston, MA  02111-1307  USA. 

# Copyrights (C)
# for this R-port: 
#   1999 - 2006, Diethelm Wuertz, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:                 REGRESSION TERM PLOTS
#  .termPlot                 Line Plot          
#  .termPersp                Perspective Plot         
#  .termContour              Contour Plot             
################################################################################


.termPlot =
function(object, terms = NULL)
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Settings:
    data = object@fit$model
    colNames = all.vars(object@formula)
    data = as.data.frame(object@data)[, colNames]
    X = data[, -1]
    Y = data[, 1]
    if (is.null(terms)) namesX = colNames[-1] else namesX = terms
    namesY = colnames(data)[1]
    meanX = matrix(sapply(X, mean), nrow = nrow(X), ncol = ncol(X), 
        byrow = TRUE)
    colnames(meanX) <- colNames[-1]
     
    # Terms:   
    for (term in namesX) {    
        # Data:
        Xp = meanX
        Xq = Xp[, term] = X[, term]
        Yp = predict(object, Xp, se.fit = TRUE)
        Yr = as.vector(object@residuals) + Yp$fit
        # Sort:
        IDX = sort(Xq, index.return = TRUE)$ix 
        xIDX = Xq[IDX]
        yIDX = Yp$fit[IDX]
        dyIDX = Yp$se.fit[IDX]
        yrange = range(c(yIDX, Yr))   
        # Plot:
        plot(xIDX, yIDX, type = "l", ylim = yrange, col = "red",
            xlab = term, ylab = namesY) 
        Main  = paste(object@method, ": ", namesY, " ~ ", term, sep = "")
        title(main = Main)
        lines(xIDX, yIDX + dyIDX, col = "red", lty = 3)
        lines(xIDX, yIDX - dyIDX, col = "red", lty = 3)
        grid()      
        # Add Residuals:
        points(as.vector(Xq), Yr, pch = 19, cex = 0.7, col = "grey")
        lines(xIDX, yIDX, col = "red")
        # Add Rug:
        rug(Xq, ticksize = 0.03, side = 1, quiet = TRUE) 
        rug(Yr, ticksize = 0.03, side = 2, quiet = TRUE)
    }
    
    # Return Value:
    invisible()
}


################################################################################   


.termPersp =
function(object, terms = NULL, gridPoints = 25)
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Settings:
    data = object@fit$model
    colNames = all.vars(object@formula)
    data = as.data.frame(object@data)[, colNames]
    X = data[, -1]
    Y = data[, 1]
    if (is.null(terms)) namesX = colNames[-1] else namesX = terms
    namesY = colnames(data)[1]
    Mx = My = gridPoints 
    meanX = matrix(sapply(X, mean), nrow = Mx*My, ncol = ncol(X), 
        byrow = TRUE)
    colnames(meanX) <- colNames[-1]
    
    # Terms:
    N = length(namesX)
    for (i in 1:(N-1)) {    
        xrange = range(X[, namesX[i]])
        x = seq(xrange[1], xrange[2], length = Mx)
        for (j in (i+1):N) { 
            yrange = range(X[, namesX[j]])
            y = seq(yrange[1], yrange[2], length = My)
            # Grid 2D:
            xoy = cbind(rep(x, My), as.vector(matrix(y, Mx, My, byrow = TRUE)))
            XY = matrix(xoy, Mx * My, 2, byrow = FALSE)
            gridX = XY[, 1]
            gridY = XY[, 2]
            Z = meanX
            Z[, namesX[i]] = gridX
            Z[, namesX[j]] = gridY
            Zp = predict(object, Z)               
            # Plot:
            z = matrix(Zp, nrow = Mx) 
            .perspPlot(x, y, z, xlab = namesX[i], ylab = namesX[j], 
                zlab = namesY)
            Main = paste(object@method, ": ", namesY, " ~ ", namesX[i], " + ", 
                namesX[j], sep = "")
            title(main = Main)
        }
    }
    
    # Return Value:
    invisible()
}


################################################################################   


.termContour =
function(object, terms = NULL, gridPoints = 25)
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Settings:
    data = object@fit$model
    colNames = all.vars(object@formula)
    data = as.data.frame(object@data)[, colNames]
    X = data[, -1]
    Y = data[, 1]
    if (is.null(terms)) namesX = colNames[-1] else namesX = terms
    namesY = colnames(data)[1]
    Mx = My = gridPoints 
    meanX = matrix(sapply(X, mean), nrow = Mx*My, ncol = ncol(X), 
        byrow = TRUE)
    colnames(meanX) <- colNames[-1]
    
    # Terms:
    N = length(namesX)
    for (i in 1:(N-1)) {    
        xrange = range(X[, namesX[i]])
        x = seq(xrange[1], xrange[2], length = Mx)
        for (j in (i+1):N) { 
            yrange = range(X[, namesX[j]])
            y = seq(yrange[1], yrange[2], length = My)
            # Grid 2D:
            xoy = cbind(rep(x, My), as.vector(matrix(y, Mx, My, byrow = TRUE)))
            XY = matrix(xoy, Mx * My, 2, byrow = FALSE)
            gridX = XY[, 1]
            gridY = XY[, 2]
            Z = meanX
            Z[, namesX[i]] = gridX
            Z[, namesX[j]] = gridY
            Zp = predict(object, Z)               
            # Plot:
            z = matrix(Zp, nrow = Mx) 
            contour(x, y, z, xlab = namesX[i], ylab = namesX[j])
            Main = paste(object@method, ": ", namesY, " ~ ", namesX[i], " + ", 
                namesX[j], sep = "")
            title(main = Main)
        }
    }
    
    # Return Value:
    invisible()
}


################################################################################

