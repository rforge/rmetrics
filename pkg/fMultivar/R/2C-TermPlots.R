
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
# FUNCTION:                 REGRESSION TERM PLOTS
#  .termPlot                 Line Plot          
#  .termPlot.default
#  .termPlot.fREG
#  .termPersp                Perspective Plot         
#  .termPersp.default
#  .termPersp.fREG
#  .termContour              Contour Plot             
#  .termContour.default
#  .termContour.fREG
################################################################################


.termPlot = 
function(model, ...)
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Return Value:
    UseMethod(".termPlot")
}
    

# ------------------------------------------------------------------------------


.termPlot.default = 
function(model, formula = NULL, rug = TRUE, partial.resid = TRUE, ...)
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
     
    # Add Predict Methods:
    
    # Data Matrix:
    data = as.data.frame(model.frame(model))
    X = data[, -1]
    Y = data[, 1]
    meanX = matrix(sapply(X, mean), nrow = nrow(X), ncol = ncol(X), byrow = TRUE)
    colnames(meanX) <- colnames(X)
              
    if (is.null(formula)) {
        # Plot:
        namesX = all.vars(model$terms)[-1]
        namesY = all.vars(model$terms)[1]
        for (i in 1:length(namesX)) {    
            # Data:
            Xp = meanX
            Xq = Xp[, i] = X[, i] 
            Yp = .predict(model, data.frame(Xp), se.fit = TRUE) 
            if (is.numeric(Yp)) Yp = list(fit = Yp, se.fit = 0*Yp)
            Yr = model$residuals + Yp$fit     
            # Sort:
            IDX = sort(Xq, index.return = TRUE)$ix 
            xIDX = Xq[IDX]
            yIDX = Yp$fit[IDX]
            dyIDX = Yp$se.fit[IDX]
            yrange = range(c(yIDX, Yr))   
            # Plot:
            plot(xIDX, yIDX, type = "l", ylim = yrange, col = "red",
                xlab = namesX[i], ylab = namesY, ...) 
            CLASS = strsplit(class(model)[1], "\\.")[[1]][1]
            Main  = paste(CLASS, ": ", namesY, " ~ ", namesX[i], sep = "")
            title(main = Main)
            lines(xIDX, yIDX + dyIDX, col = "red", lty = 3)
            lines(xIDX, yIDX - dyIDX, col = "red", lty = 3)
            grid()      
            # Add Residuals:
            points(Xq, Yr, pch = 19, cex = 0.7, col = "grey")
            lines(xIDX, yIDX, col = "red")
            # Add Rug:
            rug(Xq, ticksize = 0.03, side = 1, quiet = TRUE) 
            rug(Yr, ticksize = 0.03, side = 2, quiet = TRUE)
        }
    } else {
        namesX = all.vars(formula)[2]
        namesY = all.vars(formula)[1]
        Xp = meanX
        Xq = Xp[, namesX] = X[, namesX]  
        Yp = .predict(model, data.frame(Xp), se.fit = TRUE) 
        if (is.numeric(Yp)) Yp = list(fit = Yp, se.fit = 0*Yp)
        Yr = model$residuals + Yp$fit   
        # Sort:
        IDX = sort(Xq, index.return = TRUE)$ix 
        xIDX = Xq[IDX]
        yIDX = Yp$fit[IDX]
        dyIDX = Yp$se.fit[IDX]
        yrange = range(c(yIDX, Yr))   
        # Plot:
        plot(xIDX, yIDX, type = "l", ylim = yrange, col = "red",
            xlab = namesX, ylab = namesY) 
        CLASS = strsplit(class(model)[1], "\\.")[[1]][1]
        Main  = paste(CLASS, ": ", namesY, " ~ ", namesX, sep = "")
        title(main = Main)
        lines(xIDX, yIDX + dyIDX, col = "red", lty = 3)
        lines(xIDX, yIDX - dyIDX, col = "red", lty = 3)
        grid()      
        # Add Residuals:
        points(Xq, Yr, pch = 19, cex = 0.7, col = "grey")
        lines(xIDX, yIDX, col = "red")
        # Add Rug:
        rug(Xq, ticksize = 0.03, side = 1, quiet = TRUE) 
        rug(Yr, ticksize = 0.03, side = 2, quiet = TRUE)
    }
    
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


.termPlot.fREG = 
function(model, formula = NULL, rug = TRUE, partial.resid = TRUE, ...)
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # term Plot:
    CLASS = class(model@fit)[-1]
    model = model@fit
    class(model) = CLASS
    .termPlot.default(model, formula, rug, partial.resid, ...)
    
    # Return Value:
    invisible()
}


################################################################################   


.termPersp = 
function(model, ...)
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Return Value:
    UseMethod(".termPersp")
}
    

# ------------------------------------------------------------------------------
 

.termPersp.default = 
function(model, formula = NULL, M = 25, ...)
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
        
    # Add Predict Methods:
    predict.mars = .predict.mars
    predict.polymars = .predict.polymars
    
    # Grid Points:
    Mx = My = M
    
    # Data Matrix:
    data = as.data.frame(model.frame(model))
    X = data[, -1]
    Y = data[, 1]
    meanX = matrix(sapply(X, mean), nrow = Mx*My, ncol = ncol(X), byrow = TRUE)
    colnames(meanX) <- colnames(X)
      
    if (is.null(formula)) {
        # Plot:
        N = length(namesX)
        namesX = all.vars(model$terms)[-1]
        namesY = all.vars(model$terms)[1]
        for (i in 1:(N-1)) {    
            xrange = range(X[, i])
            x = seq(xrange[1], xrange[2], length = Mx)
            for (j in (i+1):N) { 
                yrange = range(X[, j])
                y = seq(yrange[1], yrange[2], length = My)
                # Grid 2D:
                xoy = cbind(rep(x, My), as.vector(matrix(y, Mx, My, byrow = TRUE)))
                XY = matrix(xoy, Mx * My, 2, byrow = FALSE)
                gridX = XY[, 1]
                gridY = XY[, 2]
                Z = meanX
                Z[, i] = gridX
                Z[, j] = gridY
                Zp = .predict(model, data.frame(Z))               
                # Plot:
                z = matrix(Zp, nrow = Mx) 
                .perspPlot(x, y, z, xlab = namesX[i], ylab = namesX[j], 
                    zlab = namesY, ...)
                Main = paste(class(model)[1], ": ", 
                    namesY, " ~ ", namesX[i], " + ", namesX[j], sep = "")
                title(main = Main)
            }
        }
    } else {
        namesX = all.vars(formula)[2:3]
        stopifnot(namesX[1] != namesX[2])
        namesY = all.vars(formula)[1] 
        xrange = range(X[, namesX[1]])
        x = seq(xrange[1], xrange[2], length = Mx)
        yrange = range(X[, namesX[1]])
        y = seq(yrange[1], yrange[2], length = My)
        # Grid 2D:
        xoy = cbind(rep(x, My), as.vector(matrix(y, Mx, My, byrow = TRUE)))
        XY = matrix(xoy, Mx * My, 2, byrow = FALSE)
        gridX = XY[, 1]
        gridY = XY[, 2]
        Z = meanX
        Z[, namesX[1]] = gridX
        Z[, namesX[2]] = gridY
        Zp = .predict(model, data.frame(Z))               
        # Plot:
        z = matrix(Zp, nrow = Mx) 
        .perspPlot(x, y, z, xlab = namesX[1], ylab = namesX[2], 
            zlab = namesY, ...)
        Main = paste(class(model)[1], ": ", 
            namesY, " ~ ", namesX[1], " + ", namesX[2], sep = "")
        title(main = Main)
    }
    
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


.termPersp.fREG = 
function(model, formula = NULL, M = 25, ...)
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # term Plot:
    .termPersp.default(model = model@fit, formula = formula, M = M, ...)
    
    # Return Value:
    invisible()
}


################################################################################   


.termContour = 
function(model, ...)
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Return Value:
    UseMethod(".termContour")
}
    

# ------------------------------------------------------------------------------
 

.termContour.default = 
function(model, formula = NULL, M = 25, ...)
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
        
    # Add Predict Methods:
    predict.mars = .predict.mars
    predict.polymars = .predict.polymars
    
    # Grid Points:
    Mx = My = M
    
    # Data Matrix:
    data = as.data.frame(model.frame(model))
    X = data[, -1]
    Y = data[, 1]
    meanX = matrix(sapply(X, mean), nrow = Mx*My, ncol = ncol(X), byrow = TRUE)
    colnames(meanX) <- colnames(X)
      
    if (is.null(formula)) {
        # Plot:
        N = length(namesX)
        namesX = all.vars(model$terms)[-1]
        namesY = all.vars(model$terms)[1]
        for (i in 1:(N-1)) {    
            xrange = range(X[, i])
            x = seq(xrange[1], xrange[2], length = Mx)
            for (j in (i+1):N) { 
                yrange = range(X[, j])
                y = seq(yrange[1], yrange[2], length = My)
                # Grid 2D:
                xoy = cbind(rep(x, My), as.vector(matrix(y, Mx, My, byrow = TRUE)))
                XY = matrix(xoy, Mx * My, 2, byrow = FALSE)
                gridX = XY[, 1]
                gridY = XY[, 2]
                Z = meanX
                Z[, i] = gridX
                Z[, j] = gridY
                Zp = .predict(model, data.frame(Z))               
                # Plot:
                z = matrix(Zp, nrow = Mx) 
                contour(x, y, z, xlab = namesX[i], ylab = namesX[j], ...)
                Main = paste(class(model)[1], ": ", 
                    namesY, " ~ ", namesX[i], " + ", namesX[j], sep = "")
                title(main = Main)
            }
        }
    } else {
        namesX = all.vars(formula)[2:3]
        stopifnot(namesX[1] != namesX[2])
        namesY = all.vars(formula)[1] 
        xrange = range(X[, namesX[1]])
        x = seq(xrange[1], xrange[2], length = Mx)
        yrange = range(X[, namesX[1]])
        y = seq(yrange[1], yrange[2], length = My)
        # Grid 2D:
        xoy = cbind(rep(x, My), as.vector(matrix(y, Mx, My, byrow = TRUE)))
        XY = matrix(xoy, Mx * My, 2, byrow = FALSE)
        gridX = XY[, 1]
        gridY = XY[, 2]
        Z = meanX
        Z[, namesX[1]] = gridX
        Z[, namesX[2]] = gridY
        Zp = predict(model, data.frame(Z))               
        # Plot:
        z = matrix(Zp, nrow = Mx) 
        contour(x, y, z, xlab = namesX[1], ylab = namesX[2], ...)
        Main = paste(class(model)[1], ": ", 
            namesY, " ~ ", namesX[1], " + ", namesX[2], sep = "")
        title(main = Main)
    }
    
    # Return Value:
    invisible()
} 


# ------------------------------------------------------------------------------


.termContour.fREG = 
function(model, formula = NULL, M = 25, ...)
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # term Plot:
    .termContour.default(model = model@fit, formula = formula, M = M, ...)
    
    # Return Value:
    invisible()
}


################################################################################

