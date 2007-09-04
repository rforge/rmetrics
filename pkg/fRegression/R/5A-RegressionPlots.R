
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
# FUNCTIONS:
#  .terms.fREG
#  .response2Plot
################################################################################


.terms.fREG = 
function(object, formula = Y ~ X1)
{   # A function implemented by Diethelm Wuertz

    # Description:
    
    # Arguments:
    
    # FUNCTION:
    
    select = all.vars(formula)
    print(class(object@fit)[-1])
    
    fit = object@fit
    data = as.data.frame(object@data)
    X = predict(fit, data, type = "terms")[, select[2]]
    Y = predict(fit, data, type = "response")
    
    plot(X, Y, xlab = select[2], ylab = select[1], col = "steelblue", pch = 19)
    grid()   
    rug(X)
    
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


.response2Plot = 
function(object, formula = Y ~ X1 + X2, N = 10, fun = mean)
{   # A function implemented by Diethelm Wuertz

    # Description:
    
    # Arguments:
    
    # FUNCTION:
    
    .grid2d = function (x = (0:10)/10, y = x) 
    {
        nx = length(x)
        ny = length(y)
        xoy = cbind(rep(x, ny), as.vector(matrix(y, nx, ny, byrow = TRUE)))
        XY = matrix(xoy, nx * ny, 2, byrow = FALSE)
        list(x = XY[, 1], y = XY[, 2])
    }

    Data = object@data
    select = all.vars(formula)
    
    X = data[, select[2]]
    Y = data[, select[3]]
    Z = data[, select[1]]
    rangeX = range(X)
    rangeY = range(Y)
    statsData = colStats(Data, fun)
    
    U = seq(rangeX[1], rangeX[2], length = N)
    V = seq(rangeY[1], rangeY[2], length = N)
    newGrid = .grid2d(U, V)
    
    newData = matrix(rep(statsData, times = N*N), 
        byrow = TRUE, ncol = ncol(Data))
    colnames(newData) = colnames(Data)
    newData[, select[2]] = newGrid$x
    newData[, select[3]] = newGrid$y
    newData[, select[1]] = NA
    newData = data.frame(newData)
    P = predict(object, newdata = newData)$fit
    
    W = matrix(P, byrow = FALSE, ncol = N)
    persp(U, V, W, xlab = select[2], ylab = select[3], zlab = select[1],
        phi = 30, theta = -40, col = "steelblue")->res
        
    R = sign(object@residuals)
    points(trans3d(X[R>0], Y[R>0], Z[R>0], pm = res), col = 5, pch =16)
    points(trans3d(X[R<0], Y[R<0], Z[R<0], pm = res), col = 6, pch =16)
    
    # Return Value:
    invisible()
}


################################################################################

