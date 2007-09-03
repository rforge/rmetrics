
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
#  .interactiveRegressionPlot
#  .interactive.multPlot
#  .terms.fREG
#  .response2Plot
################################################################################


.interactiveRegressionPlot = 
function(x, choices = paste("Plot", 1:19), 
plotFUN = paste("plot.", 1:19, sep = ""), which = "all", ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Plot method for an object of class "template".
    
    # Arguments:
    #   x - an object to be plotted
    #   choices - the character string for the choice menu
    #   plotFUN - the names of the plot functions
    #   which - plot selection, which graph should be 
    #     displayed. If a character string named "ask" the 
    #     user is interactively asked which to plot, if
    #     a logical vector of length N, those plots which
    #     are set "TRUE" are displayed, if a character string
    #     named "all" all plots are displayed.
    
    # Note:
    #   At maximum 19 plots are supported.

    # FUNCTION:
    
    # Some cecks:
    if (length(choices) != length(plotFUN)) 
        stop("Arguments choices and plotFUN must be of same length.")
    if (length(which) > length(choices)) 
        stop("Arguments which has incorrect length.")
    if (length(which) > length(plotFUN)) 
        stop("Arguments which has incorrect length.")
    if (length(choices) > 19)
        stop("Sorry, only 19 plots at max are supported.")
    
                              
    # Plot:
    if (is.numeric(which)) {
        Which = rep(FALSE, times = length(choices))
        Which[which] = TRUE
        which = Which
    }
    if (which[1] == "all") {
        which = rep(TRUE, times = length(choices))
    }
    if (which[1] == "ask") {
        .interactive.multPlot(x, choices, ...) 
    } else {
        for ( i in 1:length(which) ) {
            FUN = match.fun(plotFUN[i])
            if (which[i]) FUN(x) 
        } 
    }
            
    # Return Value:
    invisible(x)
}


# ------------------------------------------------------------------------------

                
.interactive.multPlot = 
function (x, choices, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    
    # Arguments:
    
    # FUNCTION:
    
    # Match Functions, up to nine ...
    if (length(plotFUN) < 19) plotFUN = 
        c(plotFUN, rep(plotFUN[1], times = 19 - length(plotFUN)))
    plot.1  = match.fun(plotFUN[1]);  plot.2  = match.fun(plotFUN[2]) 
    plot.3  = match.fun(plotFUN[3]);  plot.4  = match.fun(plotFUN[4]) 
    plot.5  = match.fun(plotFUN[5]);  plot.6  = match.fun(plotFUN[6]) 
    plot.7  = match.fun(plotFUN[7]);  plot.8  = match.fun(plotFUN[8]) 
    plot.9  = match.fun(plotFUN[9]);  plot.10 = match.fun(plotFUN[10])
    plot.11 = match.fun(plotFUN[11]); plot.12 = match.fun(plotFUN[12]) 
    plot.13 = match.fun(plotFUN[13]); plot.14 = match.fun(plotFUN[14]) 
    plot.15 = match.fun(plotFUN[15]); plot.16 = match.fun(plotFUN[16]) 
    plot.17 = match.fun(plotFUN[17]); plot.18 = match.fun(plotFUN[18]) 
    plot.19 = match.fun(plotFUN[19])        
    pick = 1
    
    while (pick > 0) { 
        pick = menu (
        ### choices = paste("plot:", choices),
        choices = paste(" ", choices), 
        title = "\nMake a plot selection (or 0 to exit):")
        # up to 19 plot functions ...
        switch (pick, 
            plot.1(x),  plot.2(x),  plot.3(x),  plot.4(x),  plot.5(x), 
            plot.6(x),  plot.7(x),  plot.8(x),  plot.9(x),  plot.10(x),
            plot.11(x), plot.12(x), plot.13(x), plot.14(x), plot.15(x), 
            plot.16(x), plot.17(x), plot.18(x), plot.19(x)) 
    } 
}


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
    newGrid = grid2d(U, V)
    
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

