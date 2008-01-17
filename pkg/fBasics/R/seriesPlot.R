
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the 
# GNU Library General Public License for more details.
#
# You should have received A copy of the GNU Library General 
# Public License along with this library; if not, write to the 
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
# MA  02111-1307  USA

# Copyrights (C)
# for this R-port: 
#   1999 - 2008, Diethelm Wuertz, Rmetrics Foundation, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:                TAILORED SERIES PLOTS:     
#  seriesPlot               Dispalys a time Series Plot           
#  cumulatedPlot            Displays a cumulated series given the returns
#  returnPlot               Displays returns given the cumulated series
#  drawdownPlot             Displays drawdown series from returns
################################################################################


seriesPlot <- 
    function(x, labels = TRUE, type = "l", col = "steelblue", 
    ylab = "Returns", grid = FALSE, rug = TRUE, ...) 
{   
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Dispalys a time Series Plot  
  
    # Arguments:
    #   x - an uni- or multivariate return series of class 'timeSeries' 
    #       or any other object which can be transformed by the function
    #       'as.timeSeries()' into an object of class 'timeSeries'.
    
    # Example:
    # tS=timeSeries(cbind(rnorm(12),rt(12,4)),timeCalendar(),units=c("N","T"))
    # seriesPlot(tS)
    
    # FUNCTION:

    # timeSeries:
    if (!is.timeSeries(x)) x = as.timeSeries(x)
    Units = x@units
    DIM = dim(x@Data)[2]
    if (length(col) == 1) col = rep(col, times = DIM)
    
    # Series Plots:
    for (i in 1:DIM) {
        X = x[, i]
        if (labels) {
            plot(x = X, type = type, col = col[i], 
                main = Units[i], ylab = ylab, xlab = "Time", ...)
            if (grid) grid()
        } else {
            plot(x = X, col = col[i], ...)   
        }
        
        # Add Zero Line:
        abline(h = 0, col = "grey")
        
        # Add Rugs:
        if (rug) {
            rug(as.vector(X), ticksize = 0.01, side = 4, quiet = TRUE)
        }
            
    }
         
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


cumulatedPlot <-  
    function(x, index = 100, labels = TRUE, type = "l", col = "steelblue", 
    ylab = "Index", grid = FALSE, rug = TRUE, ...) 
{   
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Displays a cumulated series given the returns
  
    # Arguments:
    #   x - an uni- or multivariate return series of class 'timeSeries' 
    #       or any other object which can be transformed by the function
    #       'as.timeSeries()' into an object of class 'timeSeries'.
    
    # Example:
    # tS=timeSeries(cbind(rnorm(12),rt(12,4)),timeCalendar(),units=c("N","T"))
    # seriesPlot(tS)
    
    # FUNCTION:

    # timeSeries:
    if (!is.timeSeries(x)) x = as.timeSeries(x)
    x = index * exp(colCumsums(x))
    Units = x@units
    DIM = dim(x@Data)[2]
    if (length(col) == 1) col = rep(col, times = DIM)
    
    # Series Plots:
    for (i in 1:DIM) {
        X = x[, i]
        if (labels) {
            plot(x = X, type = type, col = col[i], 
                main = Units[i], ylab = ylab, xlab = "Time", ...)
            if (grid) grid()
        } else {
            plot(x = X, col = col[i], ...)   
        }
        abline(h = 0, col = "grey")
        if (rug) {
            rug(as.vector(X), ticksize = 0.01, side = 4, quiet = TRUE)
        }
            
    }
         
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


returnPlot <-  
    function(x, labels = TRUE, type = "l", col = "steelblue", 
    ylab = "Returns", grid = FALSE, rug = TRUE, ...) 
{   
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Displays returns given the cumulated series
  
    # Arguments:
    #   x - an uni- or multivariate return series of class 'timeSeries' 
    #       or any other object which can be transformed by the function
    #       'as.timeSeries()' into an object of class 'timeSeries'.
    
    # Example:
    # tS=timeSeries(cbind(rnorm(12),rt(12,4)),timeCalendar(),units=c("N","T"))
    # seriesPlot(tS)
    
    # FUNCTION:

    # timeSeries:
    if (!is.timeSeries(x)) x = as.timeSeries(x)
    x = returns(x, ...)
    Units = x@units
    DIM = dim(x@Data)[2]
    if (length(col) == 1) col = rep(col, times = DIM)
    
    # Return Plots:
    for (i in 1:DIM) {
        X = x[, i]
        if (labels) {
            plot(x = X, type = type, col = col[i], 
                main = Units[i], ylab = ylab, xlab = "Time", ...)
            if (grid) grid()
        } else {
            plot(x = X, col = col[i], ...)   
        }
        abline(h = 0, col = "grey")
        if (rug) {
            rug(as.vector(X), ticksize = 0.01, side = 4, quiet = TRUE)
        }
            
    }
         
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


drawdownPlot <-  
    function(x, labels = TRUE, type = "l", col = "steelblue", 
    ylab = "Drawdowns", grid = FALSE, rug = TRUE, ...) 
{   
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Displays drawdowns given the return series
  
    # Arguments:
    #   x - an uni- or multivariate return series of class 'timeSeries' 
    #       or any other object which can be transformed by the function
    #       'as.timeSeries()' into an object of class 'timeSeries'.
    
    # Example:
    # tS=timeSeries(cbind(rnorm(12),rt(12,4)),timeCalendar(),units=c("N","T"))
    # seriesPlot(tS)
    
    # FUNCTION:

    # timeSeries:
    if (!is.timeSeries(x)) x = as.timeSeries(x)
    Units = x@units
    DIM = dim(x@Data)[2]
    if (length(col) == 1) col = rep(col, times = DIM)
    
    # Return Plots:
    for (i in 1:DIM) {
        X = drawdowns(x[, i])
        if (labels) {
            plot(x = X, type = type, col = col[i], 
                main = Units[i], ylab = ylab, xlab = "Time", ...)
            if (grid) grid()
        } else {
            plot(x = X, col = col[i], ...)   
        }
        abline(h = 0, col = "grey")
        if (rug) {
            rug(as.vector(X), ticksize = 0.01, side = 4, quiet = TRUE)
        }
            
    }
         
    # Return Value:
    invisible()
}


################################################################################

