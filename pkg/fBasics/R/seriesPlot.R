
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
    function(x, type = "l", col = "steelblue", 
    FUN = noDecor, ...) 
{   
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Dispalys a time Series Plot  
  
    # Arguments:
    #   x - an uni- or multivariate return series of class 'timeSeries' 
    #       or any other object which can be transformed by the function
    #       'as.timeSeries()' into an object of class 'timeSeries'.
    
    # FUNCTION:
 
    # timeSeries:
    stopifnot(is.timeSeries(x))
    N = NCOL(x)
    decor = match.fun(FUN)
    if (length(col) == 1) col = rep(col, times = N)
     
    # Series Plots:
    for (i in 1:N) {
        X = x[, i] 
        plot(x = X, type = type, col = col[i], ann = FALSE, ...)
        title(...)
        decor(x = X)      
    }
    
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


cumulatedPlot <-  
    function(x, index = 100, type = "l", col = "steelblue", 
    FUN = noDecor, ...) 
{   
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Displays a cumulated series given the returns
    
    # FUNCTION:

    # timeSeries:
    stopifnot(is.timeSeries(x))
    x = index * exp(colCumsums(x))
    seriesPlot(x = x, type = type, col = col, FUN = FUN, ...)
         
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


returnPlot <-  
    function(x, type = "l", col = "steelblue", 
    FUN = noDecor, ...) 
{   
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Displays returns given the cumulated series
    
    # FUNCTION:

    # timeSeries:
    stopifnot(is.timeSeries(x))
    x = returns(x, ...)
    seriesPlot(x = x, type = type, col = col, FUN = FUN, ...)
         
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


drawdownPlot <-  
    function(x, type = "l", col = "steelblue", 
    FUN = noDecor, ...) 
{   
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Displays drawdowns given the return series
    
    # FUNCTION:

    # timeSeries:
    stopifnot(is.timeSeries(x))
    x = drawdowns(x, ...)
    seriesPlot(x = x, type = type, col = col, FUN = FUN, ...)
         
    # Return Value:
    invisible()
}


################################################################################

