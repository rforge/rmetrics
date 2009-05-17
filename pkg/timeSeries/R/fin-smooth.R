#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  ../../COPYING


################################################################################
# FUNCTION:             DESCRIPTION:
#  .supsmuSmoother       Smoothes a time series with the supsmu function
#  .lowessSmoother       Smoothes a time series with the lowess function
#  .splineSmoother       Smoothes a time series with the smooth.spline function
################################################################################


# IMPORTANT:
#   DW: These are older functions which have to be rewritten ...
#   The functions are thought to be used to smooth financial 
#   price or index series.


# ------------------------------------------------------------------------------


.supsmuSmoother <- 
function(x, bass = 5, ...)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Smoothes a time series with the supsmu function
    
    # Arguments:
    #   x - an univariate timeSeries object, e.g. a price or index series
    #   bass - controls the smoothness of the fitted curve. Values of up 
    #       to 10 indicate increasing smoothness.
    #   ... - further arguments passed to the function smooth()
    
    # Example:
    #   x = .supsmuSmoother(MSFT[, 4], bass = 0.1); x; plot(x)
    
    # FUNCTION:
    
    # Settings:
    stopifnot(isUnivariate(x))
    
    # Handle Missing Values:
    x = na.omit(x, ...)
    
    # Convert to Vector:
    X = x
    x = as.vector(x)
    
    # Smooth:
    ans = stats::supsmu(x = 1:length(x), y = x, bass = bass, ... )
    data  = cbind(x, ans$y) 
    colnames(data) = c(colnames(X), "supsmu")
    rownames(data) = as.character(time(X))
    series(X) = data
    
    # Return Value:
    X
}


# ------------------------------------------------------------------------------


.lowessSmoother <- 
function(x, f = 0.5, ...)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Smoothes a time series with the lowess function
    
    # Arguments:
    #   x - an univariate timeSeries object, e.g. a price or index series
    #   f - the smoother span. This gives the proportion of points in the 
    #       plot which influence the smooth at each value. Larger values 
    #       give more smoothness.
    #   ... - further arguments passed to the function smooth()
    
    # Example:
    #   x = .lowessSmoother(MSFT[, 4], f = 0.05); x; plot(x)
    
    # FUNCTION:
    
    # Settings:
    stopifnot(isUnivariate(x))
    
    # Handle Missing Values:
    x = na.omit(x, ...)
    
    # Convert to Vector:
    X = x
    x = as.vector(x)
    
    # Smooth:
    ans = stats::lowess(x, f = f, ...)$y
    data  = cbind(x, ans) 
    colnames(data) = c(colnames(X), "lowess")
    rownames(data) = as.character(time(X))
    series(X) <- data
    
    # Return Value:
    X
}


################################################################################


.splineSmoother <- 
function(x, ...)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Smoothes a time series with the smooth.spline function
    
    # Arguments:
    #   x - an univariate timeSeries object, e.g. a price or index series
    #   f - the smoother span. This gives the proportion of points in the 
    #       plot which influence the smooth at each value. Larger values 
    #       give more smoothness.
    #   ... - further arguments passed to the function smooth.spline()
    
    # smooth.spline(x, y = NULL, w = NULL, df, spar = NULL, cv = FALSE, 
    #   all.knots = FALSE, nknots = NULL, keep.data = TRUE, df.offset = 0, 
    #   penalty = 1, control.spar = list()) 
   
    # Example:
    #   x = .splineSmoother(MSFT[, 4], spar = 0.4); x; plot(x)
    
    # FUNCTION:
    
    # Settings:
    stopifnot(isUnivariate(x))
    
    # Handle Missing Values:
    x = na.omit(x, ...)
    
    # Convert to Vector:
    X = x
    x = as.vector(x)
    
    # Smooth:
    ans = stats::smooth.spline(x, ...)$y
    data = cbind(x, ans) 
    colnames(data) = c(colnames(X), "spline")
    rownames(data) = as.character(time(X))
    series(X) <- data
    
    # Return Value:
    X
}


################################################################################

