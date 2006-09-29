
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
# You should have received a copy of the GNU Library General
# Public License along with this library; if not, write to the
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston,
# MA  02111-1307  USA

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
# FUNCTION:                 BENCHMARK ANALYSIS FUNCTIONS:
#  getReturns                Computes return series given a price series
#  maxDrawDown               Computes the maximum drawdown
#  sharpeRatio               Calculates the Sharpe Ratio
#  sterlingRatio             Calculates the Sterling Ratio
#  ohlcPlot                  Creates a Open-High-Low-Close plot
################################################################################


getReturns = 
function(x, type = c("continuous", "discrete"), percentage = FALSE, 
trim = TRUE)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes returns given a price series, for SPlus compatibility.
    
    # Arguments:
    #   type - a character string specifying the type of returns to be 
    #       computed. Valid choices are: "continuous" and "discrete". 
    #       If type="continuous", the returns are calculated as the 
    #       logarithm differences; if type="discrete", the returns are 
    #       calculated as percentage changes. The default is "continuous" 
    #   percentage - a logical flag. If TRUE, the return series will be 
    #       expressed in percentage points. The default is FALSE. 
    #   trim - a logical flag. If TRUE, the first missing observation 
    #       in the return series will be removed. The default is TRUE. 

    # Note:
    #   Function for S-Plus Compatibility
    #   getReturns(x, type = "continuous", percentage = FALSE, trim = TRUE) 
    
    # FUNCTION:
    
    # Check Type:
    type = match.arg(type)
    
    # Return Series:
    ans = returnSeries(x = x, type = type, percentage = percentage, 
        trim = trim, digits = 12)
        
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


maxDrawDown = 
function(x)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes the maximum drawdown
    
    # FUNCTION:
    
    # Check for timeSeries Object:
    TS = FALSE
    if (is.timeSeries(x)) {
        TS = TRUE
        positions = x@positions
        x = seriesData(x)
        x = removeNA(x)
    }

    # Check for Univariate Series:
    if (NCOL(x) > 1) {
        stop("x is not a vector or univariate timeSeries")
    }
    
    # Check for NAs:
    if(any(is.na(x))) {
        stop("NAs in x")
    }
    
    cmaxx = cummax(x)-x
    mdd = max(cmaxx)
    to = which(mdd == cmaxx)
    from = double(NROW(to))
    for (i in 1:NROW(to))
        from[i] = max(which(cmaxx[1:to[i]] == 0))   
        
    if (TS) {
        from = positions[from]
        to = positions[to]
    }
        
    # Return Value:
    list(maxdrawdown = mdd, from = from, to = to)
}


# ------------------------------------------------------------------------------


sharpeRatio = 
function(x, r = 0, scale = sqrt(250))
{   # A function implemented by Diethelm Wuertz

    # Notes:
    #   A copy from A. Traplettis "tseries" package
    
    # FUNCTION:
    
    # Check for timeSeries Object:
    if (is.timeSeries(x)) { 
        x = seriesData(x)
        x = removeNA(x)
        
    }
    
    # Check for Univariate Series:
    if(NCOL(x) > 1)
        stop("x is not a vector or univariate time series")
        
    # Check for NAs:
    if(any(is.na(x)))
        stop("NAs in x")
     
    # Sharpe Ratio:
    if(NROW(x) == 1) {
        return(NA)
    } else {
        y = diff(x)
        return(scale * (mean(y)-r)/sd(y))
    }
    
    # Return Value:
    invsible()
}


# ------------------------------------------------------------------------------


sterlingRatio = 
function(x)
{   # A function implemented by Diethelm Wuertz

    # Notes:
    #   A copy from A. Traplettis "tseries" package
    
    # FUNCTION:
    
    # Check for timeSeries Object:
    TS = FALSE
    if (is.timeSeries(x)) {
        TS = TRUE
        Unit = x@units
        x = seriesData(x)
        x = removeNA(x)
    }
        
    # Check for Univariate Series:
    if(NCOL(x) > 1)
        stop("x is not a vector or univariate time series")
        
    # Check for NAs:
    if(any(is.na(x)))
        stop("NAs in x")
        
    # Sterling Ratio:
    if (NROW(x) == 1) {
        return(NA)
    } else {
        ans = (x[NROW(x)]-x[1]) / maxDrawDown(x)$maxdrawdown
        if (TS) names(ans) = Unit
        return(ans)
    }
    
    # Return Value:
    invsible()
}


# ------------------------------------------------------------------------------


ohlcPlot = 
function(x, xlim = NULL, ylim = NULL, xlab = "Time", ylab, col = par("col"), 
bg = par("bg"), axes = TRUE, frame.plot = axes, ann = par("ann"), main = NULL,
date = c("calendar", "julian"), format = "%Y-%m-%d",
origin = "1899-12-30", ...)
{   # A function implemented by Diethelm Wuertz

    # Notes:
    #   A copy from A. Traplettis 'tseries' package
    
    # FUNCTION:
    
    if ((!is.mts(x)) ||
        (colnames(x)[1] != "Open") ||
        (colnames(x)[2] != "High") ||
        (colnames(x)[3] != "Low") ||
        (colnames(x)[4] != "Close"))
        stop("x is not a open/high/low/close time series")
    xlabel = if (!missing(x)) 
        deparse(substitute(x))
    else NULL
    if (missing(ylab)) {
        ylab = xlabel
    }
    date = match.arg(date)
    time.x = time(x)
    dt = min(lag(time.x)-time.x)/3
    ylim = c(min(x, na.rm = TRUE), max(x, na.rm = TRUE))
    if (is.null(xlim)) 
        xlim = range(time.x)
    if (is.null(ylim)) 
        ylim = range(x[is.finite(x)])
    plot.new()
    plot.window(xlim, ylim, ...)
    for (i in 1:NROW(x)) {
        segments(time.x[i], x[i,"High"], time.x[i], x[i,"Low"],
            col = col[1], bg = bg)
        segments(time.x[i] - dt, x[i,"Open"], time.x[i], x[i,"Open"],
            col = col[1], bg = bg)
        segments(time.x[i], x[i,"Close"], time.x[i] + dt, x[i,"Close"],
            col = col[1], bg = bg)
    }
    if (ann) 
        title(main = main, xlab = xlab, ylab = ylab, ...)  
    if (axes) {
        if (date == "julian") {
            axis(1, ...)
            axis(2, ...)
        } else {
            n = NROW(x)
            lab.ind = round(seq(1, n, length=5))
            D = as.vector(time.x[lab.ind]*86400) + 
                as.POSIXct(origin, tz = "GMT")
            DD = format.POSIXct(D, format = format, tz ="GMT")
            axis(1, at=time.x[lab.ind], lab=DD, ...)
            axis(2, ...)
        }
    }
    if (frame.plot) {
        box(...)
    }
}


################################################################################

