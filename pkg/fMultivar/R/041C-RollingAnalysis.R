
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
# FUNCTION:                 DESCRIPTION:
#  rollFun                   Compute Rolling Function Value
#   rollMean                  Compute Rolling Mean
#   rollVar                   Compute Rolling Variance
#   rollMin                   Compute Rolling Minimum
#   rollMax                   Compute Rolling Maximum
#   .roll.RUnit               Unit Testing
################################################################################


rollFun = 
function(x, n, trim = TRUE, na.rm = FALSE, FUN, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Compute rolling function value

    # FUNCTION:
    
    # Transform:
    x.orig = x
    if (is.timeSeries(x)) TS = TRUE else TS = FALSE
    if (TS) {
        positions = x.orig@positions
        x = x.orig@Data[, 1]
        
    } else {
        x = as.vector(x.orig)
        names(x) = NULL
    }
    
    # Remove NAs:
    if (na.rm) { 
        if (TS) positions = positions[!is.na(x)]
        x = as.vector(na.omit(x))
    }
    
    # Roll FUN:
    start = 1
    end = length(x)-n+1
    m = x[start:end]
    for (i in 2:n) {
        start = start + 1
        end = end + 1
        m = cbind(m, x[start:end])}
    
    # Result:
    ans = apply(m, MARGIN = 1, FUN = FUN, ...)
    
    # Trim:
    if (!trim) 
        ans = c(rep(NA, (n-1)), ans)
    if (trim & TS)
        positions = positions[-(1:(n-1))]
        
    # Back to timeSeries:
    if (TS) {
        ans = timeSeries(as.matrix(ans), positions, units = x.orig@units,
            FinCenter = x.orig@FinCenter)
    }
    
    # Return value:
    ans
}


# ------------------------------------------------------------------------------
    

rollMean = 
function(x, n = 9, trim = TRUE, na.rm = FALSE) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Compute rolling mean

    # Examples:
    #
    #   x = timeSeries(as.matrix(cumsum(rnorm(12))), timeCalendar(), 
    #       units = "rnorm",FinCenter = "GMT")
    #   rollMean(x, n = 4, trim = FALSE, na.rm = FALSE)
    #   rollMean(x, n = 4, trim = TRUE, na.rm = FALSE)
    #
    #   x@Data[8, ] = NA
    #   rollMean(x, n = 4, trim = FALSE, na.rm = FALSE)
    #   rollMean(x, n = 4, trim = FALSE, na.rm = TRUE)
    #   rollMean(x, n = 4, trim = TRUE, na.rm = TRUE)
    
    # FUNCTION:
    
    # Roll Mean:
    rmean = rollFun(x = x, n = n, trim = trim, na.rm = na.rm, FUN = mean) 
    
    # Return Value:
    rmean
}
    
    
# ------------------------------------------------------------------------------


rollVar  = 
function(x, n = 9, trim = TRUE, unbiased = TRUE, na.rm = FALSE) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Compute rolling variance

    # FUNCTION:
    
    if (is.timeSeries(x)) TS = TRUE else TS = FALSE 
   
    # Roll Var:
    rvar = rollFun(x = x, n = n, trim = trim, na.rm = na.rm, FUN = var) 
    
    # Unbiased ?
    if (!unbiased) {
        if (TS) {
            rvar@Data = (rvar@Data * (n-1))/n
        } else {
            rvar = (rvar * (n-1))/n
        }
    }

    # Return Value:
    rvar 
}
    

# ------------------------------------------------------------------------------


rollMax  = 
function(x, n = 9, trim = TRUE, na.rm = FALSE) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Compute rolling maximum

    # FUNCTION:
    
    # Roll Max:
    rmax = rollFun(x = x, n = n, trim = trim, na.rm = na.rm,  FUN = max)
    
    # Return Value:
    rmax 
}
    

# ------------------------------------------------------------------------------


rollMin  = 
function(x, n = 9, trim = TRUE, na.rm = FALSE) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Compute rolling function minimum

    # FUNCTION:
 
    # Roll Min:
    rmin = rollFun(x = x, n = n, trim = trim, na.rm = na.rm,  FUN = min) 
    
    # Return Value:
    rmin 
}


# ------------------------------------------------------------------------------


.roll.RUnit =
function()
{
    
    # Try Vector:
    cat("\n-------- Vector -------------------------------\n")
    
    # .roll.RUnit()
    n = 3
    
    for (na.rm in c(TRUE, FALSE)) {
        
        x = 1:10
        if (na.rm) x[6] = NA
        cat("\n\nna.rm: ", na.rm)
        cat("\nx: ", x, "\n")
        
        for (trim in c(TRUE, FALSE)) {
        
            cat("\ntrim: ", trim, "\n\n")
            
            # Sum:
            ans = rollFun(x, n = n, trim = trim, na.rm = na.rm, FUN = sum)
            print(ans)
            # 15 20 25 30 35 40 45 50 55 60 65 70 75 80 85 90
            
            # Mean:
            ans = rollFun(x, n = n, trim = trim, na.rm = na.rm, FUN = mean)
            #  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18
            print(ans)
            
            # Var:
            ans = rollFun(x, n = n, trim = trim, na.rm = na.rm, FUN = var)
            #  2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5
            print(ans)
            
            # Min:
            ans = rollFun(x, n = n, trim = trim, na.rm = na.rm, FUN = min)
            #  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
            print(ans)
            
            # Max:
            ans = rollFun(x, n = n, trim = trim, na.rm = na.rm, FUN = max)
            #  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
            print(ans)
            
        }
    }
    
    # Try timeSeries:
    cat("\n-------- Time Series --------------------------\n")
    charvec = paste("1999", 10, 11:20, sep = "-")
    ts = timeSeries(data = 1:10, charvec, units = "SERIES", zone = "GMT",
        FinCenter = "GMT") 
    
    # .roll.RUnit()
    n = 3
    
    for (na.rm in c(TRUE, FALSE)) {
        
        x = ts
        if (na.rm) x@Data[6, ] = NA
        cat("\n\nna.rm: ", na.rm)
        print(x)
        
        for (trim in c(TRUE, FALSE)) {
        
            cat("\ntrim: ", trim, "\n\n")
            
            # Sum:
            ans = rollFun(x, n = n, trim = trim, na.rm = na.rm, FUN = sum)
            print(ans)
            # 15 20 25 30 35 40 45 50 55 60 65 70 75 80 85 90
            
            # Mean:
            ans = rollFun(x, n = n, trim = trim, na.rm = na.rm, FUN = mean)
            #  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18
            print(ans)
            
            # Var:
            ans = rollFun(x, n = n, trim = trim, na.rm = na.rm, FUN = var)
            #  2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5 2.5
            print(ans)
            
            # Min:
            ans = rollFun(x, n = n, trim = trim, na.rm = na.rm, FUN = min)
            #  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
            print(ans)
            
            # Max:
            ans = rollFun(x, n = n, trim = trim, na.rm = na.rm, FUN = max)
            #  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
            print(ans)
            
        }
    }
        
    invisible()
}


################################################################################

