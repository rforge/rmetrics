
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
# FUNCTION:      DESCRIPTION: 
#  removeNA       Remove NAs from a matrix object
#  substituteNA   Substitute NAs by zero, the column mean or median
#  interpNA       Interpolate NAs using R's "approx" function
################################################################################


################################################################################
# FUNCTION:      DESCRIPTION: 
#  removeNA       Remove NAs from a matrix object
#  substituteNA   Substitute NAs by zero, the column mean or median
#  interpNA       Interpolate NAs using R's "approx" function


removeNA = 
function (x, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Remove NA's from objects which can be transformed to a matrix
    
    # Arguments:
    #   x - an object which can be transformed to a matrix
    
    # FUNCTION:
    
    # Convert to Matrix:
    if (class(x) == "timeSeries") {
        TS = TRUE
        positions = x@positions
        FinCenter = x@FinCenter
        units = x@units
        x = x@Data
    } else {
        TS = FALSE
        x = as.matrix(x, ...)
    }
    
    # Remove:
    nas.row = apply(is.na(x), 1, any)
    x.row = x[!nas.row, , drop = FALSE]
    nas.col = apply(is.na(x.row), 2, any)
    ans = x.row[, !nas.col, drop = FALSE]
    
    # timeSeries:
    if (TS) {
        ans = timeSeries(data = ans, charvec = rownames(ans), 
            units = units, zone = FinCenter, FinCenter = FinCenter)
    }
       
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


substituteNA =
function(x, type = c("zeros", "mean", "median"), ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Imputes missing data by zeros, the median or the
    #   mean values of all matrix elements
    
    # Arguments:
    #   x - an object which can be transformed to a matrix
    #   type - method specifies the substitution method to be
    #       used. Choices are "zeros", "mean", or "constant"
        
    # FUNCTION:

    # Convert to Matrix:
    if (class(x) == "timeSeries") {
        TS = TRUE
        positions = x@positions
        FinCenter = x@FinCenter
        units = x@units
        ans = x@Data
    } else {
        TS = FALSE
        ans = as.matrix(x, ...)
    }
    
    # Type:
    type = type[1]   
    if (type == "zeros" | type == "z") {
        ans = apply(ans, 2,
            function(z) {z[is.na(z)] = 0; z}) 
    } 
    if (type == "median") {
        ans = apply(ans, 2,
            function(z) {z[is.na(z)] = median(z, na.rm = TRUE); z}) 
    }
    if (type == "mean") {
        ans = apply(ans, 2,
            function(z) {z[is.na(z)] = mean(z, na.rm = TRUE); z}) 
    }
    
    # timeSeries:
    if (TS) {
        ans = timeSeries(data = ans, charvec = positions, units = units, 
            zone = FinCenter, FinCenter = FinCenter)
    }
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


interpNA =
function(x, method = c("linear", "before", "after"), ...)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Interpolates missing values in a matrix object   
    
    # Arguments:
    #   x - a numeric vector or time series object of class 'ts'.
    #   method - the method how to interpolate the vector, one of
    #       the applied vector strings: "linear", "before" or 
    #       after.
    
    # Details:
    #   To interpolate the function 'approx' is used.
    
    # Value:
    #   Returns a vector or time series object where the missing
    #   values are interpolated.
        
    # FUNCTION:
    
    # Convert to Matrix:
    if (class(x) == "timeSeries") {
        TS = TRUE
        positions = x@positions
        FinCenter = x@FinCenter
        units = x@units
        x = x@Data
    } else {
        TS = FALSE
        x = as.matrix(x, ...)
    }
    
    # Internal Function:    
    interpVectorNA = function(x, method, f) {
        n = length(x)
        idx = (1:n)[!is.na(x)]
        x = approx(idx, x[idx], 1:n, method = method, f = f)$y
        x  }
    
    # Select Method:
    method = method[1]; 
    f = 0
    if (method == "before") {
        method = "constant"
        f = 0
    }
    if (method == "after") {
        method = "constant"
        f = 1
    }
    
    # For each Column:
    for (i in 1:ncol(x)) {
        x[, i] = interpVectorNA(x[, i], method, f) 
    }
        
    # timeSeries:
    if (TS) {
        x = timeSeries(data = x, charvec = positions, units = units, 
            zone = FinCenter, FinCenter = FinCenter)
    }
    
    # Return Value:
    x
}


################################################################################

