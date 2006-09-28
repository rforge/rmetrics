
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
# FUNCTION:      DESCRIPTION: 
#  removeNA       Remove NAs from a matrix object
#  substituteNA   Substitute NAs by zero, the column mean or median
#  interpNA       Interpolate NAs using R's "approx" function
#  knnNA          Impute NAs by the "knn"-Algorithm from R's EMV package
#  .knn           Internal function from package EMV
################################################################################


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


# ------------------------------------------------------------------------------


knnNA = 
function(x, k = max(dim(as.matrix(x))[1]*0.01,2), correlation = FALSE, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Estimation of missing values in a matrix by a k-th nearest 
    #   neighboors algorithm.
    
    # Arguments:
    #   x - a numeric matrix that contains the missing values to 
    #       be estimated. 
    #   k - the number of neighboors (rows) to estimate the missing 
    #       values. 
    #   correlation - a logical value, if TRUE the selection of the 
    #       neighboors is based on the sample correlation. The 
    #       neighboors with the highest correlations are selected. 
    #   ... - optional arguments passed to the "knn" function.

    # Notes:
    #   require(EMV)
    #   Version: 1.2 
    #   Author: Raphael Gottardo 
    #   Maintainer: Raphael Gottardo <raph@stat.washington.edu> 
    #   License: GPL version 2 or later 
    
    # FUNCTION:

    # Settings:
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
    
    # KNN:
    ans = .knn(m = x, k = k, correlation = correlation, ...)$data
    
    # timeSeries
    if (TS) {
        ans = timeSeries(data = ans, charvec = positions, units = units, 
            zone = FinCenter, FinCenter = FinCenter)
    }
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.knn = 
function(m, k = max(dim(m)[1]*0.01,2), na.rm = TRUE, nan.rm = TRUE,
inf.rm = TRUE, correlation = FALSE, dist.bound = FALSE)
{   # A copy from EMV package

    # Notes:
    #   Package: EMV
    #   Title: Estimation of Missing Values for a Data Matrix
    #   Version: 1.3.1
    #   Author: Raphael Gottardo
    #   Description: Estimation of missing values in a matrix by 
    #       a k-th nearest neighboors algorithm
    #   Maintainer: Raphael Gottardo <raph@stat.washington.edu>
    #   License: GPL version 2 or later
    #   Packaged: Thu Oct 7 15:29:04 2004

    # FUNCTION:
    
    # Check:
    if (is.matrix(m) == FALSE)
        stop(message = "not a valid matrix object")
    
    # Dimension:
    n = dim(m)[1]

    # At least 2 to compute the mean 
    if (k < 2)
        stop(message="k should be bigger than 1")

    # At most n-1 neighboors
    if (k > n)
        k <- n-1
    
    nb.col = dim(m)[2]
    nb.row = dim(m)[1]

    # Code when linking to C
    missing.code = -9999999
  
    vector = as.double(t(m))
    tmp = vector
  
    # Replace the missing values by -9999999 (C code) 
    vector[is.finite(vector) == FALSE] = missing.code
  
    # Compute:
    result = .C("knnc",
        vector = as.double(vector),
        nb.col = as.integer(nb.col),
        nb.row = as.integer(nb.row),
        k = as.integer(k),
        as.integer(correlation),
        distance = double(nb.row),
        as.double(dist.bound),
        PACKAGE = "fMultivar")

    vector = result$vector
    
    # Still missing values if complete row of missing values
    vector[vector == missing.code] = tmp[vector == missing.code]
  
    # Remove the non-missing rows for the distances
    distance = result$distance[result$distance != missing.code & 
        result$distance != -missing.code]
    row = (1:nb.row)[result$distance != missing.code & 
        result$distance != -missing.code]
    distance = cbind(row,distance)
  
    if (na.rm == FALSE) {
        vector[is.na(tmp) == TRUE & is.nan(tmp) == FALSE] = NA
    }
  
    if (inf.rm == FALSE) {
        index = is.finite(tmp) == FALSE & is.na(tmp) == FALSE & 
            is.nan(tmp) == FALSE
        vector[index] = tmp[index]
    }
  
    if (nan.rm == FALSE)
        vector[is.nan(tmp) == TRUE] = NaN
  
    # Coerce vector back into the matrix
    newdata = matrix(vector , nrow = nb.row, ncol = nb.col, byrow = TRUE)
    
    # Return Value:
    list(data = newdata, distance = distance)
}


################################################################################

