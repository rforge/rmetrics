
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
# METHODS              CREATE A TIMESERIES FROM OTHER OBJECTS:
#  is.timeSeries        S3: Tests for a 'timeSeries' object
#  as.timeSeries        S3: Defines method for a 'timeSeries' object
#  as.timeS*.default    S3: Returns the input
#  as.timeS*.numeric    S3: Transforms a numeric vector into a 'timeSeries'
#  as.timeS*.data.frame S3: Transformas a 'data.frame' into a 'timeSeries'
#  as.timeS*.matrix     S3: Transformas a 'matrix' into a 'timeSeries'
#  as.timeS*.ts         S3: Transforms a 'ts' object into a 'timeSeries'
#  as.timeS*.character  S3: Loads and transformas from a demo file
#  as.timeS*.zoo        S3: Transforms a 'zoo' object into a 'timeSeries'
#
# METHODS              TRANSFORM A TIMESERIES INTO OTHER OBJECTS:
#  as.vector.timeS*     S3: Converts a univariate 'timeSeries' to a vector
#  as.matrix.timeS*     S3: Converts a 'timeSeries' to a 'matrix'
#  as.data.frame.t*     S3: Converts a 'timeSeries' to a 'data.frame'
#  as.ts.timeSeries     S3: Converts a 'timeSeries' to a 'ts'
#      
# NEW METHODS 
#  .as.vector.zoo
#  .as.matrix.zoo
#  .quantile.zoo
#
#  .t.timeSeries
#  .mergeSeries
################################################################################


is.timeSeries = 
function (object) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Tests for a 'timeSeries' object.
    
    # Arguments:
    #   object - a 'timeSeries' object to be tested.
    
    # Value:
    #   Returns 'TRUE' or 'FALSE' depending on whether its
    #   argument is of 'timeSeries' type or not.
        
    # FUNCTION:
    
    # Check:
    ans = inherits(object, "timeSeries")
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


as.timeSeries =
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    UseMethod("as.timeSeries")
}


# ------------------------------------------------------------------------------


as.timeSeries.default =
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    # Return Value:
    x
}


# ------------------------------------------------------------------------------


as.timeSeries.numeric =
function(x, ...)
{   # A function implemented by Diethelm Wuertz


    # Create a dummay daily 'timeSeries' object:
    ans = dummyDailySeries(x)
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


as.timeSeries.data.frame =
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    # Examples:
    #   data(bmwRet); head(as.timeSeries(data(bmwRet)))

    # FUNCTION:
    
    # Check if the first column has a valid ISO-format:
    charvec = as.character(as.vector(x[, 1]))
    isoFormat = c("%Y%m%d", "%Y-%m-%d", "%Y%m%d%H%M", "%Y-%m-%d %H:%M",
        "%Y%m%d%H%M%S", "%Y-%m-%d %H:%M:%S")
    isoCheck = 0
    for (i in 1:4) {
        Test = !is.na(strptime(charvec, isoFormat[i])) 
        if (Test[1]) isoCheck = i
    }
    if (isoCheck == 0) {
        stop("Could not identify format type") 
    } else {
        format = isoFormat[isoCheck]
    }
    
    # Transform to Matrix:
    if (dim(x)[2] == 2) {
        X = matrix(x[, -1], ncol = dim(x)[2] - 1) 
    } else {
        X = x[, -1]
    }
    colNames = colnames(X) = colnames(x)[-1]
    Numeric = NULL
    for (i in 1:length(X[1, ])) {
        if (is.numeric(X[1, i])) Numeric = c(Numeric, i)
    }   
    if (is.null(numeric)) {
        stop("x contains no numeric columns") 
    } else {
        data = as.matrix(X[, Numeric])
        colnames(data) = colNames[Numeric]
        if (length(Numeric) != length(X[1, ])) {
            recordIDs = data.frame(X[, -Numeric])
            colnames(recordIDs) = colnames(X)[-Numeric]
        }
    }
     
    # Create Time Series Object:                          
    ans = timeSeries(data = data, charvec = charvec, 
        units = colnames(data), format = format, zone = "GMT", 
        FinCenter = "GMT", recordsIDs = recordIDs)  
        
    # Return Value:    
    ans
}


# ------------------------------------------------------------------------------


as.timeSeries.matrix =
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    x = as.data.frame(x)
    as.timeSeries(x)
}
   
    
# ------------------------------------------------------------------------------


as.timeSeries.ts =
function(x, ...)
{   # A function implemented by Diethelm Wuertz


    # Create a dummay daily 'timeSeries' object:
    ans = dummyDailySeries(as.vector(x))
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


as.timeSeries.character =
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    # Example:
    #   as.timeSeries(data(nyse))
    
    x = eval(parse(text = eval(x)))
    as.timeSeries(x)
}



# ------------------------------------------------------------------------------


as.timeSeries.zoo =
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    timeSeries(data = as.matrix(x), charvec = attr(x, "index"), 
        units = colnames(x), , format = format, , zone = "GMT",
        FinCenter = "GMT")

}


################################################################################ 


as.vector.timeSeries =
function(x, mode = "any") 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Converts a univariate "timeSeries" to a vector

    # Arguments:
    #   x - a 'timeSeries' object
    
    # Value:
    #   Returns the data slot of 'timesSeries' object as a vector.
        
    # FUNCTION:
        
    # Check:
    if (class(x) != "timeSeries") 
        stop("x is not a timeSeries object!")
    if (dim(as.matrix(x))[[2]] != 1) 
        stop("x is not a univariate timeSeries object!")
        
    # Convert:
    rownames = dimnames(x)[[1]]
    x = x@Data
    class(x) = "numeric"
    x = as.vector(x)
    names(x) = rownames
    
    # Return Value:
    x 
}
    

# ------------------------------------------------------------------------------


as.matrix.timeSeries =
function(x) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Converts a multivariate "timeSeries" to a matrix

    # Arguments:
    #   x - a 'timeSeries' object
    
    # Value:
    #   Returns the data slot of a 'timesSeries' object as a vector.
    
    # FUNCTION:
    
    # Check:
    if (class(x) != "timeSeries") stop("x is not a timeSeries object!")
        
    # Convert:
    ans = as.matrix(x@Data) # is matrix
        
    # Return Value:
    ans 
}
    

# ------------------------------------------------------------------------------


as.data.frame.timeSeries =
function(x, row.names = NULL, optional = NULL) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Converts a multivariate "timeSeries" to a data.frame
    
    # Arguments:
    #   x - a 'timeSeries' object
    #   row.names, optional - not used
    
    # Value:
    #   Returns the data slot of a 'timesSeries' object as a data frame.
    
    # FUNCTION:
    
    # Check:
    if (class(x) != "timeSeries") stop("x is not a timeSeries object!")
        
    # Convert:
    dimNames = dimnames(x@Data)
    ans = as.matrix(x@Data) 
    dimnames(ans) = dimNames
    ans = as.data.frame(ans)
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


as.ts.timeSeries = 
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Converts a colum from a 'timeSeries' object into an object
    #   of class 'ts'.
    
    # Example:
    #   x = as.timeSeries(data(daxRet)); as.ts(x[1:50, ])
    
    # Transform:
    if (isUnivariate(x)) {
        ans = as.ts(as.vector(x@Data[, 1]), ...)
    } else if (isMultivariate(x)) {
        ans = as.ts(x@Data, ...)
    }
    
    # Add Attribute:
    attr(ans, "positions") = seriesPositions(x)
    
    # Return Value:
    ans
}
    

################################################################################
# FOR THE FUTURE:

.as.vector.zoo =
function(x, mode = "any") 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Converts a univariate "zoo" series to a vector

    # Arguments:
    #   x - a 'zoo' object
    
    # Example:
    #   require(tseries); as.vector(get.hist.quote("IBM", quote = "Close"))
    
    # Value:
    #   Returns the data of an 'zoo' object as a named vector.
        
    # FUNCTION:
        
    # Check:
    if (class(x) != "zoo") 
        stop("x is not a timeSeries object!")
    if (dim(x)[[2]] != 1) 
        stop("x is not an univariate zoo object!")
        
    # Convert:
    Names = as.character(attr(x, "index"))
    x = unclass(x)[,1]
    names(x) = Names
    attr(x, "index") = NULL
    
    # Return Value:
    x 
}
    

# ------------------------------------------------------------------------------


.as.matrix.zoo =
function(x) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Converts a multivariate "timeSeries" to a matrix

    # Arguments:
    #   x - a 'timeSeries' object
    
    # Value:
    #   Returns the data of an 'zoo' object as a named matrix.
    
    # Example:
    #   require(tseries); as.matrix(get.hist.quote("IBM"))
    
    # FUNCTION:
    
    # Check:
    if (class(x) != "zoo") 
        stop("x is not a timeSeries object!")
    if (dim(x)[[2]] <= 1) 
        stop("x is not a multivariate zoo object!")
        
    # Convert:
    Names = as.character(attr(x, "index"))
    x = unclass(x)
    rownames(x) = Names
    attr(x, "index") = NULL
        
    # Return Value:
    x 
}


# ------------------------------------------------------------------------------


.quantile.zoo = 
function(x, probs = 0.95, ...)
{   # A function implemented by Diethelm Wuertz

    # Arguments:
    #   x - an object of class 'timeSeries'. The quantiles will be 
    #       computed for the selected column.
    #   probs - a numeric value or numeric vector with probabilities.
    #   column - the selected column    
    
    # Examples:
    #   quantile(as.timeSeries(data(daxRet)))
    
    # FUNCTION:
    
    # Convert to timeSeries:
    ans = quantile(as.timeSeries(x), ...)
       
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.t.timeSeries =
function(x)
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Transpose:
    ans = t(x@Data)
    
    # Return Value:
    ans
}


################################################################################ 
# OLD:


.mergeSeries = 
function(x, y, units = NULL)
{   # A function implemented by Diethelm Wuertz
    
    # IMPORTANT:
    #   This is the old version where 'y' is a matrix with the same 
    #   row dimension as 'x'.
    
    # Description:
    #   Merges a 'timeSeries' with a 'matrix' object 
    
    # Arguments:
    #   x - a 'timeSeries' object
    #   y - a numeric matrix with the same number of rows as x
    
    # Value:
    #   Returns a S4 object of class 'timeSeries'.
    
    # FUNCTION:
    
    # Test Input:
    if (class(x) != "timeSeries") stop("x must be a timeSeries")
    if (!is.matrix(y)) stop("y must be a matrix")
    xRows = dim(x@Data)[1]
    yRows = dim(as.matrix(y))[1]
    if (xRows != yRows) stop("x and y must be of same length")
    
    # Bind Data:
    x@Data = cbind(x@Data, y)
    
    # Add Column Names:
    if (is.null(colnames(y))) 
        colnames(y) <- paste("Y", 1:(dim(y)[2]), sep="")
    colnames(x@Data) <- c(x@units, colnames(y))
    if (!is.null(units)) {
        x@units = units
        colnames(x@Data) <- units }     
    
    # Return Value:
    new("timeSeries", 
        Data = x@Data, 
        positions = x@positions, 
        format = as.character(x@format), 
        FinCenter = as.character(x@FinCenter),
        units = colnames(x@Data), 
        recordIDs = data.frame(),
        title = as.character(x@title), 
        documentation = as.character(x@documentation) )          
}

   
################################################################################

