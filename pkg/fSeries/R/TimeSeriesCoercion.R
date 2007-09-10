
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
# METHOD:                   CREATE A TIMESERIES FROM OTHER OBJECTS:
#  is.timeSeries             Tests for a 'timeSeries' object
#  as.timeSeries             Defines method for a 'timeSeries' object
#  as.timeSeries.default     Returns the input
#  as.timeSeries.numeric     Transforms a numeric vector into a 'timeSeries'
#  as.timeSeries.data.frame  Transformas a 'data.frame' into a 'timeSeries'
#  as.timeSeries.matrix      Transformas a 'matrix' into a 'timeSeries'
#  as.timeSeries.ts          Transforms a 'ts' object into a 'timeSeries'
#  as.timeSeries.character   Loads and transformas from a demo file
#  as.timeSeries.zoo         Transforms a 'zoo' object into a 'timeSeries'
# METHOD:                   TRANSFORM A TIMESERIES INTO OTHER OBJECTS:
#  as.vector.timeSeries      Converts a univariate 'timeSeries' to a vector
#  as.matrix.timeSeries      Converts a 'timeSeries' to a 'matrix'
#  as.data.frame.timeSeries  Converts a 'timeSeries' to a 'data.frame'
#  as.ts.timeSeries          Converts a 'timeSeries' to a 'ts'     
# METHOD:                   HANDLING ZOO OBJECTS:
#  .as.vector.zoo            Converts a 'zoo' into a 'vector' object
#  .as.matrix.zoo            Converts a 'zoo' into a 'matrix' object
#  .quantile.zoo             Computes quantiles from a 'zoo' object
################################################################################


################################################################################
# METHODS:                  CREATE A TIMESERIES FROM OTHER OBJECTS:
#  is.timeSeries             Tests for a 'timeSeries' object
#  as.timeSeries             Defines method for a 'timeSeries' object
#  as.timeSerie.default      Returns the input
#  as.timeSeries.numeric     Transforms a numeric vector into a 'timeSeries'
#  as.timeSeries.data.frame  Transformas a 'data.frame' into a 'timeSeries'
#  as.timeSeries.matrix      Transformas a 'matrix' into a 'timeSeries'
#  as.timeSeries.ts          Transforms a 'ts' object into a 'timeSeries'
#  as.timeSeries.character   Loads and transformas from a demo file
#  as.timeSeries.zoo         Transforms a 'zoo' object into a 'timeSeries'


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
        
    # Changes:
    #
    
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

    # FUNCTION:
    
    # Return Value:
    x
}


# ------------------------------------------------------------------------------


as.timeSeries.numeric =
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Create a dummay daily 'timeSeries' object:
    if (is.null(dim(x))) x = matrix(x)
    ans = dummyDailySeries(x, ...)
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


as.timeSeries.data.frame =
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Converts a data.frame into a timeSeries object
    
    # Notes:
    #   The first column must contain the dates.
    
    # Examples:
    #   data(bmwRet); head(as.timeSeries(data(bmwRet)))
 
    # FUNCTION:
    
    # Check if the first column has a valid ISO-format:
    dummyDates = FALSE
    firstColumn = TRUE
    charvec = as.character(as.vector(x[, 1]))
    format = .whichFormat(charvec, ...)
    if (format == "unknown") {
        charvec = rownames(x)
        format = .whichFormat(charvec, ...)
        if (format == "unknown") {
            # warning("Could not identify timeDate Format")
            dummyDates = TRUE
            N = length(as.vector(x[, 1]))
            charvec = as.character(timeSequence(from = "1970-01-01", 
                length.out = N, format = "%Y-%m-%d", zone = "GMT", 
                FinCenter = "GMT"))
            format = .whichFormat(charvec, ...)
        }
        firstColumn = FALSE
    }
    
    # Transform to Matrix:
    if (firstColumn) {
        X = cbind(x[, -1])
    } else {
        X = x
    }
    colNames = colnames(X) 
    # rownames(X) = charvec
    
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
        } else {
            recordIDs = data.frame()
        }
    }
     
    # Create Time Series Object:                          
    ans = timeSeries(data = data, charvec = charvec, 
        units = colnames(data), format = format, zone = "GMT", 
        FinCenter = "GMT", recordIDs = recordIDs) 
    if (dummyDates) attr(ans, "control")<-"Dummy Dates Used" 
        
    # Return Value:    
    ans
}


# ------------------------------------------------------------------------------


as.timeSeries.matrix =
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # As timeSeries:
    x = as.data.frame(x)
    ans = as.timeSeries(x, ...)
    
    # Return Value:
    ans
}
   
    
# ------------------------------------------------------------------------------


as.timeSeries.ts =
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
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
 
    # FUNCTION:
    
    # Load Demo File - Returns a data frame:
    x = eval(parse(text = eval(x)))
    
    # timeSeries:
    ans = as.timeSeries(x)
    
    # Return Value:
    ans
}



# ------------------------------------------------------------------------------


as.timeSeries.zoo =
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # as. timeSeries:
    ans = timeSeries(data = as.matrix(x), charvec = attr(x, "index"), 
        units = colnames(x), , format = format, , zone = "GMT",
        FinCenter = "GMT")
        
    # Return Value:
    ans

}


################################################################################ 
# METHODS:                  TRANSFORM A TIMESERIES INTO OTHER OBJECTS:
#  as.vector.timeSeries      Converts a univariate 'timeSeries' to a vector
#  as.matrix.timeSeries      Converts a 'timeSeries' to a 'matrix'
#  as.data.frame.timeSeries  Converts a 'timeSeries' to a 'data.frame'
#  as.ts.timeSeries          Converts a 'timeSeries' to a 'ts'     


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
    stopifnot(isUnivariate(x))
        
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
function(x, ...) 
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
function(x, row.names = NULL, optional = NULL, ...) 
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
    
    # Changes:
    #
    
    # FUNCTION:
    
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
# METHODS:              HANDLING ZOO OBJECTS:
#  .as.vector.zoo            Converts a 'zoo' into a 'vector' object
#  .as.matrix.zoo           Converts a 'zoo' into a 'matrix' object
#  .quantile.zoo            Computes quantiles from a 'zoo' object


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
    #   Converts a 'zoo' into a 'matrix' object

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

    # Description:
    #   Computes quantiles from a 'zoo' object
    
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

   
################################################################################

