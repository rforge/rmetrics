
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
# FUNCTION:                 COLUMN/ROW CUMULATIVE STATISTICS:
#  rowCumsums                Computes sample cumulated sums by row
#  rowCumsums.default        S3 default method (for matrix objects)
#  rowCumsums.timeSeries     S3 method for timeSeries objects
#  rowCumsums.zoo            S3 method for zoo objects
#  colCumsums                Computes sample cumulated sums by column
#  colCumsums.default        S3 default method (for matrix objects)
#  colCumsums.timeSeries     S3 method for timeSeries objects
#  colCumsums.zoo            S3 method for zoo objects
# FUNCTION:                 ADDONS:
#  colCummaxs                Computes cumulated maximum values
#  colCummaxs.default        S3 default method (for matrix objects)
#  colCummaxs.timeSeries     S3 method for timeSeries objects
#  colCummaxs.zoo            S3 method for zoo objects
#  colCumprods               Computes cumulated product values
#  colCumprods.default       S3 default method (for matrix objects)
#  colCumprods.timeSderies   S3 method for timeSeries objects
#  colCumprods.zoo           S3 method for zoo objects
# FUNCTION:                 NO LONGER SUPPORTED:
#  cumsum.timeSeries         use colCumsums()
################################################################################


################################################################################
# @comments
#   DW 2007-09-20           further col* functions added, see ADDONS
################################################################################


################################################################################
# @todo
#
################################################################################


################################################################################
#  rowCumsums                Computes sample cumulated sums by row
#  rowCumsums.default        S3 default method (for matrix objects)
#  rowCumsums.timeSeries     S3 method for timeSeries objects
#  rowCumsums.zoo            S3 method for zoo objects
#  colCumsums                Computes sample cumulated sums by row
#  colCumsums.default        S3 default method (for matrix objects)require
#  colCumsums.timeSeries     S3 method for timeSeries objects
#  colCumsums.zoo            S3 method for zoo objects


rowCumsums =
function(x, na.rm = FALSE, ...)
{
    UseMethod("rowCumsums") 
}


# ------------------------------------------------------------------------------


rowCumsums.default =
function(x, na.rm = FALSE, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes sample cumulated sums by row (for matrix objects)

    # FUNCTION:

    # Transform:
    X = as.matrix(x, ...)

    # Statistics:
    if (na.rm) {
        result = apply(na.omit(X), MARGIN = 2, FUN = cumsum, ...)
    } else {
        result = apply(X, MARGIN = 2, FUN = cumsum, ...)
    }
    colnames(result) = paste(1:ncol(x))

    # Statistics:
    result <- apply(if(na.rm) na.omit(X) else X, 2, cumsum, ...)

    # Return Value:
    result
}


# ------------------------------------------------------------------------------


rowCumsums.timeSeries =
function(x, na.rm = FALSE, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes sample cumulated sums by row for timeSeries objects

    # FUNCTION:

    # Cumulative Sums:
    x = rowCumsums(as.matrix(x, ...))

    # Time Series Input ?
    if (class(x) == "timeSeries") {
        x@Data = result
        result = x
    }

    # Return Value:
    result
}


# ------------------------------------------------------------------------------


rowCumsums.zoo =
function(x, na.rm = FALSE, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes sample cumulated sums by row for zoo objects

    # FUNCTION:

    # Cumulative Sums:
    x = rowCumsums(as.matrix(x, ...))
 
    # Zoo Input ?
    if (class(x) == "zoo") {
        index = attr(x, "index")
        frequency = attr(x, "frequency")
        result = zoo(result, index, frequency)
    }

    # Return Value:
    result
}


################################################################################


colCumsums =
function(x, na.rm = FALSE, ...)
{
    UseMethod("rowCumsums") 
}


# ------------------------------------------------------------------------------


colCumsums.default =
function(x, na.rm = FALSE, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes sample cumulated sums by column (for matrix objects)

    # FUNCTION:

    # Transform:
    X = as.matrix(x, ...)

    # Statistics:
    if (na.rm) {
        result = apply(na.omit(X), MARGIN = 2, FUN = cumsum, ...)
    } else {
        result = apply(X, MARGIN = 2, FUN = cumsum, ...)
    }
    colnames(result) = paste(1:ncol(x))

    # Statistics:
    result <- apply(if(na.rm) na.omit(X) else X, 2, cumsum, ...)

    # Return Value:
    result
}


# ------------------------------------------------------------------------------


colCumsums.timeSeries =
function(x, na.rm = FALSE, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes sample cumulated sums by column for timeSeries objects

    # FUNCTION:

    # Cumulative Sums:
    x = colCumsums(as.matrix(x, ...))

    # Time Series Input ?
    if (class(x) == "timeSeries") {
        x@Data = result
        result = x
    }

    # Return Value:
    result
}


# ------------------------------------------------------------------------------


colCumsums.zoo =
function(x, na.rm = FALSE, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes sample cumulated sums by column for zoo objects

    # FUNCTION:

    # Cumulative Sums:
    x = rowCumsums(as.matrix(x, ...))
 
    # Zoo Input ?
    if (class(x) == "zoo") {
        index = attr(x, "index")
        frequency = attr(x, "frequency")
        result = zoo(result, index, frequency)
    }

    # Return Value:
    result
}


################################################################################
#  colCummaxs                Computes cumulated maximum values
#  colCummaxs.default        S3 default method (for matrix objects)
#  colCummaxs.timeSeries     S3 method for timeSeries objects
#  colCummaxs.zoo            S3 method for zoo objects
#  colCumprods               Computes cumulated product values
#  colCumprods.default       S3 default method (for matrix objects)
#  colCumprods.timeSderies   S3 method for timeSeries objects
#  colCumprods.zoo           S3 method for zoo objects


.conflicts.OK = TRUE


# ------------------------------------------------------------------------------
# DW: moved from BasicExtensions ...


colCummaxs =
function(x, na.rm = FALSE, ...)
{   
    UseMethod("colCummaxs")
}

    
# ------------------------------------------------------------------------------


colCummaxs.default =
function(x, na.rm = FALSE, ...)
{   
    # Cumulated Maxima:
    ans = apply(as.matrix(x), 2, cummax, ...) 
    colnames(ans) = colnames(x)
    
    # Return Value: 
    ans
}

    
# ------------------------------------------------------------------------------


colCummaxs.timeSeries =
function(x, na.rm = FALSE, ...)
{   
    # Cumulated Maxima:
    ans = colCummaxs(as.matrix(x, ...), ...)
    
    # Time Series Input ?
    if (class(x) == "timeSeries") {
        x@Data = ans
        ans = x
    }
    
    
    # Return Value: 
    ans
}

    
# ------------------------------------------------------------------------------


colCummaxs.zoo =
function(x, na.rm = FALSE, ...)
{   
    # Cumulated Maxima:
    ans = colCummaxs(as.matrix(x, ...), na.rm, ...)
    
    # Zoo Input ?
    if (class(x) == "zoo") {
        index = attr(x, "index")
        frequency = attr(x, "frequency")
        ans = zoo(ans, index, frequency)
    }

    # Return Value: 
    ans
}


################################################################################

  
colCumprods =
function(x, na.rm = FALSE, ...)
{   
    UseMethod("colCummaxs")
}

    
# ------------------------------------------------------------------------------


colCumprods.default =
function(x, na.rm = FALSE, ...)
{   
    # Cumulated Maxima:
    ans = apply(as.matrix(x, ...), 2, cumprod, ...) 
    colnames(ans) = colnames(x)
    
    # Return Value: 
    ans
}

    
# ------------------------------------------------------------------------------


colCumprods.timeSeries =
function(x, na.rm = FALSE, ...)
{   
    # Cumulated Maxima:
    ans = colCumprods(as.matrix(x, ...), na.rm, ...)
    
    # Time Series Input ?
    if (class(x) == "timeSeries") {
        x@Data = ans
        ans = x
    }
        
    # Return Value: 
    ans
}

    
# ------------------------------------------------------------------------------


colCumprods.zoo =
function(x, na.rm = FALSE, ...)
{   
    # Cumulated Maxima:
    ans = colCummaxs(as.matrix(x, ...), ...)
    
    # Zoo Input ?
    if (class(x) == "zoo") {
        index = attr(x, "index")
        frequency = attr(x, "frequency")
        ans = zoo(ans, index, frequency)
    }

    # Return Value: 
    ans
}  


################################################################################


# NO LONGER IN USE


cumsum.timeSeries = 
function(x) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   No longer in use, use colCumsums()

    # FUNCTION:
    
    # Cumulate:
    x@Data = colCumsums(x@Data)
    
    # Return Value:
    x    
}


################################################################################

