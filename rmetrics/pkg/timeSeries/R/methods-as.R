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
# METHOD:                   CREATE A TIMESERIES FROM OTHER OBJECTS:
#  as.timeSeries             Defines method for a 'timeSeries' object
#  as.timeSeries.default     Returns the input
#  as.timeSeries.data.frame  Transformas a 'data.frame' into a 'timeSeries'
#  as.timeSeries.character   Loads and transformas from a demo file
#  as.timeSeries.zoo         Transforms a 'zoo' object into a 'timeSeries'
# METHOD:                   TRANSFORM A TIMESERIES INTO OTHER OBJECTS:
#  as.vector.timeSeries      Converts a univariate 'timeSeries' to a vector
#  as.matrix.timeSeries      Converts a 'timeSeries' to a 'matrix'
#  as.numeric.timeSeries     Converts a 'timeSeries' to a 'numeric'
#  as.data.frame.timeSeries  Converts a 'timeSeries' to a 'data.frame'
#  as.ts.timeSeries          Converts a 'timeSeries' to a 'ts'
#  as.ts.logical             Converts a 'timeSeries' to 'logical'
################################################################################


################################################################################
# METHODS:                  CREATE A TIMESERIES FROM OTHER OBJECTS:
#  as.timeSeries             Defines method for a 'timeSeries' object
#  as.timeSerie.default      Returns the input
#  as.timeSeries.numeric     Transforms a numeric vector into a 'timeSeries'
#  as.timeSeries.data.frame  Transformas a 'data.frame' into a 'timeSeries'
#  as.timeSeries.matrix      Transformas a 'matrix' into a 'timeSeries'
#  as.timeSeries.ts          Transforms a 'ts' object into a 'timeSeries'
#  as.timeSeries.character   Loads and transformas from a demo file
#  as.timeSeries.zoo         Transforms a 'zoo' object into a 'timeSeries'

as.timeSeries <-
    function(x, ...)
{   # A function implemented by Diethelm Wuertz
    # Extended by Yohan Chalabi

    UseMethod("as.timeSeries")
}

# ------------------------------------------------------------------------------

as.timeSeries.default <-
    function(x, ...)
{   # A function implemented by Diethelm Wuertz
    # Extended by Yohan Chalabi

    # FUNCTION:

    # Return Value:
    ans <- timeSeries(x, ...)

    ans
}

setAs("ANY", "timeSeries", function(from) as.timeSeries.default(from))

# ------------------------------------------------------------------------------

as.timeSeries.data.frame <-
    function(x, ...)
{   # A function implemented by Diethelm Wuertz
    # Extended by Yohan Chalabi

    # Description:
    #   Converts a data.frame into a timeSeries object

    # Notes:
    #   The first column must contain the dates.

    # Examples:
    #   data(bmwRet); head(as.timeSeries(data(bmwRet)))

    # FUNCTION:

    # Check if rownames(x) or the first column has a valid ISO-format:
    if (is.numeric(x[,1])) {
        # is.numeric() is better than format == "unkown"
        # which can give wrong result. i.e. whichFormat(0.1253328600)
        charvec <- rownames(x)
        format = whichFormat(charvec, silent = TRUE)
        if (format == "unknown") format = "counts"
        X <- x
        colNames = colnames(x)
    } else {
        charvec = as.character(as.vector(x[, 1]))
        format = whichFormat(charvec, silent = TRUE)
        if (format == "unkown")
            stop("can not read the first column as a valid ISO-format date")
        X <- data.frame(x[,-1])
        colNames <- colnames(x)[-1]
    }

    Numeric = NULL
    for (i in seq_len(NCOL(X))) {
        if (is.numeric(X[, i])) Numeric = c(Numeric, i)
    }
    if (is.null(Numeric)) {
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

    units <- colnames(data)

    # Create Time Series Object:
    ans <- timeSeries(data = data, charvec = charvec, format = format,
                      recordIDs = recordIDs, ...)

    # Return Value:
    ans
}

setAs("data.frame", "timeSeries", function(from) as.timeSeries.data.frame(from))

# ------------------------------------------------------------------------------


as.timeSeries.character <-
function(x, ...)
{   # A function implemented by Diethelm Wuertz
    # Extended by Yohan Chalabi

    # Example:
    #   as.timeSeries(data(nyse))

    # FUNCTION:

    # Load Demo File - Returns a data frame:
    x <- eval(parse(text = eval(x)))

    # timeSeries:
    ans <- as.timeSeries(x, ...)

    # Return Value:
    ans
}

setAs("character", "timeSeries", function(from) as.timeSeries.character(from))

# ------------------------------------------------------------------------------


as.timeSeries.zoo <-
    function(x, ...)
{   # A function implemented by Diethelm Wuertz
    # Extended by Yohan Chalabi

    # FUNCTION:

    # as. timeSeries:

    ans <- timeSeries(data = as.matrix(x),
                      charvec = as.character(attr(x, "index")), ...)

    # Return Value:
    ans

}


################################################################################
# METHODS:                  TRANSFORM A TIMESERIES INTO OTHER OBJECTS:
#  as.matrix.timeSeries      Converts a 'timeSeries' to a 'matrix'
#  as.data.frame.timeSeries  Converts a 'timeSeries' to a 'data.frame'
#  as.ts.timeSeries          Converts a 'timeSeries' to a 'ts'
#  as.ts.logical             Converts a 'timeSeries' to 'logical'

as.matrix.timeSeries <-
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
    if (class(x) != "timeSeries")
        stop("x is not a timeSeries object!")

    # Convert:
    ans <- x@.Data # is matrix
    dimnames(ans) <- list(x@positions, x@units)

    # Results
    ans
}


# ------------------------------------------------------------------------------

as.data.frame.timeSeries <-
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
    if (class(x) != "timeSeries")
        stop("x is not a timeSeries object!")

    # Convert:
    ans = as.data.frame(as.matrix(x))

    # Return Value:
    ans
}

# ------------------------------------------------------------------------------

as.ts.timeSeries <-
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
        ans = as.ts(as.vector(x), ...)
    } else if (isMultivariate(x)) {
        ans = as.ts(as.matrix(x), ...)
    }

    # Add Attribute:
    attr(ans, "positions") = time(x)

    # Return Value:
    ans
}

# ------------------------------------------------------------------------------

## as.logical.timeSeries <- function(x, ...) as.logical(series(x), ...)

################################################################################

