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


is.timeSeries <-
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
    ans <- inherits(object, "timeSeries")

    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


as.timeSeries <-
    function(x, zone = myFinCenter, FinCenter = myFinCenter, ...)
{   # A function implemented by Diethelm Wuertz
    # Extended by Yohan Chalabi

    UseMethod("as.timeSeries")
}


# ------------------------------------------------------------------------------


as.timeSeries.default <-
    function(x, zone = myFinCenter, FinCenter = myFinCenter, ...)
{   # A function implemented by Diethelm Wuertz
    # Extended by Yohan Chalabi

    # FUNCTION:

    # Return Value:
    x
}



# ------------------------------------------------------------------------------

as.timeSeries.timeSeries <-
    function(x, zone = x@FinCenter, FinCenter = myFinCenter, ...)
{   # A function implemented by Diethelm Wuertz
    # Extended by Yohan Chalabi

    # FUNCTION:

    .Deprecated("finCenter<-", "fSeries")

    stopifnot(class(x) == "timeSeries")
    if (zone != x@FinCenter)
        warning("argument zone is ignored and FinCenter\n of series is used as zone")

    # convert to user financial centre
    positions <- timeDate(charvec = time(x), zone = x@FinCenter,
                          FinCenter = FinCenter)

    time(x) <- positions

    # Return Value:
    x
}



# ------------------------------------------------------------------------------


as.timeSeries.numeric =
    function(x, zone = myFinCenter, FinCenter = myFinCenter, ...)
{   # A function implemented by Diethelm Wuertz
    # Extended by Yohan Chalabi

    # FUNCTION:

    # Create a dummay daily 'timeSeries' object:
    if (is.null(dim(x))) x <- matrix(x)
    ans <- dummyDailySeries(x, zone = zone, FinCenter = FinCenter, ...)

    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


as.timeSeries.data.frame <-
    function(x, zone = myFinCenter, FinCenter = myFinCenter, ...)
{   # A function implemented by Diethelm Wuertz
    # Extended by Yohan Chalabi

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
    format = whichFormat(charvec, silent = TRUE)
    if (is.numeric(x[,1])) {
        # is.nuermic() is better than format == "unkown"
        # which can give wrong result. i.e. whichFormat(0.1253328600)
        charvec = rownames(x)
        format = whichFormat(charvec, silent = TRUE)
        if (format == "unknown") {
            # warning("Could not identify timeDate Format")
            dummyDates = TRUE
            N = length(as.vector(x[, 1]))
            charvec = as.character(timeSequence(from = "1970-01-01",
                length.out = N, format = "%Y-%m-%d", zone = zone,
                FinCenter = FinCenter))
            format = whichFormat(charvec, silent = TRUE)
            warning("Dummy Dates Used in as.timeSeries()", call. = FALSE)

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
        units = colnames(data), format = format, zone = zone,
        FinCenter = FinCenter, recordIDs = recordIDs)

    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


as.timeSeries.matrix <-
    function(x, zone = myFinCenter, FinCenter = myFinCenter,...)
{   # A function implemented by Diethelm Wuertz
    # Extended by Yohan Chalabi

    # FUNCTION:

###     if (attr(x, "oclass") == "timeSeries") {
###         data <- matrix(as.numeric(x), ncol = ncol(x))
###         charvec <- rownames(x)
###         units <- attr(x, "units")
###         format <- attr(x, "format")
###         zone <- FinCenter <- attr(x, "FinCenter")
###         recordIDs <- attr(x, "recordIDs")
###         title <- attr(x, "title")
###         documentation <- attr(x, "documentation")

###         ans <- timeSeries(data = data, charvec = charvec, units = units,
###                           format = format, zone = zone, FinCenter =FinCenter,
###                           # recordIDs = recordIDs,
###                           title = title,
###                           documenation = documentation)
###     } else {

        # As timeSeries:
        x <- as.data.frame(x)
        ans <- as.timeSeries(x, zone = zone, FinCenter = FinCenter, ...)
###    }

    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


as.timeSeries.ts <-
    function(x, zone = myFinCenter, FinCenter = myFinCenter,...)
{   # A function implemented by Diethelm Wuertz
    # Extended by Yohan Chalabi

    # FUNCTION:

    # Create a dummay daily 'timeSeries' object:
    ans <- dummyDailySeries(as.matrix(x),
                            zone = zone, FinCenter = FinCenter,
                            unit = colnames(x), ...)

    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


as.timeSeries.character <-
function(x, zone = myFinCenter, FinCenter = myFinCenter, ...)
{   # A function implemented by Diethelm Wuertz
    # Extended by Yohan Chalabi

    # Example:
    #   as.timeSeries(data(nyse))

    # FUNCTION:

    # Load Demo File - Returns a data frame:
    x <- eval(parse(text = eval(x)))

    # timeSeries:
    ans <- as.timeSeries(x, zone = zone, FinCenter = FinCenter, ...)

    # Return Value:
    ans
}



# ------------------------------------------------------------------------------


as.timeSeries.zoo <-
    function(x, zone = myFinCenter, FinCenter = myFinCenter,...)
{   # A function implemented by Diethelm Wuertz
    # Extended by Yohan Chalabi

    # FUNCTION:

    # as. timeSeries:

    # Seperate x in numeric and non-numeric parts
    data <- as.matrix(x)
    recordIDs <- data.frame()
    charvec <- attr(x, "index") # ! check if this is POSIX or Date object

    ans <- timeSeries(data = data, charvec = charvec,
                      units = colnames(x), recordIDs = recordIDs,
                      zone = zone, FinCenter = FinCenter, ...)

    # Return Value:
    ans

}


################################################################################
# METHODS:                  TRANSFORM A TIMESERIES INTO OTHER OBJECTS:
#  as.vector.timeSeries      Converts a univariate 'timeSeries' to a vector
#  as.matrix.timeSeries      Converts a 'timeSeries' to a 'matrix'
#  as.data.frame.timeSeries  Converts a 'timeSeries' to a 'data.frame'
#  as.ts.timeSeries          Converts a 'timeSeries' to a 'ts'


as.vector.timeSeries <-
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
    if (class(x) != "timeSeries") stop("x is not a timeSeries object!")

    # Convert:
    ans = as.matrix(x@Data) # is matrix

###     attr(ans, "oclass") <- "timeSeries"
### #    attr(ans, "positions") <- as.character(x@positions)
###     attr(ans, "format") <- x@format
###     attr(ans, "FinCenter") <- x@FinCenter
###     attr(ans, "units") <- x@units
###     attr(ans, "recordIDs") <- x@recordIDs
###     attr(ans, "title") <- x@title
###     attr(ans, "documentation") <- x@documentation

    # Return Value:
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
        ans = as.ts(as.vector(x@Data[, 1]), ...)
    } else if (isMultivariate(x)) {
        ans = as.ts(x@Data, ...)
    }

    # Add Attribute:
    attr(ans, "positions") = time(x)

    # Return Value:
    ans
}


################################################################################

