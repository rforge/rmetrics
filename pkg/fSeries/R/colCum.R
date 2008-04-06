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
# FUNCTION:                 COLUMN CUMULATIVE SUMS:
#  colCumsums                Computes sample cumulated sums by column
#  colCumsums.default        S3 default method (for matrix objects)
#  colCumsums.timeSeries     S3 method for timeSeries objects
# FUNCTION:                 COLUMN CUMULATIVE MAXIMA:
#  colCummaxs                Computes cumulated maximum values
#  colCummaxs.default        S3 default method (for matrix objects)
#  colCummaxs.timeSeries     S3 method for timeSeries objects
# FUNCTION:                 COLUMN CUMULATIVE MAXIMA:
#  colCummins                Computes cumulated maximum values
#  colCummins.default        S3 default method (for matrix objects)
#  colCummins.timeSeries     S3 method for timeSeries objects
# FUNCTION:                 COLUMN CUMULATIVE MINIMA:
#  colCumprods               Computes cumulated product values
#  colCumprods.default       S3 default method (for matrix objects)
#  colCumprods.timeSeries    S3 method for timeSeries objects
# FUNCTION:                 COLUMN CUMULATIVE RETURNS:
#  colCumreturns             Computes cumulated product values
#  colCumreturns.default     S3 default method (for matrix objects)
#  colCumreturns.timeSeries  S3 method for timeSeries objects
################################################################################


.conflicts.OK = TRUE


# ------------------------------------------------------------------------------


colCumsums <-
    function(x, na.rm = FALSE, ...)
{
    UseMethod("colCumsums")
}


# ------------------------------------------------------------------------------


colCumsums.default <-
    function(x, na.rm = FALSE, ...)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes sample cumulated sums by column (for matrix objects)

    # Arguments:

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


colCumsums.timeSeries <-
    function(x, na.rm = FALSE, ...)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Computes sample cumulated sums by column for timeSeries objects

    # Arguments:

    # FUNCTION:

    # Cumulative Sums:
    X = colCumsums(as.matrix(x, ...))

    # Time Series Input ?
    if (class(x) == "timeSeries") {
        x@Data = X
    }

    # Return Value:
    x
}


# ------------------------------------------------------------------------------
# DW: moved from BasicExtensions ...


colCummaxs <-
    function(x, na.rm = FALSE, ...)
{
    UseMethod("colCummaxs")
}


# ------------------------------------------------------------------------------


colCummaxs.default <-
    function(x, na.rm = FALSE, ...)
{
    # Description:

    # Arguments:

    # FUNCTION:

    # Cumulated Maxima:
    ans = apply(as.matrix(x), 2, cummax, ...)
    colnames(ans) = colnames(x)

    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


colCummaxs.timeSeries <-
    function(x, na.rm = FALSE, ...)
{
    # Description:

    # Arguments:

    # FUNCTION:

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


################################################################################


colCummins <-
    function(x, na.rm = FALSE, ...)
{
    UseMethod("colCummins")
}


# ------------------------------------------------------------------------------


colCummins.default <-
    function(x, na.rm = FALSE, ...)
{
    # Description:

    # Arguments:

    # FUNCTION:

    # Cumulated minima:
    ans = apply(as.matrix(x), 2, cummin, ...)
    colnames(ans) = colnames(x)

    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


colCummins.timeSeries <-
    function(x, na.rm = FALSE, ...)
{
    # Description:

    # Arguments:

    # FUNCTION:

    # Cumulated minima:
    ans = colCummins(as.matrix(x, ...), ...)

    # Time Series Input ?
    if (class(x) == "timeSeries") {
        x@Data = ans
        ans = x
    }


    # Return Value:
    ans
}


################################################################################


colCumprods <-
    function(x, na.rm = FALSE, ...)
{
    UseMethod("colCumprods")
}


# ------------------------------------------------------------------------------


colCumprods.default <-
    function(x, na.rm = FALSE, ...)
{
    # Description:

    # Arguments:

    # FUNCTION:

    # Cumulated Maxima:
    ans = apply(as.matrix(x, ...), 2, cumprod, ...)
    colnames(ans) = colnames(x)

    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


colCumprods.timeSeries <-
    function(x, na.rm = FALSE, ...)
{
    # Description:

    # Arguments:

    # FUNCTION:

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


################################################################################


colCumreturns <-
    function(x, method = c("geometric", "simple"), na.rm = FALSE, ...)
{
    UseMethod("colCumreturns")
}


# ------------------------------------------------------------------------------


colCumreturns.default <-
    function(x, method = c("geometric", "simple"), na.rm = FALSE, ...)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Cumulates Returns from a stream of returns

    # Arguments:
    #   x - a vector, matrix, data frame and timeSeries.
    #       asset returns.}
    #   method - generate geometric (TRUE) or simple (FALSE) returns,
    #       default "geometric".

    # FUNCTION:

    # Handle Missing Values:
    if (na.rm) x = na.omit(x, ...)
    method <- match.arg(method)

    # Cumulative Returns:
    if (method == "geometric") {
        ans = colCumsums(x)
    } else if (method == "simple") {
        ans = colCumprods(1+x) - 1
    }
    colnames(ans) = colnames(x)

    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


colCumreturns.timeSeries <-
    function(x, method = c("geometric", "simple"), na.rm = FALSE, ...)
{
    # Description:

    # Arguments:

    # FUNCTION:

    # Cumulated Maxima:
    ans = colCumreturns(as.matrix(x), method = method, na.rm = na.rm, ...)

    # Time Series Input ?
    if (class(x) == "timeSeries") {
        x@Data = ans
        ans = x
    }

    # Return Value:
    ans
}


################################################################################

