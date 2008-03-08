
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
# FUNCTION:                 COLUMN CUMULATIVE SUMS:
#  colCumsums                Computes sample cumulated sums by column
#  colCumsums.default        S3 default method (for matrix objects)
#  colCumsums.timeSeries     S3 method for timeSeries objects
# FUNCTION:                 COLUMN CUMULATIVE MAXIMA:
#  colCummaxs                Computes cumulated maximum values
#  colCummaxs.default        S3 default method (for matrix objects)
#  colCummaxs.timeSeries     S3 method for timeSeries objects
# FUNCTION:                 COLUMN CUMULATIVE MINIMA:
#  colCumprods               Computes cumulated product values
#  colCumprods.default       S3 default method (for matrix objects)
#  colCumprods.timeSeries    S3 method for timeSeries objects
# FUNCTION:                 COLUMN CUMULATIVE RETURNS:
#  colCumreturns             Computes cumulated product values
#  colCumreturns.default     S3 default method (for matrix objects)
#  colCumreturns.timeSeries  S3 method for timeSeries objects
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


.conflicts.OK = TRUE


# ------------------------------------------------------------------------------


colCumsums =
function(x, na.rm = FALSE, ...)
{
    UseMethod("colCumsums")
}


# ------------------------------------------------------------------------------


colCumsums.default =
function(x, na.rm = FALSE, ...)
{   # A function implemented by Diethelm Wuertz

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


colCumsums.timeSeries =
function(x, na.rm = FALSE, ...)
{   # A function implemented by Diethelm Wuertz

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


colCummaxs =
function(x, na.rm = FALSE, ...)
{
    UseMethod("colCummaxs")
}


# ------------------------------------------------------------------------------


colCummaxs.default =
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


colCummaxs.timeSeries =
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


colCummins =
function(x, na.rm = FALSE, ...)
{
    UseMethod("colCummmins")
}


# ------------------------------------------------------------------------------


colCummins.default =
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


colCummins.timeSeries =
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


colCumprods =
function(x, na.rm = FALSE, ...)
{
    UseMethod("colCumprods")
}


# ------------------------------------------------------------------------------


colCumprods.default =
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


colCumprods.timeSeries =
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


colCumreturns =
function(x, method = c("geometric", "simple"), na.rm = FALSE, ...)
{
    UseMethod("colCumreturns")
}


# ------------------------------------------------------------------------------


colCumreturns.default =
function(x, method = c("geometric", "simple"), na.rm = FALSE, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Cumulates Returns from a stream of returns

    # Arguments:
    #   x - a vector, matrix, data frame and timeSeries.
    #       asset returns.}
    #   method - generate geometric (TRUE) or simple (FALSE) returns,
    #       default "geometric".

    # FUNCTION:

    # Handle Missing Values:
    if (na.rm) R = na.omit(R, ...)

    # Cumulative Returns:
    if (method == "geometric") {
        ans = cumsum(R)
    } else if (method == "simple") {
        ans = cumprod(1+R) - 1
    }
    names(ans) = colnames(R)


    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


colCumreturns.timeSeries =
function(x, method = c("geometric", "simple"), na.rm = FALSE, ...)
{
    # Description:

    # Arguments:

    # FUNCTION:

    # Cumulated Maxima:
    ans = colCumreturns(as.matrix(x, ...), na.rm, ...)

    # Time Series Input ?
    if (class(x) == "timeSeries") {
        x@Data = ans
        ans = x
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

