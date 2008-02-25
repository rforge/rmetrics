
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
# METHOD:                   SUBSETTING METHODS ON DATA:
#  [.timeSeries              Subsets of a 'timeSeries' object
#  [<-.timeSeries            Assign value to subsets of a 'timeSeries' object
#  cut.timeSeries            Cuts a block from a 'timeSeries' object
#  windows.timeSeries        Windows a piece from a 'timeSeries' object.
#  head.timeSeries           Returns the head of a 'timeSeries' object
#  tail.timeSeries           Returns the tail of a 'timeSeries' object
#  outlier.timeSeries        Removes outliers from a 'timeSeries' object
################################################################################


"[.timeSeries" <-
    function(x, i , j)
{   # A function implemented by Diethelm Wuertz
    # Modified by Yohan Chalabi

    # Description:
    #   Extracts or replaces subsets from 'timeSeries' objects

    # Arguments:
    #   x - a 'timeSeries' object
    #   i, j - subset indexes.

    # Value:
    #   Returns a subset from an object 'timeSeries'.

    # FUNCTION:

    # Check Timezone:
    TZ = Sys.getenv("TZ")
    if (TZ[[1]] != "GMT") {
        Sys.setenv(TZ = "GMT")
        TZ.RESET = TRUE
    } else {
        TZ.RESET = FALSE
    }

    # Subsets:
    if(missing(i)) { i <- min(1, nrow(x@Data)):nrow(x@Data) }
    if(missing(j)) { j <- min(1, ncol(x@Data)):ncol(x@Data) }

    if (is.timeSeries(i) && is.logical(i@Data))
        i <- as.logical(i@Data)

    if (!is.numeric(i) && !is.logical(i))
        i <- as.character(i)

    # Subset:
    subx <- x@Data[i, j, drop = FALSE]
    x@Data = subx
    x@positions = x@positions[i]
    x@units = colnames(subx)

    # Record IDs:
    if (sum(dim(x@recordIDs)) > 0) {
        x@recordIDs <- x@recordIDs[i, , drop = FALSE]
    }


    # Return Value:
    if (TZ.RESET) Sys.setenv(TZ = TZ)
    x
}

# ------------------------------------------------------------------------------

"[<-.timeSeries" <-
    function(x, i, j, value)
{   # A function implemented by Yohan Chalabi

    # Description:
    #   assign subsets of 'timeSeries' objects

    # Arguments:
    #   x - a 'timeSeries' object
    #   i, j - subset indexes.
    #   value - value to be assign

    # Value:
    #   Returns an object 'timeSeries' with the new value assigned.

    # FUNCTION:

    # Subsets:

    if (!missing(i) && is.timeSeries(i) && is.logical(i@Data))
        i <- as.logical(i@Data)

    if (!missing(i) && !is.numeric(i) && !is.logical(i))
        i <- as.character(i)

    if (!missing(i) && !missing(j)) x@Data[i,j] <- value
    if (!missing(i) && missing(j)) x@Data[i,] <- value
    if (missing(i) && !missing(j)) x@Data[,j] <- value

    # Return Value:
    x
}

# ------------------------------------------------------------------------------

cut.timeSeries =
function (x, from, to, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Cuts out a piece from a 'timeSeries' object.

    # Arguments:
    #   x - a 'timeSeries' object
    #   from, to - two 'timeDate' position vectors which size the
    #       blocks

    # Value:
    #   Returns a S4 object of class 'timeSeries'.

    # FUNCTION:

    from = timeDate(from)
    to = timeDate(to)
    Positions = seriesPositions(x)
    Units = x@units
    colNames = colnames(x@Data)
    test = (Positions >= from & Positions <= to)
    Data = as.matrix(x@Data)[test, ]
    Data = as.matrix(Data)
    x@Data = Data
    x@positions = x@positions[test]
    x@units = Units
    x@recordIDs = data.frame()
    colnames(x@Data) = colNames

    # Return value:
    x
}


.cut.timeSeries =
function(x, from, to, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Cuts out a piece from a 'timeSeries' object.

    # Arguments:
    #   x - a 'timeSeries' object
    #   from, to - two 'timeDate' position vectors which size the
    #       blocks

    # Value:
    #   Returns a S4 object of class 'timeSeries'.

    # FUNCTION:

    # Check:
    stopifnot(is.timeSeries(x))
    if (!is(from, "timeDate"))
        from = as.timeDate(x, zone = x@FinCenter, FinCenter = x@FinCenter)
    if (!is(to, "timeDate"))
        to = as.timeDate(x, zone = x@FinCenter, FinCenter = x@FinCenter)

    Positions = seriesPositions(x)
    if (missing(from)) from = Positions[1]
    if (missing(to)) to = rev(Positions)[1]
    Positions = as.POSIXct(Positions, tz = "GMT")
    from = as.POSIXct(from, tz = "GMT")
    to = as.POSIXct(to, tz = "GMT")

    # Cut:
    test = (Positions >= from & Positions <= to)
    Index = (1:length(test))[test]
    if (length(Index) == 0) return()

    # Return value:
    x[Index, ]
}


# ------------------------------------------------------------------------------


window.timeSeries =
function (x, from, to, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Windows a piece from a 'timeSeries' object.

    # Arguments:
    #   x - a 'timeSeries' object
    #   from, to - two 'timeDate' position vectors which size the
    #       blocks

    # Details:
    #   from and to, are both included in the window.

    # Value:
    #   Returns a S4 object of class 'timeSeries'.

    # FUNCTION:

    from = timeDate(from)
    to = timeDate(to)
    Positions = seriesPositions(x)
    Units = x@units
    colNames = colnames(x@Data)
    test = (Positions >= from & Positions <= to)
    Data = as.matrix(x@Data)[test, ]
    Data = as.matrix(Data)
    x@Data = Data
    x@positions = x@positions[test]
    x@units = Units
    x@recordIDs = data.frame()
    colnames(x@Data) = colNames

    # Return value:
    x
}


# ------------------------------------------------------------------------------


head.timeSeries =
function(x, n = 6, recordIDs = FALSE, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns the head of a 'timeSeries' object

    # Arguments:
    #   x - a 'timeSeries' object.

    # Value:
    #   Returns the head of an object of class 'timeSeries'.

    # FUNCTION:

    # Head:
    if (recordIDs) {
        if (dim(x@Data)[1] == dim(x@recordIDs)[1]) {
            Head = head(cbind(x@Data, as.matrix(x@recordIDs)), n = n, ...)
        } else {
            Head = head(x@Data, n = n, ...)
        }
    } else {
        Head = head(x@Data, n = n, ...)
    }

    # Return Value:
    Head
}


# ------------------------------------------------------------------------------


tail.timeSeries =
function(x, n = 6, recordIDs = FALSE, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns the tail of a 'timeSeries' object

    # Arguments:
    #   x - a 'timeSeries' object.

    # Value:
    #   Returns the tail of an object of class 'timeSeries'.

    # FUNCTION:

    # Tail:
    if (recordIDs) {
        if (dim(x@Data)[1] == dim(x@recordIDs)[1]) {
            Tail = tail(cbind(x@Data, as.matrix(x@recordIDs)), n = n, ...)
        } else {
            Tail = tail(x@Data, n = n, ...)
        }
    } else {
        Tail = tail(x@Data, n = n, ...)
    }

    # Return Value:
    Tail
}


# ------------------------------------------------------------------------------


outlier.timeSeries =
function(x, sd = 10, complement = TRUE, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns outliers in a timeSeries object or the complement

    # Arguments:
    #   x - a 'timeSeries' object.
    #   sd - a numeric value of standard deviations, e.g. 10
    #       means that values larger or smaller tahn ten
    #       times the standard deviation of the series will
    #       be removed.
    #   complement - a logical flag, should the outler series
    #       or its complement be returns.

    # FUNCTION:

    # Check if univariate Series:
    if (dim(x@Data)[2] != 1)
        stop("Supports only univariate timeSeries Objects")

    # Find Outliers:
    SD = sd * sd(x@Data)
    if (complement) {
        x  = x[abs(x@Data) <= SD]
    } else {
        x = x[abs(x@Data) > SD]
    }

    # Return Value:
    x
}


################################################################################

