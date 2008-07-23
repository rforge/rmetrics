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
# METHOD:                   SUBSETTING METHODS ON DATA:
#  [.timeSeries              Subsets of a 'timeSeries' object
#  [<-.timeSeries            Assign value to subsets of a 'timeSeries' object
#  cut.timeSeries            Cuts a block from a 'timeSeries' object
#  windows.timeSeries        Windows a piece from a 'timeSeries' object.
#  head.timeSeries           Returns the head of a 'timeSeries' object
#  tail.timeSeries           Returns the tail of a 'timeSeries' object
#  outlier.timeSeries        Removes outliers from a 'timeSeries' object
################################################################################

# ------------------------------------------------------------------------------
# index

setMethod("[", signature(x = "timeSeries", i = "index_timeSeries", j = "index_timeSeries"),
          function(x, i, j, ..., drop = FALSE)
      {
          # series settings
          format = x@format
          if (format != "counts")
              FinCenter <- finCenter(x)

          # subset data and positions
          data <- series(x)[i, j, drop = drop]
          charvec <- rownames(data)

          # to handle special case when
          # series(x)[i, j, drop = drop] returns numeric(0)
          if (!NROW(data) || !NCOL(data)) data <- NULL

          # Record IDs:
          df <- x@recordIDs
          recordIDs <- if (sum(dim(df)) > 0) df[i, , drop = drop] else df

          # Return new timeSeries
          new("timeSeries", data = data, charvec = charvec, format = format,
              zone = FinCenter, FinCenter = FinCenter,
              recordIDs = recordIDs)
      })

setMethod("[", signature(x = "timeSeries", i = "index_timeSeries", j = "missing"),
          function(x, i, j, ..., drop = FALSE)
      {
          if(nargs() == 2) {
              if(any(as.logical(i)) || prod(dim(x)) == 0)
                   as.vector(x)[i]
          } else {
              callGeneric(x, i = i, j = min(1, NCOL(x)):NCOL(x),
                          drop = drop, ...)
          }
      })

setMethod("[", signature(x = "timeSeries", i = "missing", j = "index_timeSeries"),
          function(x, i, j, ..., drop = FALSE)
          callGeneric(x, i = min(1, NROW(x)):NROW(x), j = j, drop = drop, ...))

# ------------------------------------------------------------------------------
# timeDate

setMethod("[", signature(x = "timeSeries", i = "timeDate", j = "index_timeSeries"),
          function(x, i, j, ..., drop = FALSE)
      {
          # series settings
          format = x@format
          if (format != "counts") {
              FinCenter <- finCenter(x)
              # convert FinCenter of index_timeSeries to FinCenter of timeSeries
              i <- timeDate(i, zone = i@FinCenter, FinCenter = FinCenter)
          }
          i <- as(i, "character")
          callGeneric(x, i=i, j=j, drop=drop, ...)
      })

setMethod("[", signature(x = "timeSeries", i = "timeDate", j = "missing"),
          function(x, i, j, ..., drop = FALSE)
          callGeneric(x, i = i, j = min(1, NCOL(x)):NCOL(x), drop = drop, ...))

# ------------------------------------------------------------------------------
# matrix

setMethod("[", signature(x = "timeSeries", i = "matrix", j = "index_timeSeries"),
          function(x, i, j, ..., drop = FALSE)
          callGeneric(x, i = as.vector(i), j = j, drop = drop, ...))

setMethod("[", signature(x = "timeSeries", i = "matrix", j = "missing"),
          function(x, i, j, ..., drop = FALSE)
      {
          if(nargs() == 2) {
              if(any(as.logical(i)) || prod(dim(x)) == 0)
                   as.vector(x)[i]
          } else {
              callGeneric(x, i = i, j = min(1, NCOL(x)):NCOL(x),
                          drop = drop, ...)
          }
      })

# ------------------------------------------------------------------------------
# timeSeries

setMethod("[", signature(x = "timeSeries", i = "timeSeries", j = "index_timeSeries"),
          function(x, i, j, ..., drop = FALSE)
      {
          if (x@format != "counts" &&
              i@format != "counts" &&
              finCenter(x) != finCenter(i))
              stop("FinCenter of timeSeries and subset do not match")
          callGeneric(x, i = series(i), j = j, drop = drop, ...)
      })

setMethod("[", signature(x = "timeSeries", i = "timeSeries", j = "missing"),
          function(x, i, j, ..., drop = FALSE)
      {
          if(nargs() == 2) {
              if(any(as.logical(i)) || prod(dim(x)) == 0)
                   as.vector(x)[series(i)]
          } else {
              callGeneric(x, i = i, j = min(1, NCOL(x)):NCOL(x),
                          drop = drop, ...)
          }
      })

# ------------------------------------------------------------------------------

setMethod("[",
          signature(x = "timeSeries", i = "missing", j = "missing"),
          function(x, i, j, ..., drop = FALSE) x)

# ------------------------------------------------------------------------------
# ANY

setMethod("[", signature(x = "timeSeries", i = "ANY", j = "ANY"),
          function(x,i,j, ..., drop = FALSE)
          stop("invalid or not-yet-implemented 'timeSeries' subsetting"))

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

    value <- as.matrix(value)

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
{   # A function implemented by Diethelm Wuertz and Yohan Chalabi

    # Description:
    #   Cuts out a piece from a 'timeSeries' object.

    # Arguments:
    #   x - a 'timeSeries' object
    #   from, to - two 'timeDate' position vectors which size the
    #       blocks

    # Value:
    #   Returns a S4 object of class 'timeSeries'.

    # FUNCTION:

    .Deprecated("window", "fSeries")

    stopifnot(is.timeSeries(x))
    if (x@format == "counts")
        stop(as.character(match.call())[1],
             " is for time series and not for signal series.")

    from = timeDate(from)
    to = timeDate(to)
    Positions = time(x)

    test = (Positions >= from & Positions <= to)
    ans <- x[test,]

    # Return value:
    ans
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
    stopifnot(is.timeSeries(x))
    if (x@format == "counts")
        stop(as.character(match.call())[1],
             " is for time series and not for signal series.")

    from = timeDate(from)
    to = timeDate(to)
    Positions = time(x)

    test = (Positions >= from & Positions <= to)
    ans <- x[test,]

    # Return value:
    ans
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
    cat(x@FinCenter, "\n", sep = "")
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
    cat(x@FinCenter, "\n", sep = "")
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

