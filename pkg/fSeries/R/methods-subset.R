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

## "[.timeSeries" <-
##     function(x, i , j)
## {   # A function implemented by Diethelm Wuertz
##     # Modified by Yohan Chalabi

##     # Description:
##     #   Extracts or replaces subsets from 'timeSeries' objects

##     # Arguments:
##     #   x - a 'timeSeries' object
##     #   i, j - subset indexes.

##     # Value:
##     #   Returns a subset from an object 'timeSeries'.

##     # FUNCTION:

##     # Check Timezone:
##     TZ = Sys.getenv("TZ")
##     if (TZ[[1]] != "GMT") {
##         Sys.setenv(TZ = "GMT")
##         TZ.RESET = TRUE
##     } else {
##         TZ.RESET = FALSE
##     }

##     # Subsets:
##     if(missing(i)) { i <- min(1, nrow(x@Data)):nrow(x@Data) }
##     if(missing(j)) { j <- min(1, ncol(x@Data)):ncol(x@Data) }

##     if (is.timeSeries(i) && is.logical(i@Data))
##         i <- as.logical(i@Data)

##     if (!is.numeric(i) && !is.logical(i))
##         i <- as.character(i)

##     # Subset:
##     subx <- x@Data[i, j, drop = FALSE]
##     x@Data = subx
##     x@positions = x@positions[i]
##     x@units = as.character(colnames(subx))
##     # YC as.character important if colnames == NULL

##     # Record IDs:
##     if (sum(dim(x@recordIDs)) > 0) {
##         x@recordIDs <- x@recordIDs[i, , drop = FALSE]
##     }


##     # Return Value:
##     if (TZ.RESET) Sys.setenv(TZ = TZ)
##     x
## }

setMethod("[", signature(x = "timeSeries", i = "missing", j = "missing", drop = "ANY"), function(x, i, j, drop, ...) x)

setMethod("[", signature(x = "timeSeries", i = "index", j = "missing", drop = "missing"), function(x, i, j, drop, ...)
      {
          if(nargs() == 2) { ## e.g. M[0] , M[TRUE],  M[1:2]
              if(any(as.logical(i)) || prod(dim(x)) == 0)
                   as.vector(x)[i]
          } else {
              callGeneric(x, i=i, drop = FALSE)
          }
      })

## select columns
setMethod("[", signature(x = "timeSeries", i = "missing", j = "index", drop = "missing"), function(x,i, j, drop, ...) callGeneric(x, j = j, drop = FALSE))

setMethod("[", signature(x = "timeSeries", i = "index", j = "index", drop = "missing"), function(x,i,j, drop, ...) callGeneric(x, i = i, j = j, drop = FALSE))

setMethod("[", signature(x = "timeSeries", i = "ANY", j = "ANY", drop = "ANY"), function(x,i,j, drop, ...) stop("invalid or not-yet-implemented 'timeSeries' subsetting"))

setMethod("[", signature(x = "timeSeries", i = "matrix", j = "missing", drop = "missing"), function(x, i, j, drop, ...) callGeneric(x, i = i, drop=FALSE))

setMethod("[", signature(x = "timeSeries", i = "matrix", j = "missing", drop = "logical"),
          function(x, i, j, drop, ...)
      {
          i <- as(i, "vector")

          if (!is.logical(i))
              stop("matrix used as an index must be logical")

          if (NROW(i) == NROW(x))
              callGeneric(x, i = i , drop = drop, ...)
          else
              as.vector(x)[i]
      })

setMethod("[", signature(x = "timeSeries", i = "timeSeries", j = "missing", drop = "missing"), function(x, i, j, drop, ...) callGeneric(x, i = i, drop=FALSE))

setMethod("[", signature(x = "timeSeries", i = "timeSeries", j = "missing", drop = "logical"),
          function(x, i, j, drop, ...)
      {
          if (x@format != "counts" &&
              i@format != "counts" &&
              finCenter(x) != finCenter(i))
              stop("FinCenter of timeSeries and subset do not match")

          i <- as(i, "vector")

          if (!is.logical(i))
              stop("timeSeries used as an index must be logical")

          if (NROW(i) == NROW(x))
              callGeneric(x, i = i , drop = drop, ...)
          else
              as.vector(x)[i]
      })

setMethod("[", signature(x = "timeSeries", i = "timeDate", j = "missing", drop = "missing"), function(x, i, j, drop, ...)
          callGeneric(x, i=i, j = min(1, NCOL(x)):NCOL(x), drop=FALSE, ...))

setMethod("[", signature(x = "timeSeries", i = "timeDate", j = "index", drop = "missing"), function(x, i, j, drop, ...) callGeneric(x, i=i, j=j, drop=FALSE, ...))

setMethod("[", signature(x = "timeSeries", i = "timeDate", j = "index", drop = "logical"),
          function(x, i, j, drop, ...)
      {
          # series settings
          format = x@format
          if (format != "counts") {
              FinCenter <- finCenter(x)
              # convert FinCenter of index to FinCenter of timeSeries
              i <- timeDate(i, zone = i@FinCenter, FinCenter = FinCenter)
          }

          i <- as(i, "character")
          callGeneric(x, i=i, j=j, drop=drop)
      })

setMethod("[", signature(x = "timeSeries", i = "index", j = "missing",
                         drop = "logical"),
          function(x, i, j, drop, ...)
          callGeneric(x, i = i, j = min(1, NCOL(x)):NCOL(x), drop = drop))

setMethod("[", signature(x = "timeSeries", i = "missing", j = "index", drop = "logical"), function(x, i, j, drop, ...)
          callGeneric(x, i = min(1, NROW(x)):NROW(x), j = j, drop = drop))

setMethod("[", signature(x = "timeSeries", i = "index", j = "index",
                         drop = "logical"),
          function(x, i, j, drop, ...)
      {
          # series settings
          format = x@format
          if (format != "counts")
              FinCenter <- finCenter(x)

          # subset data and positions
          data <- series(x)[i, j, drop = drop]
          charvec = rownames(data)

          # Record IDs:
          df <- x@recordIDs
          recordIDs <- if (sum(dim(df)) > 0) df[i, , drop = drop] else df

          # Return new timeSeries
          new("timeSeries", data = data, charvec = charvec, format = format,
              zone = FinCenter, FinCenter = FinCenter,
              recordIDs = recordIDs)
      })

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

