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
#  as.list.timeSeries        Converts a 'timeSeries' to 'list'
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

# ------------------------------------------------------------------------------

# YC:
# here keep S3 methods because it should expect an oldClass object as argument

# ------------------------------------------------------------------------------

as.timeSeries <- function(x, ...) UseMethod("as.timeSeries")

# ------------------------------------------------------------------------------

as.timeSeries.default <- function(x, ...)
{   # A function implemented by Diethelm Wuertz
    # Extended by Yohan Chalabi

    # FUNCTION:

    # Return Value:
    ans <- timeSeries(x, ...)

    ans
}

setAs("ANY", "timeSeries", function(from) as.timeSeries(from))

# ------------------------------------------------------------------------------

as.timeSeries.data.frame <- function(x, ...)
{   # A function implemented by Diethelm Wuertz
    # Extended by Yohan Chalabi

    # Description:
    #   Converts a data.frame into a timeSeries object

    # Notes:
    #   The first column must contain the dates.

    # Examples:
    #   data(bmwRet); head(as.timeSeries(data(bmwRet)))

    # FUNCTION:

    if (all(!(num <- unlist(lapply(x, is.numeric)))))
        stop("x contains no numeric columns")

    # Check if rownames(x) or the first column has a valid ISO-format:
    if (num[1])
        # is.numeric() is better than format == "unkown"
        # which can give wrong result. i.e. whichFormat(0.1253328600)
        suppressWarnings(charvec <- timeDate(rownames(x)))
    else
        suppressWarnings(charvec <- timeDate(as.vector(x[,1])))

    data <- as.matrix(x[, num])
    units <- names(x)[num]
    if (any(!(cl <- num[-1]))) {
        recordIDs <- as.data.frame(x[, !c(TRUE, cl)]) # do not take first column
        names(recordIDs) <- names(x)[!c(TRUE, cl)]
    } else {
        recordIDs <- data.frame()
    }

    # Create Time Series Object:
    timeSeries(data = data,
               charvec = charvec,
               units = units,
               recordIDs = recordIDs, ...)
}

setAs("data.frame", "timeSeries", function(from) as.timeSeries(from))

# ------------------------------------------------------------------------------

as.timeSeries.character <- function(x, ...)
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

setAs("character", "timeSeries", function(from) as.timeSeries(from))

# ------------------------------------------------------------------------------

as.timeSeries.zoo <- function(x, ...)
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

# YC : since 2.9.0 must define proper S4 methods

setMethod("as.matrix", "timeSeries", function(x, ...)
      {   # A function implemented by Diethelm Wuertz

          # Description:
          #   Converts a multivariate "timeSeries" to a matrix

          # Arguments:
          #   x - a 'timeSeries' object

          # Value:
          #   Returns the data slot of a 'timesSeries' object as a vector.

          # FUNCTION:

          # Check:
          if (!inherits(x, "timeSeries"))
              stop("x is not a timeSeries object!")

          # Convert:
          ans <- getDataPart(x) # is matrix
          dimnames(ans) <- dimnames(x)

          # Results
          ans
      })

setAs("timeSeries", "matrix", function(from) as.matrix(from))

# ------------------------------------------------------------------------------

setMethod("as.data.frame", "timeSeries",
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
      })

setAs("timeSeries", "data.frame", function(from) as.data.frame(from))

# ------------------------------------------------------------------------------

setMethod("as.ts", "timeSeries",
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
      })

setAs("timeSeries", "ts", function(from) as.ts(from))

# ------------------------------------------------------------------------------

## YC : unneeded since timeSeries inherits from the structure class
## as.logical.timeSeries <- function(x, ...) as.logical(series(x), ...)

# ------------------------------------------------------------------------------

# YC : important for functions like lapply and sapply to work properly
setMethod("as.list", "timeSeries", function(x, ...)
      {
          data <- getDataPart(x)
          ncols <- NCOL(data)
          value <- vector("list", ncols)
          for (i in seq.int(ncols)) value[[i]] <- as.vector(data[, i])
          names(value) <- colnames(x)
          value
      })

setAs("timeSeries", "list", function(from) as.list(from))

################################################################################
