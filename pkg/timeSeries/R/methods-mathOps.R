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
# S4 METHOD:                MATHEMATICAL OPERATIONS ON DATA:
#  Ops,timeSeries            Returns group 'Ops' for a 'timeSeries' object
#  diff,timeSeries           Differences a 'timeSeries' object
#  scale,timeSeries          Centers and/or scales a 'timeSeries' object
#  quantile,timeSeries       Returns quantiles of an univariate 'timeSeries'
################################################################################

# ------------------------------------------------------------------------------
# Ops

setMethod("Ops", c("vector", "timeSeries"),
          function(e1, e2)
      {
          lattrs <- attributes(e2)
          e2 <- getDataPart(e2)
          value <- callGeneric(e1, e2)
          if (identical(dim(value), dim(e2))) {
              attributes(value) <- lattrs
              value <- asS4(value, TRUE)
          }
          value
      })

setMethod("Ops", c("array", "timeSeries"),
          function(e1, e2)
      {
          e1 <- as.vector(e1)
          lattrs <- attributes(e2)
          e2 <- getDataPart(e2)
          value <- callGeneric(e1, e2)
          if (identical(dim(value), dim(e2))) {
              attributes(value) <- lattrs
              value <- asS4(value, TRUE)
          }
          value
      })

setMethod("Ops", c("timeSeries", "vector"),
          function(e1, e2)
      {
          lattrs <- attributes(e1)
          e1 <- getDataPart(e1)
          value <- callGeneric(e1, e2)
          if (identical(dim(value), dim(e1))) {
              attributes(value) <- lattrs
              value <- asS4(value, TRUE)
          }
          value
      })

setMethod("Ops", c("timeSeries", "array"),
          function(e1, e2)
      {
          lattrs <- attributes(e1)
          e1 <- getDataPart(e1)
          value <- callGeneric(e1, e2)
          if (identical(dim(value), dim(e1))) {
              attributes(value) <- lattrs
              value <- asS4(value, TRUE)
          }
          value
      })

setMethod("Ops", c("timeSeries", "timeSeries"),
          function(e1, e2)
      {
          # check if conformable arrays
          if (!identical(dim(e1), dim(e2)))
              stop("non-conformable arrays")
          # check if positions are identical
          if (!identical(e1@positions, e2@positions))
              stop("positions slot do not match")
          # save recordIDs
          recordIDs <- data.frame(e1@recordIDs, e2@recordIDs)
          lattrs <- attributes(e1)
          e1 <- getDataPart(e1)
          e2 <- getDataPart(e2)
          value <- callGeneric(e1, e2)
          if (identical(dim(value), dim(e1))) {
              attributes(value) <- lattrs
              value <- asS4(value, TRUE)
          }
          value
      })

# ------------------------------------------------------------------------------

## setMethod("cummax", "timeSeries", function(x) callGeneric(getDataPart(x)))
## setMethod("cummin", "timeSeries", function(x) callGeneric(getDataPart(x)))
## setMethod("cumprod", "timeSeries", function(x) callGeneric(getDataPart(x)))
## setMethod("cumsum", "timeSeries", function(x) callGeneric(getDataPart(x)))

# ------------------------------------------------------------------------------

setMethod("diff", "timeSeries",
          function(x, lag = 1, diff = 1, trim = FALSE, pad = NA, ...)
      {   # A function implemented by Diethelm Wuertz
          # Modified by Yohan Chalabi

          # Description:
          #   Difference 'timeSeries' objects.

          # Arguments:
          #   x - a 'timeSeries' object.
          #   lag - an integer indicating which lag to use.
          #       By default 1.
          #   diff - an integer indicating the order of the difference.
          #       By default 1.
          #   trim - a logical. Should NAs at the beginning of the
          #       series be removed?
          #   pad - a umeric value with which NAs should be replaced
          #       at the beginning of the series.

          # Value:
          #   Returns a differenced object of class 'timeSeries'.

          # FUNCTION:

          # Convert:
          y = getDataPart(x) # as.matrix(x)

          # Check NAs:
          # if (any(is.na(y))) stop("NAs are not allowed in time series")

          # Difference:
          z = diff(y, lag = lag, difference = diff)

          diffNums = dim(y)[1] - dim(z)[1]

          # Trim:
          if (!trim) {
              zpad = matrix(0*y[1:diffNums, ] + pad, nrow = diffNums)
              z = rbind(zpad, z)
          }

          pos <-
              if (!trim)
                  x@positions
              else
                  x@positions[-(1:diffNums)]

          # Record IDs:
          df <- x@recordIDs
          if (trim) {
              if (sum(dim(df)) > 0) {
                  TRIM = dim(df)[1] - dim(z)[1]
                  df = df[-(1:TRIM), ]
              }
          }

          # Return Value:
          timeSeries(data = z, charvec = pos, units = colnames(z),
                     format = x@format, zone = x@FinCenter,
                     FinCenter = x@FinCenter, recordIDs = df,
                     title = x@title, documentation = x@documentation)
      })


# ------------------------------------------------------------------------------

setMethod("scale", "timeSeries",
          function(x, center = TRUE, scale = TRUE)
{   # A function implemented by Diethelm Wuertz and Yohan Chalabi

    # Description:
    #   Centers and/or scales a 'timeSeries' object.

    # Arguments:

    # FUNCTION:

    # Scale:
    setDataPart(x, scale(x = getDataPart(x), center = center, scale = scale))
})

################################################################################
