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
#  Compare,timeSeries        Returns group 'Compare' for a 'timeSeries' object
#  Ops,timeSeries            Returns group 'Ops' for a 'timeSeries' object
#  Math,timeSeries           Returns group Math for a 'timeSeries' object
#  Math2,timeSeries          Returns group Math2 for a 'timeSeries' object
#  Summary,timeSeries        Returns group Summary for a 'timeSeries' object
#  diff,timeSeries           Differences a 'timeSeries' object
#  scale,timeSeries          Centers and/or scales a 'timeSeries' object
#  quantile,timeSeries       Returns quantiles of an univariate 'timeSeries'
################################################################################

setMethod("Ops", c("timeSeries", "timeSeries"),
          function(e1, e2)
      {

          test = as.integer((e1@format == "counts") + (e2@format == "counts"))
          switch(as.character(test),
                 # convert series y to FinCenter of series x
                 "0" = {
                     if (finCenter(e1) != finCenter(e2)) {
                         finCenter(e2) <- finCenter(e1)
                         warning("FinCenter changed to ",
                                 finCenter(e2), call. = FALSE)}
                 },
                 # if one of the two series is  signal series, the other
                 # series is converted to a signal series
                 "1" = {
                     e1 <- timeSeries(e1, format = "counts");
                     e2 <- timeSeries(e2, format = "counts")
                 })

          # check if positions are identical
          if (!identical(time(e1), time(e2)))
              stop("positions slot do not match")

          series(e1) <- callGeneric(as(e1, "matrix"), as(e2, "matrix"))
          e1@recordIDs <- data.frame(e1@recordIDs, e2@recordIDs)

          # should construct new timeSeries with name combines of the others ...

          e1
      })

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
          y = as.matrix(x)

          # Check NAs:
          # if (any(is.na(y))) stop("NAs are not allowed in time series")

          # Difference:
          z = diff(y, lag = lag, difference = diff)

          # Trim:
          if (!trim) {
              diffNums = dim(y)[1] - dim(z)[1]
              zpad = matrix(0*y[1:diffNums, ] + pad, nrow = diffNums)
              rownames(zpad) = rownames(y)[1:diffNums]
              z = rbind(zpad, z)
          }

          # Record IDs:
          df = x@recordIDs
          if (trim) {
              if (sum(dim(df)) > 0) {
                  TRIM = dim(df)[1] - dim(z)[1]
                  df = df[-(1:TRIM), ]
              }
          }

          # Return Value:
          timeSeries(data = z, charvec = rownames(z), units = colnames(z),
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
    series(x) = scale(x = series(x), center = center, scale = scale)

    # Return Value:
    x
})

################################################################################
