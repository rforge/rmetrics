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

          # check if FinCenter are identical
          if (finCenter(e1) != finCenter(e2)) {
              finCenter(e2) <- finCenter(e1)
              warning("FinCenter changed to ", finCenter(e2), call. = FALSE)
          }

          # check if positions are identical
          if (!identical(time(e1), time(e2)))
              stop("positions slot do not match")

          series(e1) <- callGeneric(as(e1, "matrix"), as(e2, "matrix"))

          e1@recordIDs <- data.frame(e1@recordIDs, e2@recordIDs)

          e1
      })

setMethod("Ops", c("timeSeries", "ANY"),
          function(e1, e2)
      {
          series(e1) <- callGeneric(as(e1, "matrix"), e2)
          e1
      })

setMethod("Ops", c("ANY", "timeSeries"),
          function(e1, e2)
      {
          series(e2) <- callGeneric(e1, as(e2, "matrix"))
          e2
      })

# important for +/- timeSeries()
setMethod("+", c("timeSeries", "missing"), function(e1, e2) e1)
setMethod("-", c("timeSeries", "missing"), function(e1, e2) 0-e1)

# ------------------------------------------------------------------------------
#
# Auto-generate S4 methods for Math, Math2 and Summary

.members <- c("Math", "Math2", "Summary")

# template definition
.template <-  c("{",
                "finCenter <- finCenter(ANY)",
                "ANY <- as.matrix(ANY)",
                "ans <- callNextMethod()",
                "if (is.matrix(ans))",
                "    ans <- timeSeries(ans, zone = finCenter,",
                "                      FinCenter = finCenter)",
                "ans",
                "}")

# set the Methods in a loop
for (.f in .members) {
    .def <- function()NULL
    formals(.def) <- formals(.f)
    .bodyText <- gsub("ANY", names(formals(.f))[1], .template)
    body(.def) <- parse(text = .bodyText)
    setMethod(.f, "timeSeries", .def)
}

# ------------------------------------------------------------------------------

###      Note that two members of the 'Math' group, 'log' and 'trunc', have
###      more than one argument: S4 group dispatch will always pass only
###      one argument to the method so if you want to handle 'base' in
###      'log', set a specific method as well.

setMethod("log",
          "timeSeries",
          function(x, base = exp(1)) {
              FinCenter <- finCenter(x)
              x <- as.matrix(x)
              ans <- log(x, base = base)
              ans <- timeSeries(ans, zone = FinCenter,
                                FinCenter = FinCenter)
              ans
          })
setMethod("trunc",
          "timeSeries",
          function(x, ...) {
              FinCenter <- finCenter(x)
              x <- as.matrix(x)
              ans <- trunc(x, ...)
              ans <- timeSeries(ans, zone = FinCenter,
                                FinCenter = FinCenter)
              ans
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
    x@Data = scale(x = x@Data, center = center, scale = scale)

    # Return Value:
    x
})

################################################################################
