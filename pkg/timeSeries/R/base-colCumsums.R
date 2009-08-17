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
#  colCumsums,matrix         S3 default method (for matrix objects)
#  colCumsums,timeSeries     S3 method for timeSeries objects
# FUNCTION:                 COLUMN CUMULATIVE MAXIMA:
#  colCummaxs                Computes cumulated maximum values
#  colCummaxs,matrix         S3 default method (for matrix objects)
#  colCummaxs,timeSeries     S3 method for timeSeries objects
# FUNCTION:                 COLUMN CUMULATIVE MAXIMA:
#  colCummins                Computes cumulated maximum values
#  colCummins,matrix         S3 default method (for matrix objects)
#  colCummins,timeSeries     S3 method for timeSeries objects
# FUNCTION:                 COLUMN CUMULATIVE MINIMA:
#  colCumprods               Computes cumulated product values
#  colCumprods,matrix        S3 default method (for matrix objects)
#  colCumprods,timeSeries    S3 method for timeSeries objects
# FUNCTION:                 COLUMN CUMULATIVE RETURNS:
#  colCumreturns             Computes cumulated product values
#  colCumreturns,matrix      S3 default method (for matrix objects)
#  colCumreturns,timeSeries  S3 method for timeSeries objects
################################################################################

# ------------------------------------------------------------------------------

setMethod("colCumsums", "matrix",
    function(x, na.rm = FALSE, ...)
          apply(if (na.rm) na.omit(x) else x, 2, cumsum, ...))

# ------------------------------------------------------------------------------

setMethod("colCumsums", "timeSeries",
          function(x, na.rm = FALSE, ...)
          apply(if (na.rm) na.omit(x) else x, 2, cumsum, ...))

# ------------------------------------------------------------------------------

setMethod("colCummaxs", "matrix",
          function(x, na.rm = FALSE, ...)
          apply(if (na.rm) na.omit(x) else x, 2, cummax, ...))

# ------------------------------------------------------------------------------

setMethod("colCummaxs", "timeSeries",
          function(x, na.rm = FALSE, ...)
          apply(if (na.rm) na.omit(x) else x, 2, cummax, ...))

# ------------------------------------------------------------------------------

setMethod("colCummins", "matrix",
          function(x, na.rm = FALSE, ...)
          apply(if (na.rm) na.omit(x) else x, 2, cummin, ...))

# ------------------------------------------------------------------------------

setMethod("colCummins", "timeSeries",
          function(x, na.rm = FALSE, ...)
          apply(if (na.rm) na.omit(x) else x, 2, cummin, ...))

# ------------------------------------------------------------------------------

setMethod("colCumprods", "matrix",
          function(x, na.rm = FALSE, ...)
          apply(if (na.rm) na.omit(x) else x, 2, cumprod, ...))

# ------------------------------------------------------------------------------

setMethod("colCumprods", "timeSeries",
          function(x, na.rm = FALSE, ...)
          apply(if (na.rm) na.omit(x) else x, 2, cumprod, ...))

# ------------------------------------------------------------------------------

setMethod("colCumreturns", "matrix",
          function(x, method = c("geometric", "simple"), na.rm = FALSE, ...)
      {
          # A function implemented by Diethelm Wuertz and Yohan Chalabi

          # Description:
          #   Cumulates Returns from a stream of returns

          # Arguments:
          #   x      : a matrix object
          #   method : generate geometric or simple returns,
          #            default "geometric".

          # FUNCTION:

          # Handle Missing Values:
          if (na.rm) x <- na.omit(x, ...)
          method <- match.arg(method)

          # Return Value
          switch(method,
                 "geometric" = colCumsums(x),
                 "simple" = colCumprods(1+x) - 1)
      })

# ------------------------------------------------------------------------------

setMethod("colCumreturns", "timeSeries",
          function(x, method = c("geometric", "simple"), na.rm = FALSE, ...)
      {
          # A function implemented by Diethelm Wuertz and Yohan Chalabi

          # Description:
          #   Cumulates Returns from a stream of returns

          # Arguments:
          #   x      : a timeSeries object
          #   method : generate geometric or simple returns,
          #            default "geometric".

          # FUNCTION:

          # Handle Missing Values:
          if (na.rm) x <- na.omit(x, ...)
          method <- match.arg(method)

          # Return Value
          switch(method,
                 "geometric" = colCumsums(x),
                 "simple" = colCumprods(1+x) - 1)
      })

################################################################################
