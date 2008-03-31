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
# FUNCTION:                 COLUMN/ROW CUMULATIVE STATISTICS:
#  rowCumsums                Computes sample cumulated sums by row
#  rowCumsums,ANY            S4 method (for object which can be converted to  matrix
#  rowCumsums,timeSeries     S4 method for timeSeries objects
################################################################################


################################################################################
# @comments
#   DW 2007-09-20           further col* functions added, see ADDONS
################################################################################


################################################################################
# @todo
#
################################################################################


setMethod("rowCumsums", "ANY",
          function(x, na.rm = FALSE, ...)
      {   # A function implemented by Diethelm Wuertz and Yohan Chalabi

          # Description:
          #   Computes sample cumulated sums by row (for matrix objects)

          # Arguments:

          # FUNCTION:

          # Transform:
          X <- as(x, "matrix")

          # Statistics:
          if (na.rm) {
              result = apply(na.omit(X), MARGIN = 2, FUN = cumsum, ...)
          } else {
              result = apply(X, MARGIN = 2, FUN = cumsum, ...)
          }
          colnames(result) <- paste(1:NCOL(x))

          # Statistics:
          result <- apply(if(na.rm) na.omit(X) else X, 2, cumsum, ...)

          # Return Value:
          result
      })


# ------------------------------------------------------------------------------


setMethod("rowCumsums", "timeSeries",
          function(x, na.rm = FALSE, ...)
      {   # A function implemented by Diethelm Wuertz and Yohan Chalabi

          # Description:
          #   Computes sample cumulated sums by row for timeSeries objects

          # Arguments:

          # FUNCTION:

          # Cumulative Sums:

          series(x) <- callGeneric(as(x, "matrix"), na.rm, ...)

          # Return Value:
          x
      })

################################################################################

