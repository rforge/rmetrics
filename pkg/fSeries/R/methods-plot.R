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
#  plot.timeSeries           Plots a 'timeSeries' object
#  points.timeSeries         Adds points to a 'timeSeries' plot
#  lines.timeSeries          Adds lines to a 'timeSeries' plot
################################################################################


setMethod("plot", c("timeSeries","missing"),
          function(x, y, ...)
      {   # A function implemented by Diethelm Wuertz

          # Description:
          #   NEW Plot method for an object of class "timeSeries"

          # Arguments:
          #   x - a "timeSeries" object

          # FUNCTION:

          # Plot:
          positions <- time(x)
          if (x@format == "counts") {
              plot(x = positions, y = x@Data, ...)
          } else {
              plot(x = as.POSIXct(positions), y = x@Data, ...)
          }

          # Return Value:
          invisible(x)
      })


# ------------------------------------------------------------------------------


setMethod("lines", c("timeSeries"),
          function(x,  ...)
      {   # A function implemented by Diethelm Wuertz

          # Description:
          #   NEW Lines method for an object of class "timeSeries"

          # Arguments:
          #   x - a "timeSeries" object

          # Example:
          #   plot(MSFT[,1]); lines(MSFT[,1], col = "red")

          # FUNCTION:

          # Plot:
          positions <- time(x)
          if (x@format == "counts") {
              lines(x = positions, y = x@Data, ...)
          } else {
              lines(x = as.POSIXct(positions), y = x@Data, ...)
          }

          # Return Value:
          invisible(x)
      })


# ------------------------------------------------------------------------------


setMethod("points", "timeSeries",
          function(x, ...)
      {   # A function implemented by Diethelm Wuertz

          # Description:
          #   Plot method for an object of class "timeSeries"

          # Arguments:
          #   x - a "timeSeries" object

          # Value:
          #   Plots a 'timeSeries' object.

          # FUNCTION:

          # Add to Plot:
          positions <- time(x)
          if (x@format == "counts") {
              points(x = positions, y = x@Data, ...)
          } else {
              points(x = as.POSIXct(positions), y = x@Data, ...)
          }

          # Return Value:
          invisible(x)
      })


################################################################################

