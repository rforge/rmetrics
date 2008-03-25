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
# FUNCTION:                 DESCRIPTION:
#  seriesData                Extracts data slot from 'timeSeries' object
#  series                    Extracts data slot from 'timeSeries' object
#  series<-                  Assign new data slot for 'timeSeries' object
################################################################################

seriesData <-
    function(object)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #    Returns the series Data of an ordered data object.

    # Arguments:
    #   object - a 'timeSeries' object

    # Value:
    #    Returns an object of class 'matrix'.

    # FUNCTION:

    # Test:
    if (class(object) != "timeSeries")
        stop("Object is not a time Series")

    .Deprecated("series", "fSeries")

    # Get Data Slot:
    ans = object@Data

    # Return Value:
    ans
}

# ------------------------------------------------------------------------------

setMethod("series", "timeSeries",
          function(x)
      {   # A function implemented by Diethelm Wuertz and Yohan Chalabi

          # Description:
          #    Returns the series Data of an ordered data object.

          # Arguments:
          #   object - a 'timeSeries' object

          # Value:
          #    Returns an object of class 'matrix'.

          # FUNCTION:


          # Get Data Slot:
          ans <- x@Data


          # Return Value:
          ans
      })

# ------------------------------------------------------------------------------

setMethod("series<-", "timeSeries",
          function(x, value)
      {
          # A function implemented by Diethelm Wuertz and Yohan Chalabi

          # Description:
          #    Assign the series Data to a timeSeries object.

          # Arguments:
          #   object - a 'timeSeries' object

          # Value:
          #    Assign to be assign as series Data of a timeSeries.

          # FUNCTION:
          x@Data <- value
          x
      })

################################################################################



