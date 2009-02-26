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
# FUNCTION:                 POSITIONS:
#  seriesPositions           Extracts positions slot from 'timeSeries' object
#  newPositions<-            Modifies positions of a 'timeSeries' object
#  time<-                    Modifies positions of a 'timeSeries' object
# METHOD:                   POSITION HANDLING:
#  time,timeSeries           extracs time positions from a 'timeSeries'
#  sample,timeSeries         Resamples a 'timeSeries' object in time
#  sort,timeSeries           Sorts reverts a 'timeSeries' object in time
#  rev,timeSeries            Reverts a 'timeSeries' object in time
#  start,timeSeries          Extracts start date of a 'timeSeries' object
#  end,timeSeries            Extracts end date of a 'timeSeries' object
################################################################################

seriesPositions =
function(object)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Extracts the positions of a 'timeSeries' objects and
    #   converts them to a 'timeDate' object.

    # Arguments:
    #   object - a 'timeSeries' object

    # Value:
    #   Returns 'timeSeries' positions as 'timeDate' objects.

    # FUNCTION:

    # .Deprecated("time", package = "timeSeries")

    time(object)
}

# ------------------------------------------------------------------------------

"newPositions<-" =
function(object, value)
{   # A function implemented by Diethelm Wuertz

    # .Deprecated("time<-", "timeSeries")

    # FUNCTION:
    rownames(object) <- value

    # Return Value:
    object
}

# ------------------------------------------------------------------------------

## if (!exists("time<-", mode = "function"))
"time<-" <- function(x, value) UseMethod("time<-")

# ------------------------------------------------------------------------------
"time<-.timeSeries" <-
    function(x, value)
{
    stopifnot(is.timeSeries(x))

    rownames(x) <- value

    # Return
    x
}

################################################################################
# METHOD:                   POSITION HANDLING:
#  time,timeSeries           Extracs time positions from a 'timeSeries'
#  sample,timeSeries         Resamples a 'timeSeries' object in time
#  sort,timeSeries           Sorts reverts a 'timeSeries' object in time
#  rev,timeSeries            Reverts a 'timeSeries' object in time
#  start,timeSeries          Extracts start date of a 'timeSeries' object
#  end,timeSeries            Extracts end date of a 'timeSeries' object

setMethod("time" ,
          "timeSeries",
          function(x, ...)
      {   # A function implemented by Diethelm Wuertz and Yohan Chalabi

          # Description:
          #   Extracs time positions from a 'timeSeries'

          # Arguments:
          #   x - a 'timeSeries' object.

          # Value:
          #   Returns a time resampled object of class 'timeSeries'.

          # FUNCTION:

          if (length(x@positions)>0)
              timeDate(x@positions, zone = "GMT",
                       FinCenter = x@FinCenter)
          else
              seq.int(NROW(x))
      })

# ------------------------------------------------------------------------------

setMethod("sample" , "timeSeries",
          function(x, size, replace = FALSE, prob = NULL)
          x[sample(seq(NROW(x)), size, replace, prob), ])

# ------------------------------------------------------------------------------

setMethod("sort",
          "timeSeries",
          function (x, decreasing = FALSE, ...)
{
    # A function implemented by Diethelm Wuertz and Yohan Chalabi

    # Description:
    #   Time sorts a 'timeSeries' object

    # Arguments:
    #   x - a 'timeSeries' object.

    # Value:
    #   Returns a time sorted object of class 'timeSeries'.

    # FUNCTION:

    if (length(x@positions)>0)
        x[order(x@positions, decreasing = decreasing), ]
    else
        x
})

# ------------------------------------------------------------------------------

setMethod("rev", "timeSeries", function(x) x[NROW(x):1,])

# ------------------------------------------------------------------------------

setMethod("start" , "timeSeries", function(x, ...)
{
    if (length(x@positions)>0)
        timeDate(min(x@positions), zone = x@FinCenter, FinCenter = x@FinCenter)
    else
        NULL
})

# ------------------------------------------------------------------------------

setMethod("end" , "timeSeries", function(x, ...)
    {
    if (length(x@positions)>0)
        timeDate(max(x@positions), zone = x@FinCenter, FinCenter = x@FinCenter)
    else
        NULL
})

################################################################################

