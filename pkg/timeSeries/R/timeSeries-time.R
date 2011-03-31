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
#  time,timeSeries           Extracs time positions from a 'timeSeries'
#  time<-                    Defines S3 UseMethod
#  time<-.timeSeries         ... to avoid problems with zoo
# DEPRECATED:               DESCRIPTION:
#  seriesPositions           Deprecated, use time
#  newPositions<-            Deprecated, use time<-
################################################################################


.time.timeSeries <- function(x, ...)
{
    # A function implemented by Diethelm Wuertz and Yohan Chalabi

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
}


setMethod("time", "timeSeries",
          function(x, ...) .time.timeSeries(x, ...))

          
# until UseMethod dispatches S4 methods in 'base' functions
time.timeSeries <- function(x, ...) .time.timeSeries(x, ...)


# ------------------------------------------------------------------------------


`time<-` <-
function(x, value)
{
    UseMethod("time<-")
}


# ------------------------------------------------------------------------------


`time<-.timeSeries` <-
function(x, value)
{
    # A function implemented by Yohan Chalabi
    
    # Note:
    #   to avoid conflict with zoo package

    # FUNCTION:
    
    # Assign Rownames:
    rownames(x) <- value
    
    # Return Value:
    x
}


## setMethod("time<-", "timeSeries", function(x, value)
##       {
##           rownames(x) <- value
##           # Return
##           x
##       })


# #############################################################################


seriesPositions <-
    function(object)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Extracts the positions of a 'timeSeries' objects and
    #   converts them to a 'timeDate' object.

    # Arguments:
    #   object - a 'timeSeries' object

    # Value:
    #   Returns 'timeSeries' positions as 'timeDate' objects.

    # FUNCTION:

    # Deprecated:
    .Deprecated(new = "time", package = "timeSeries")

    # Return Value:
    time(object)
}


# ------------------------------------------------------------------------------


"newPositions<-" <-
    function(object, value)
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:

    # Deprecated:
    .Deprecated(new = "time<-", package = "timeSeries")

    # Assign Rownames:
    rownames(object) <- value

    # Return Value:
    object
}


################################################################################

