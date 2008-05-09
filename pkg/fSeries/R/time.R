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
#  time.timeSeries           extracs time positions from a 'timeSeries'
#  index.timeSeries          extracs index positions from a 'timeSeries'
#  index<-.timeSeries        modifies index positions from a 'timeSeries'
#  sample.timeSeries         Resamples a 'timeSeries' object in time
#  sort.timeSeries           Sorts reverts a 'timeSeries' object in time
#  rev.timeSeries            Reverts a 'timeSeries' object in time
#  start.timeSeries          Extracts start date of a 'timeSeries' object
#  end.timeSeries            Extracts end date of a 'timeSeries' object
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

    .Deprecated("time", package = "fSeries")

    # Create 'timeDate' Object:
    ans <-
        if (object@format == "counts") {
            as.integer(object@positions)
        } else {
            timeDate(charvec = object@positions, format = object@format,
                     zone = object@FinCenter, FinCenter = object@FinCenter)
        }

    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


"newPositions<-" =
function(object, value)
{   # A function implemented by Diethelm Wuertz

    .Deprecated("time<-", "fSeries")

    # FUNCTION:
    ans = timeSeries(object, value)

    # Return Value:
    ans
}

# ------------------------------------------------------------------------------

## if (!exists("time<-", mode = "function"))
"time<-" <- function(x, value) UseMethod("time<-")

# ------------------------------------------------------------------------------
"time<-.timeSeries" <-
    function(x, value)
{
    stopifnot(is.timeSeries(x))

    # FUNCTION:
    ans = timeSeries(x, value)

    # Return Value:
    ans

}

################################################################################
# METHOD:                   POSITION HANDLING:
#  time.timeSeries           Extracs time positions from a 'timeSeries'
#  sample.timeSeries         Resamples a 'timeSeries' object in time
#  sort.timeSeries           Sorts reverts a 'timeSeries' object in time
#  rev.timeSeries            Reverts a 'timeSeries' object in time
#  start.timeSeries          Extracts start date of a 'timeSeries' object
#  end.timeSeries            Extracts end date of a 'timeSeries' object


time.timeSeries <-
    function(x, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Extracs time positions from a 'timeSeries'

    # Arguments:
    #   x - a 'timeSeries' object.

    # Value:
    #   Returns a time resampled object of class 'timeSeries'.

    # FUNCTION:

    # Get Positions:
    ans <-
        if (x@format == "counts") {
            as.integer(x@positions)
        } else {
            timeDate(charvec = x@positions, format = x@format,
                     zone = x@FinCenter, FinCenter = x@FinCenter)
        }

    # Return Value:
    ans
}

# ------------------------------------------------------------------------------

# temporary fix until we have a name space and avoid problems with
# function index in package "zoo"

## if (!exists("index", mode = "function"))
##     index <- function(x, ...) UseMethod("index")

## index.timeSeries <- function(x, ...) time.timeSeries(x, ...)

## if (!exists("index<-", mode = "function"))
##     "index<-" <- function(x, value) UseMethod("index<-")

## "index<-.timeSeries" <- function(x, value) time.timeSeries(x, value)

## setMethod("index", "timeSeries",
##           function(x, ...) time.timeSeries(x, ...))
## setMethod("index<-", "timeSeries",
##           function(x,value) "time<-.timeSeries"(x, value))

# ------------------------------------------------------------------------------

sample.timeSeries =
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Time sorts a 'timeSeries' object

    # Arguments:
    #   x - a 'timeSeries' object.

    # Value:
    #   Returns a time resampled object of class 'timeSeries'.

    # FUNCTION:

    # Data:
    nPOS = length(time(x))
    index = sample(1:nPOS)
    x = x[index, ]

    # Return Value:
    x
}


# ------------------------------------------------------------------------------


sort.timeSeries <-
    function(x, ...)
{
    # A function implemented by Diethelm Wuertz and Yohan Chalabi

    # Description:
    #   Time sorts a 'timeSeries' object

    # Arguments:
    #   x - a 'timeSeries' object.

    # Value:
    #   Returns a time sorted object of class 'timeSeries'.

    # FUNCTION:

    # Data:
    POS <- x@positions
    # YC: as.integer is important because order(c(" 1", " 2", "10"))
    # YC: gives different results on Windows and Linux !
    index <- if (x@format == "counts") order(as.integer(POS)) else order(POS)
    x <- x[index, ]

    # Return Value:
    x
}

# ------------------------------------------------------------------------------

rev.timeSeries =
function(x)
{   # A function implemented by Diethelm Wuertz and Yohan Chalabi

    # Description:
    #   Time reverts a 'timeSeries' object

    # Arguments:
    #   x - a 'timeSeries' object.

    # Value:
    #   Returns a time reverted object of class 'timeSeries'.

    # FUNCTION:

    # Data:
    nPOS = length(x@positions)
    index = nPOS:1
    x = x[index, ]

    # Return Value:
    x
}


# ------------------------------------------------------------------------------


start.timeSeries =
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Extracts the first position as a character string

    # Arguments:
    #   x - a 'timeSeries' object.
    #   ... - arguments passed to other methods.

    # Value:
    #   Returns the first time/date as an object of class 'timeDate'.

    # FUNCTION:

    # S3 Method:
    POS <- time(x)
    ans <- sort(POS)[1]

    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


end.timeSeries =
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Extracts the last position as a character string

    # Arguments:
    #   x - a 'timeSeries' object.
    #   ... - arguments passed to other methods.

    # Value:
    #   Returns the last time/date as an object of class 'timeDate'.

    # FUNCTION:

    # S3 Method:
    POS <- time(x)
    ans <- sort(POS)[length(POS)]

    # Return Value:
    ans
}


################################################################################

