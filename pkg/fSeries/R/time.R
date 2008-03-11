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
    ans = timeDate(charvec = object@positions, format = object@format,
        zone = object@FinCenter, FinCenter = object@FinCenter)

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

"time<-" <-
    function(object, value)
{
    stopifnot(is.timeSeries(object))

    # FUNCTION:
    ans = timeSeries(object, value)

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


time.timeSeries =
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
    timeDate(charvec = x@positions, format = x@format,
        zone = x@FinCenter, FinCenter = x@FinCenter)
}



# ------------------------------------------------------------------------------

.sample.timeSeries =
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
    nPOS = length(x@positions)
    index = sample(1:nPOS)
    x = x[index, ]

    # recordIDs:
    DF = x@recordIDs
    DIM = dim(DF)
    if (sum(DIM) > 0) {
        df = rev(DF[, 1])
        if (DIM[2] > 1)
            for (i in 2:DIM[2]) df = data.frame(df, rev(DF[index, i]))
        colnames(df) <- colnames(DF)
        rownames(df) <- x@positions
        x@recordIDs = df
    }

    # Return Value:
    x
}


sample.timeSeries =
function (x, ...)
{
    # Index:
    Index = sample(1:length(x@positions))

    # Compose Series:
    x@positions = x@positions[Index]
    x@Data = as.matrix(x@Data[Index, ])
    colnames(x@Data) = x@units
    x@recordIDs = as.data.frame(x@recordIDs[Index, ])

    # Return value:
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
    index <- order(POS)
    x <- x[index, ]

    ### Version of DW
    ###     # recordIDs:
    ###     DF <- x@recordIDs
    ###     DIM <- dim(DF)
    ###     if (sum(DIM) > 0) {
    ###         df <- rev(DF[index, 1])
    ###         if (DIM[2] > 1)
    ###             for (i in 2:DIM[2]) df = data.frame(df, rev(DF[index, i]))
    ###         colnames(df) <- colnames(DF)
    ###         rownames(df) <- x@positions
    ###         x@recordIDs = df
    ###     }

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

    ### Version of DW
    ###     # IDs:
    ###     DF = x@recordIDs
    ###     DIM = dim(DF)
    ###     if (sum(DIM) > 0) {
    ###         df = rev(DF[, 1])
    ###         if (DIM[2] > 1)
    ###             for (i in 2:DIM[2]) df = data.frame(df, rev(DF[, i]))
    ###         colnames(df) <- colnames(DF)
    ###         rownames(df) <- x@positions
    ###         x@recordIDs = df
    ###     }

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
    ans = start.timeDate(time(x))

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
    ans = end.timeDate(time(x))

    # Return Value:
    ans
}


################################################################################

