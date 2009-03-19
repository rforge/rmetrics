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
#  merge,timeSeries          Merges two 'timeSeries' objects

setMethod("merge", c("timeSeries", "timeSeries"),
    function(x, y, ...)
{
    # A function implemented by Diethelm Wuertz and Yohan Chalabi

    if (is.signalSeries(x) | is.signalSeries(y)) {
        data <- merge(getDataPart(x), getDataPart(x))
        return(timeSeries(data = data, units = colnames(data)))
    }

    # Convert to Data Frame:
    df.x <- data.frame(as.numeric(time(x), "sec"), getDataPart(x))
    names(df.x) <- c("positions", colnames(x))
    df.y <- data.frame(as.numeric(time(y), "sec"), getDataPart(y))
    names(df.y) <- c("positions", colnames(y))

    # Merge as Data Frame:
    df <- merge(df.x, df.y, all = TRUE)
    data <- as.matrix(df[,-1])
    units <- names(df)[-1]
    charvec <- as.numeric(df[,1])

    # Return Value:
    timeSeries(data = data, charvec = charvec, units = units,
               zone = "GMT", FinCenter = finCenter(x))
})

# until UseMethod dispatches S4 methods in 'base' functions
merge.timeSeries <- function(x, y, ...) timeSeries::merge(x, y, ...)

# ------------------------------------------------------------------------------

setMethod("merge", c("timeSeries", "ANY"),
          function(x,y, ...) callGeneric(x, as(y, "timeSeries"), ...))
setMethod("merge", c("ANY", "timeSeries"),
          function(x,y, ...) callGeneric(as(x, "timeSeries"), y, ...))
setMethod("merge", c("timeSeries", "missing"), function(x,y, ...) x)

################################################################################
