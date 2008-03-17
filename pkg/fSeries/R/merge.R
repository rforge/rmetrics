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
#  merge.timeSeries          Merges two 'timeSeries' objects

merge.timeSeries <-
    function(x, y, units = NULL, ...)
{
    # A function implemented by Diethelm Wuertz and Yohan Chalabi

    # What to merge ?
    test = as.integer((x@format == "counts") + (y@format == "counts"))
    if(test == 1)
        stop("You cannot merge a signal and time Series")

    if (test == 0) FinCenter <- finCenter(y) <- finCenter(x)

    # Convert to Data Frame:
    df.x = data.frame(positions = x@positions, x@Data)
    rownames(df.x) = 1:length(x@positions)
    df.y = data.frame(positions = y@positions, y@Data)

    rownames(df.y) = length(x@positions) + (1:length(y@positions))

    # Merge as Data Frame:
    df = merge(df.x, df.y, all = TRUE)
    data = matrix(as.numeric(as.matrix(df[, -1])), ncol = NCOL(df)-1)
    colnames(data) = colnames(df)[-1]
    rownames(data) = format(df[,1])

    # Compose and sort the timeSeries:
    if (test == 0) {
        # Time Series Case:
        ans = sort(
            .timeSeries(data = data, charvec = as.character(df[,1]),
            format = NULL, zone = FinCenter, FinCenter = FinCenter))
    } else {
        # SignalSeries Case:
        ans = sort(
            .signalSeries(data = data, charvec = as.character(df[,1])))
    }

    # Return Value:
    ans
}


