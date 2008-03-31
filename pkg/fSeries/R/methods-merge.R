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

setMethod("merge", c("timeSeries", "timeSeries"),
    function(x, y, ...)
{
    # A function implemented by Diethelm Wuertz and Yohan Chalabi

    test = as.integer((x@format == "counts") + (y@format == "counts"))
    switch(as.character(test),
           # convert series y to FinCenter of series x
           "0" = { FinCenter <- finCenter(y) <- finCenter(x) },
           # if one of the two series are signal series, the other
           # series is converted to a signal series
           "1" = { x <- timeSeries(x, format = "counts");
                       y <- timeSeries(y, format = "counts") })

    # check if x and y have same date format,
    # if not convert to the most extended one
    if (y@format != x@format) {
        if (nchar(y@format) > nchar(x@format)) {
            x@positions <- format(time(x), format = y@format)
            rownames(x@Data) <- x@positions
            x@format <- y@format
        } else {
            y@positions <- format(time(y), format = x@format)
            rownames(y@Data) <- y@positions
                  y@format <- x@format
        }
    }

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
    ans <- new("timeSeries", data = data, charvec = as.character(df[,1]),
               zone = finCenter(x), FinCenter = finCenter(x), ...)
    ans <- sort(ans)

    # Return Value:
    ans
})


# ------------------------------------------------------------------------------

setMethod("merge", c("timeSeries", "ANY"),
          function(x,y, ...) callGeneric(x, as(y, "timeSeries"), ...))
setMethod("merge", c("ANY", "timeSeries"),
          function(x,y, ...) callGeneric(as(x, "timeSeries"), y, ...))
setMethod("merge", c("timeSeries", "missing"), function(x,y, ...) x)

################################################################################

