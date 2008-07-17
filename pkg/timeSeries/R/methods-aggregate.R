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
#  aggregate                 Aggregates a 'timeSeries' object
################################################################################


setMethod("aggregate",
      "timeSeries",
      function(x, by, FUN, ...)
{   # A function implemented by Yohan Chalabi and  Diethelm Wuertz

    # Description:
    #   Aggregates a 'timeSeries' object

    # Details:
    #   This function can be used to aggregate and coursen a
    #   'timeSeries' object.

    # Arguments:
    #   x - a 'timeSeries' object to be aggregated
    #   by - calendarical block, only active when both 'from'
    #       and 'to' are NULL
    #   FUN - function to be applied, by default 'colMeans'
    #   units - a character vector with column names, allows to
    #       overwrite the column names of the input 'timeSeries'
    #       object.

    # Value:
    #   Returns a S4 object of class 'timeSeries'.

    # Examples:
    #  ts = timeSeries(charvec = timeCalendar(2008))
    #  by = timeSequence(from = "2008-04-01",  to = "2009-01-01", by = "quarter")
    #  aggregate(ts, by, mean)
    #
    #  dates = timeSequence(from = "2008-01-01",  to = "2008-02-01", by = "day")
    #  ts = timeSeries(matrix(rnorm(2*length(dates)), ncol = 2), charvec = dates)
    #  by = timeSequence(from = "2008-01-01",  to = "2008-02-01", by = "week")
    #  aggregate(ts, by, function(x) log(abs(sum(x))))

    # FUNCTION:

    # Check object:
    stopifnot(class(time(x)) == class(by))

    x <- sort(x)

    INDEX <- sapply(time(x), function(x) which(x <= by)[1])
    data <- apply(getDataPart(x), 2, tapply, INDEX, FUN)

    # Return Value:
    timeSeries(data, charvec = by[unique(na.omit(INDEX))], ...)
})


################################################################################

