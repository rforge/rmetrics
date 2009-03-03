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


setMethod("aggregate", "timeSeries",
          function(x, by, FUN, ...)
      {
          # A function implemented by Yohan Chalabi and  Diethelm Wuertz

          # Description:
          #   Aggregates a 'timeSeries' object

          # Details:
          #   This function can be used to aggregate and coursen a
          #   'timeSeries' object.

          # Arguments:
          #   x - a 'timeSeries' object to be aggregated
          #   by - calendarical block
          #   FUN - function to be applied, by default 'colMeans'
          #   ... - additional argument to be passed to the newly generated
          #         timeSeries object

          # Value:
          #   Returns a S4 object of class 'timeSeries'.

          # Examples:
          # Quarterly Aggregation:
          ### m = matrix(rep(1:12,2), ncol = 2)
          ### ts = timeSeries(m, timeCalendar())
          ### Y = getRmetricsOptions("currentYear"); Y
          ### from = paste(Y, "04-01", sep = "-"); to = paste(Y+1, "01-01", sep = "-")
          ### by = timeSequence(from, to, by = "quarter") - 24*3600; by
          ### ts; aggregate(ts, by, sum)
          ### # Weekly Aggregation:
          ### dates = timeSequence(from = "2009-01-01", to = "2009-02-01", by = "day")
          ### data = 10 * round(matrix(rnorm(2*length(dates)), ncol = 2), 1); data
          ### ts = timeSeries(data = data, charvec = dates)
          ### by = timeSequence(from = "2009-01-08",  to = "2009-02-01", by = "week")
          ### by = by - 24*3600; aggregate(ts, by, sum)

          # FUNCTION:

          # Check args
          if (!((inherits(by, "timeDate") && x@format != "counts") ||
                (is.numeric(by) && x@format == "counts")))
              stop("'by' should be of the same class as 'time(x)'", call.=FALSE)

          # make sure that x is sorted
          if (is.unsorted(x))
              x <- sort(x)

          # sort and remove double entries in by
          by <- unique(sort(by))

          INDEX <- findInterval(x@positions, as.numeric(by, "sec") + 1)
          INDEX <- INDEX + 1
          is.na(INDEX) <- !(INDEX <= length(by))

          # YC : ncol important to avoid problems of dimension dropped by apply
          data <- matrix(apply(getDataPart(x), 2, tapply, INDEX, FUN), ncol=ncol(x))

          rownames(data) <- as.character(by[unique(na.omit(INDEX))])
          colnames(x) <- colnames(x)

          # Return Value:
          timeSeries(data, ...)
      })

################################################################################

