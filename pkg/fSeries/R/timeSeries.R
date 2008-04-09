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
#  timeSeries                Creates a 'timeSeries' object from scratch
################################################################################


timeSeries <-
    function (data, charvec, units = NULL, format = NULL, zone = myFinCenter,
              FinCenter = myFinCenter, recordIDs = data.frame(), title = NULL,
              documentation = NULL, ...)
{

    if (missing(data)) data <- numeric()
    if (missing(charvec)) charvec <- NULL

    ans <- new("timeSeries", data, charvec, units, format, zone,
               FinCenter, recordIDs, title, documentation, ...)

    # YC: sort import for an ordered data set
    sort(ans)
}
