
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General
# Public License along with this library; if not, write to the
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston,
# MA  02111-1307  USA

# Copyrights (C)
# for this R-port:
#   1999 - Diethelm Wuertz, GPL
#   2007 - Rmetrics Foundation, GPL
#   Diethelm Wuertz <wuertz@phys.ethz.ch>
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:                 GENERATION OF TIMEDATE OBJECTS:
#  timeSequence              Creates a regularly spaced 'timeDate' object
#  seq                       A synonyme function for timeSequence
################################################################################


timeSequence <-
    function(from, to = Sys.timeDate(),
    by = c("day", "year", "quarter", "month", "week", "hour", "min", "sec"),
    length.out = NULL, format = NULL,
    zone = "", FinCenter = "")
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Creates a regularly spaced 'timeDate' object

    # Arguments:
    #   from - starting date. Required.
    #   to - end date. Optional. If supplied must be after from.
    #   by - a character string, containing one of "sec", "min",
    #       "hour", "day", "week", "month" or "year".
    #       This can optionally be preceded by an integer and a
    #       space, or followed by "s".
    #   length.out - length.out integer, optional. Desired length
    #       of the sequence, if specified "to" will be ignored.
    #   format - the format specification of the input character
    #       vector.
    #   FinCenter - a character string with the the location of the
    #       financial center named as "continent/city".

    # Value:
    #   Returns a 'timeDate' object corresponding to the "sequence"
    #   specification.

    # Note:
    #   The 'zone' where the data were recorded is fixed to myFincenter!

    # Example:
    #   x = timeSequence("2004-01-28", "2004-02-04", by = "day")
    #   x = timeSequence("2004-01-01", "2005-01-31", by = "month")
    #   x = timeSequence("2004-01-28", by = "day", length.out = 10)
    #   x = timeSequence("2004-01-01", by = "month", length.out = 12)
    #   x = timeSequence("2004-01-28 18:00:00", "2004-01-29 05:00:00", by = "hour")
    #   x = timeSequence("2004-01-28 18:00:00", by = "hour", length.out = 12)

    # FUNCTION:
    if (zone == "")
        zone <- getRmetricsOptions("myFinCenter")
    if (FinCenter == "")
        FinCenter <- getRmetricsOptions("myFinCenter")

    to <- timeDate(to, format = format, zone = zone, FinCenter = FinCenter)
    # Missing from:
    from <-
        if (missing(from))
            timeDate(to, format = format, zone = zone, FinCenter = FinCenter) -
                24*29*3600
        else
            timeDate(from, format = format, zone = zone, FinCenter = FinCenter)



    # Settings and Checks:
    if (!is.null(length.out)) to = from
    if (FinCenter == "") FinCenter = "GMT"
    by = match.arg(by)
    if (by == "quarter") by = "3 months"

    ans <-
        if (is.null(length.out))
            seq.timeDate(from = from, to = to, by = by)
        else
            seq.timeDate(from = from, by = by, length.out = length.out)

    # Return timeDate Object:
    ans

}


# ------------------------------------------------------------------------------

seq.timeDate <-
    function(from, to, by, length.out = NULL, ...)
{
    # A function implemented by Diethelm Wuertz and Yohan Chalabi

    # Description:
    #   A synonyme function for timeSequence.

    # Arguments:
    #   from, to - two 'timeDate' objects

    # Example
    #  seq.timeDate(from = Sys.timeDate(), to = Sys.timeDate() + 3 * 24 * 3600)
    #  seq.timeDate(from = Sys.timeDate(FinCenter = "Zurich"), to = Sys.timeDate() + 3 * 24 * 3600, by = "week")

    # FUNCTION:

    # Check:
    stopifnot(inherits(from, "timeDate"))

    if (!missing(to)) {
        stopifnot(inherits(to, "timeDate"))
        to@FinCenter <- from@FinCenter
    }

    if (missing(by)) by <- "day"

    # Sequence:
    seq <-
        if (missing(to))
            seq.POSIXt(from = as.POSIXct(format(from)),
                       by = by,
                       length.out = length.out,
                       ...)
        else
            seq.POSIXt(from = as.POSIXct(format(from)),
                       to = as.POSIXct(format(to)),
                       by = by,
                       length.out = length.out,
                       ...)

    ans <- timeDate(seq, format = NULL,
                    zone = from@FinCenter, FinCenter = from@FinCenter)

    # Return Value:
    ans
}


################################################################################

