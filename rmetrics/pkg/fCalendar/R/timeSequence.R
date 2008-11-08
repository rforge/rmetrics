
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
    function(from, to = format(Sys.time(), "%Y-%m-%d"),
    by = c("day", "year", "quarter", "month", "week", "hour", "min", "sec"),
    length.out = NULL, format = NULL,
    zone = myFinCenter, FinCenter = myFinCenter)
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

    # Check Time Zone:
    TZ <- Sys.getenv("TZ")
    if(TZ[[1]] != "GMT") {
        Sys.setenv(TZ = "GMT")
        on.exit(Sys.setenv(TZ = TZ))
    }
    
    # Missing from:
    if (missing(from)) from = timeDate(to, format = format, zone = zone,
        FinCenter = FinCenter) - 24*29*3600

    # Settings and Checks:
    if (!is.null(length.out)) to = from
    if (FinCenter == "") FinCenter = "GMT"
    by = match.arg(by)
    if (by == "quarter") by = "3 months"

    # Auto-detect Input Format:
    format.from = format.to = format
    if (is.null(format)) {
        format.from = whichFormat(as.character(from))
        format.to = whichFormat(as.character(to))
    }
    if (format.from == format.to) {
        format = format.from
    } else {
        stop ("Args from and to must have the same format specification.")
    }

    # Create Charvector:
    from = strptime(as.character(from), format = format)
    if (is.null(length.out)) {
        # The start "from" and end date "to" must be specified!
        to = strptime(as.character(to), format = format)
        charvec = format(seq.POSIXt(from = from,
            to = to, by = by), format)
    } else  {
        # The end date is missing and has to be specified
        charvec = format(seq.POSIXt(from = from, by = by,
            length.out = length.out))
    }

    # Return timeDate Object:
    timeDate(charvec = charvec, format = whichFormat(charvec[1]),
        zone = zone, FinCenter = FinCenter)
}


# ------------------------------------------------------------------------------


seq.timeDate <- 
    function(from, to,
    by = c("day", "year", "quarter", "month", "week", "hour", "min", "sec"),
    length.out = NULL, ...)
{   
    # A function implemented by Diethelm Wuertz

    # Description:
    #   A synonyme function for timeSequence.

    # Arguments:
    #   from, to - two 'timeDate' objects

    # Changes:
    #

    # FUNCTION:

    # Check:
    stopifnot(class(from) == "timeDate")
    stopifnot(class(to) == "timeDate")

    # Sequence:
    by = match.arg(by)
    to = timeDate(to, zone = to@FinCenter, FinCenter = from@FinCenter)
    ans = timeSequence(from = from, to = to, by = by, length.out = length.out,
        format = NULL, zone = from@FinCenter, FinCenter = from@FinCenter)

    # Return Value:
    ans
}


################################################################################

