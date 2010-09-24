
# This R package is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This R package is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General
# Public License along with this R package; if not, write to the
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston,
# MA  02111-1307  USA


################################################################################
# FUNCTION:                 GENERATION OF TIMEDATE OBJECTS:
#  timeSequence              Creates a regularly spaced 'timeDate' object
#  seq.timeDate              A synonyme function for timeSequence
################################################################################

timeSequence <-
    function(from, to = Sys.timeDate(), by, length.out = NULL, format = NULL,
    zone = "", FinCenter = "")
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Creates a regularly spaced 'timeDate' object

    # Arguments:
    #   from - starting date.
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

    # Missing from:
    if (missing(from))
        from <- timeDate(to, format = format, zone = zone,
                         FinCenter = FinCenter) - 24*29*3600

    # Settings and Checks:
    if (!is.null(length.out)) to <- from
    if (missing(by)) by <- "day"

    # Auto-detect Input Format:
    format.from <- format.to <- format
    if (is.null(format)) {
        format.from <- whichFormat(as.character(from))
        format.to <- whichFormat(as.character(to))
        from <- timeDate(from, format = format.from, zone = zone,
                         FinCenter = FinCenter)
        to <- timeDate(to, format = format.to, zone = zone,
                       FinCenter = FinCenter)
    } else {
        from <- timeDate(from, format = format, zone = zone,
                         FinCenter = FinCenter)
        to <- timeDate(to, format = format, zone = zone,
                       FinCenter = FinCenter)
    }

    tseq <-
        if (length(length.out))
            seq(from = from,  by = by, length.out = length.out)
        else
            seq(from = from, to = to, by = by)
    tseq
}

# ------------------------------------------------------------------------------

seq.timeDate <-
    function (from, to, by, length.out = NULL, along.with = NULL,  ...)
{
    # A function implemented by Diethelm Wuertz and Yohan Chalabi

    # FUNCTION:

    # This function is the same as seq.POSIXt form the base package.

    # Modifications by Yohan Chalabi marked with ##

    if (missing(from))
        stop("'from' must be specified")
    if (!inherits(from, "timeDate")) ##
        stop("'from' must be a timeDate object") ##

    if (!missing(by) && is.character(by)) { ##
        if (identical("quarter", by)) by <- "3 months"
        by1 <- strsplit(by, " ", fixed = TRUE)[[1]]
        DST <- !is.na(pmatch(by1[length(by1)], c("months", "years", "DSTdays")))
    } else {
        DST <- FALSE
    }

    FinCenter <- from@FinCenter ##
    zone <- if (DST) from@FinCenter else "GMT" ##

    from <- ##
        if (DST)
            as.POSIXct(format(from), tz = "GMT")
        else
            as.POSIXct(from)

    cfrom <- as.POSIXct(from)
    if (length(cfrom) != 1)
        stop("'from' must be of length 1")
    tz <- "GMT" ##
    if (!missing(to)) {
        if (!inherits(to, "timeDate")) ##
            stop("'to' must be a timeDate object") ##
        if (length(to) != 1)
            stop("'to' must be of length 1")
        to <- ##
            if (DST)
                as.POSIXct(format(to), tz = "GMT")
            else
                as.POSIXct(to)
    }

    if (!missing(along.with)) {
        length.out <- length(along.with)
    }
    else if (!is.null(length.out)) {
        if (length(length.out) != 1)
            stop("'length.out' must be of length 1")
        length.out <- ceiling(length.out)
    }
    status <- c(!missing(to), !missing(by), !is.null(length.out))
    if (sum(status) != 2)
        stop("exactly two of 'to', 'by' and 'length.out' / 'along.with' must be specified")
    if (missing(by)) {
        from <- unclass(cfrom)
        to <- unclass(as.POSIXct(to))
        res <- seq.int(from, to, length.out = length.out)
        return(timeDate(res, zone = zone, FinCenter = FinCenter)) ##
    }
    if (length(by) != 1)
        stop("'by' must be of length 1")
    valid <- 0
    if (inherits(by, "difftime")) {
        by <- switch(attr(by, "units"), secs = 1, mins = 60,
                     hours = 3600, days = 86400, weeks = 7 * 86400) *
                         unclass(by)
    }
    else if (is.character(by)) {
        by2 <- strsplit(by, " ", fixed = TRUE)[[1]]
        if (length(by2) > 2 || length(by2) < 1)
            stop("invalid 'by' string")
        valid <- pmatch(by2[length(by2)], c("secs", "mins", "hours",
            "days", "weeks", "months", "years", "DSTdays"))
        if (is.na(valid))
            stop("invalid string for 'by'")
        if (valid <= 5) {
            by <- c(1, 60, 3600, 86400, 7 * 86400)[valid]
            if (length(by2) == 2)
                by <- by * as.integer(by2[1])
        }
        else by <- if (length(by2) == 2)
            as.integer(by2[1])
        else 1
    }
    else if (!is.numeric(by))
        stop("invalid mode for 'by'")
    if (is.na(by))
        stop("'by' is NA")
    if (valid <= 5) {
        from <- unclass(as.POSIXct(from))
        if (!is.null(length.out))
            res <- seq.int(from, by = by, length.out = length.out)
        else {
            to <- unclass(as.POSIXct(to))
            res <- seq.int(0, to - from, by) + from
        }
        return(timeDate(res, zone = zone, FinCenter = FinCenter)) ##
    }
    else {
        r1 <- as.POSIXlt(from)
        if (valid == 7) {
            if (missing(to)) {
                yr <- seq.int(r1$year, by = by, length.out = length.out)
            }
            else {
                to <- as.POSIXlt(to)
                yr <- seq.int(r1$year, to$year, by)
            }
            r1$year <- yr
            r1$isdst <- -1
            res <- as.POSIXct(r1)
        }
        else if (valid == 6) {
            if (missing(to)) {
                mon <- seq.int(r1$mon, by = by, length.out = length.out)
            }
            else {
                to <- as.POSIXlt(to)
                mon <- seq.int(r1$mon, 12 * (to$year - r1$year) +
                               to$mon, by)
            }
            r1$mon <- mon
            r1$isdst <- -1
            res <- as.POSIXct(r1)
        }
        else if (valid == 8) {
            if (!missing(to)) {
                length.out <- 2 + floor((unclass(as.POSIXct(to)) -
                                         unclass(as.POSIXct(from)))/86400)
            }
            r1$mday <- seq.int(r1$mday, by = by, length.out = length.out)
            r1$isdst <- -1
            res <- as.POSIXct(r1)
            if (!missing(to))
                res <- res[res <= as.POSIXct(to)]
        }
        return(timeDate(res, zone = zone, FinCenter = FinCenter)) ##
    }
}


################################################################################

