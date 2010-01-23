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
#  align                     Aligns a 'timeSeries object' to time stamps
#  .align.timeSeries         Aligns a 'timeSeries object' to time stamps
################################################################################


# DW: See also ...
#   in package timeDate
#   setMethod("align", "ANY",
#   setMethod("align", "timeDate",


# ------------------------------------------------------------------------------


setMethod("align", "timeSeries",
    function(x, by = "1d", offset = "0s",
        method = c("before", "after", "interp", "fillNA"),
        include.weekends = FALSE)
    {
        # Description:
        #   Aligns a 'timeSeries' object to equidistant time stamps
    
        # Example:
        #   data(usdthb)
        #   data = matrix(usdthb[, "BID"])
        #   charvec = as.character(usdthb[, "XDATE"])
        #   USDTHB = timeSeries(data, charvec, format = "%Y%M%d%H%M")
        #   align(USDTHB, by = "3h", offset = "92m")
        #   MSFT = as.timeSeries(data(msft.dat))
        #   align(MSFT)
    
        # See also:
        #   in package timeDate
        #   setMethod("align", "ANY",
        #   setMethod("align", "timeDate",
    
        # FUNCTION:
    
        # Settings:
        periods = c(7*24*3600, 24*3600, 3600, 60, 1)
        names(periods) = c("w", "d", "h", "m", "s")
        offset = as.integer(gsub("[a-z]", "", offset, perl = TRUE)) *
            periods[gsub("[ 0-9]", "", offset, perl = TRUE)]
        by = as.integer(gsub("[a-z]", "", by, perl = TRUE)) *
            periods[gsub("[ 0-9]", "", by, perl = TRUE)]
    
        # left-right adjustment:
        Method = match.arg(method)
        method = "linear"
        f = 0.5
        if (Method == "interp") {
            method = "linear"
            f = 0.5 }
        if (Method == "before") {
            method = "constant"
            f = 0 }
        if (Method == "after") {
            method = "constant"
            f = 1 }
    
        # Convert timeDate to GMT-POSIX
        posixGMT = as.POSIXct(
            timeDate(time(x), zone = x@FinCenter, FinCenter = "GMT"), tz = "GMT")
    
        # Compute Julian counts (x) and series values (y)
        Origin = as.POSIXct("1970-01-01", tz = "GMT")
        u <- as.integer(difftime(posixGMT, Origin, tz = "GMT", units = "secs"))
        xout = seq(u[1] + offset, u[length(u)], by = by)
        posixGMT = Origin + as.integer(xout)
    
        N = NCOL(x)
        for (i in 1:N) {
            v = as.vector(series(x[, i]))
    
            # New Positions and approximated values:
            yout = approx(u, v, xout, method = method, f = f)$y
            if (Method == "fillNA") yout[!(x %in% u)] = NA
    
            # Compose Time Series:
            tS = timeSeries(yout, posixGMT, zone = "GMT", FinCenter = x@FinCenter)
            if (i == 1) ans = tS else ans = cbind(ans, tS)
        }
    
        # Remove Weekends:
        if(!include.weekends) ans = ans[isWeekday(time(ans)), ]
        colnames(ans) <- colnames(x)
    
        # Return Value:
        ans
    }
)


################################################################################


.align.timeSeries <-
function(x, by = "1d", offset = "0s",
    method = c("before", "after", "interp", "fillNA"),
    include.weekends = FALSE)
{
    # Description:
    #   Aligns a 'timeSeries' object to equidistant time stamps

    # Example:
    #   data(usdthb)
    #   data = matrix(usdthb[, "BID"])
    #   charvec = as.character(usdthb[, "XDATE"])
    #   USDTHB = timeSeries(data, charvec, format = "%Y%M%d%H%M")
    #   align(USDTHB, by = "3h", offset = "92m")
    #   MSFT = as.timeSeries(data(msft.dat))
    #   align(MSFT)

    # FUNCTION:

    # Not for signal Series:
    stopifnot(!(x@format == "counts"))

    # Settings:
    periods = c(7*24*3600, 24*3600, 3600, 60, 1)
    names(periods) = c("w", "d", "h", "m", "s")
    offset = as.integer(gsub("[a-z]", "", offset, perl = TRUE)) *
        periods[gsub("[ 0-9]", "", offset, perl = TRUE)]
    by = as.integer(gsub("[a-z]", "", by, perl = TRUE)) *
        periods[gsub("[ 0-9]", "", by, perl = TRUE)]

    # left-right adjustment:
    Method = match.arg(method)
    method = "linear"
    f = 0.5
    if (Method == "interp") {
        method = "linear"
        f = 0.5 }
    if (Method == "before") {
        method = "constant"
        f = 0 }
    if (Method == "after") {
        method = "constant"
        f = 1 }

    # Convert timeDate to GMT-POSIX
    posixGMT = as.POSIXct(
        timeDate(time(x), zone = x@FinCenter, FinCenter = "GMT"), tz = "GMT")

    # Compute Julian counts (x) and series values (y)
    Origin = as.POSIXct("1970-01-01", tz = "GMT")
    u <- as.integer(difftime(posixGMT, Origin, tz = "GMT", units = "secs"))
    xout = seq(u[1] + offset, u[length(u)], by = by)
    posixGMT = Origin + as.integer(xout)

    N = NCOL(x)
    for (i in 1:N) {
        v = as.vector(series(x[, i]))

        # New Positions and approximated values:
        yout = approx(u, v, xout, method = method, f = f)$y
        if (Method == "fillNA") yout[!(x %in% u)] = NA

        # Compose Time Series:
        tS = timeSeries(yout, posixGMT, zone = "GMT", FinCenter = x@FinCenter)
        if (i == 1) ans = tS else ans = cbind(ans, tS)
    }

    # Remove Weekends:
    if(!include.weekends) ans = ans[isWeekday(time(ans)), ]
    colnames(ans) <- colnames(x)

    # Return Value:
    ans
}


################################################################################

