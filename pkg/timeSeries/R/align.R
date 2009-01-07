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
#  .align.timeSeries.old     aligns, old version
################################################################################


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
    
    ans = .align.timeSeries <-
        function(x, by = "1d", offset = "0s",
        method = c("before", "after", "interp", "fillNA"),
        include.weekends = FALSE)

    # Return Value:
    ans
})


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


.align.timeSeries.old =
    function(x, method = c("before", "after", "interp"), startOn = "hours",
        by = "30 m")
{   
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Aligns a 'timeSeries' object

    # FUNCTION:

    if (x@format == "counts")
        stop(as.character(match.call())[1], 
            " is for time series and not for signal series.")

    # Settings:
    method = match.arg(method)
    numberOfColumns = dim(x)[2]
    typeMethod = c(interp = "linear", before = "constant", after = "constant")
    fMethod = c(interp = 0.5, before = 0, after = 1)
    by = .by2seconds(by)

    # Convert to GMT:
    tD = timeDate(x@positions, zone = x@FinCenter, FinCenter = "GMT")

    # Convert to POSIX:
    Positions = as.POSIXct(tD, tz = "GMT")
    N = length(Positions)
    Start = as.POSIXct(trunc(Positions[1], startOn), tz = "GMT")
    End   = as.POSIXct(trunc(Positions[N], startOn), tz = "GMT") + 24*3600
    print(Start)
    print(End)

    # Compute Positions:
    X = as.integer(difftime(Positions, Start, units = "sec"))

    # Compute New Positions:
    timeDiff = as.integer(difftime(End, Start, units = "secs"))
    lengthOut = trunc(timeDiff/by) + 1
    posix = seq(from = Start, by = paste(by, "sec"), length.out = lengthOut)
    newX = as.integer(difftime(posix, Start, units = "secs"))

    # Align:
    matY = NULL
    for (i in 1:numberOfColumns) {
        Y = as.vector(x[, i])
        newY = approx(X, Y, xout = newX, method = typeMethod[method],
            f = fMethod[method])$y
        matY = cbind(matY, newY)

    }

    # Create Series in local time:
    print(head(as.character(posix)))
    ans = timeSeries(matY, as.character(posix),
        units = x@units, zone = "GMT", FinCenter = x@FinCenter,
        recordIDs = data.frame())

    # Return Value:
    ans
}


################################################################################

