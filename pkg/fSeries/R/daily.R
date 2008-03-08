
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
#   1999 - 2007, Diethelm Wuertz, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:                 FOR DAILY OPERATIONS:
#  dummyDailySeries          Creates a dummy daily 'timeSeries' object
#  alignDailySeries          Aligns a 'timeSeries' object to new positions
#  rollDailySeries           Rolls daily a 'timeSeries' on a given period
#  ohlcDailyPlot             Plots open high low close bar chart
################################################################################


dummyDailySeries <-
function(x = rnorm(365), units = NULL,
zone = myFinCenter, FinCenter = myFinCenter)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Creates a dummy daily time Series

    # Arguments:
    #   x - a numeric vector
    #   origin - the first date in the series

    # FUNCTION:

    # Check:
    stopifnot(is.numeric(x))
    if (is.null(units)) units <- paste("X", 1:NCOL(x), sep = "")
    stopifnot(length(units)==NCOL(x))

    # Time Series:
    if (is.vector(x)) data = matrix(x, ncol = 1)
    if (is.matrix(x)) data = x
    positions = timeSequence(from = "1970-01-01", length.out =
        NROW(data), zone = zone, FinCenter = FinCenter)
    ans = timeSeries(data = data, charvec = positions, units = units,
        zone = zone, FinCenter = FinCenter)

    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


alignDailySeries <-
function (x, method = c("before", "after", "interp", "fillNA"),
include.weekends = FALSE, units = NULL, zone = myFinCenter,
FinCenter = myFinCenter)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Aligns an univariate 'timeSeries' object to new positions

    # Arguments:
    #   x - an object of class "timeSeries".
    #   method -
    #       "before" - use the data from the row whose position is
    #           just before the unmatched position;
    #       "after" - use the data from the row whose position is
    #           just after the unmatched position;
    #       "linear" - interpolate linearly between "before" and
    #           "after".
    #       "fillNA" - fill missing days with NA values
    #   include.weekends - a logical value. Should weekend dates be
    #       included or removed?

    # FUNCTION:

    # Settings:
    method = match.arg(method)

    # Internal Function
    # Univariate Time Series Alignment:
    alignDailySeries.OneColumn =
    function (x, method, include.weekends, zone, FinCenter) {
        # Settings:
        units = x@units
        # Units:
        # myUnits <<- "days"
        myUnits <- "days"
        # Fill with NAs:
        if (method == "fillNA") {
            positions = time(x)
            u = as.integer(julian(positions))
            v = as.vector(x@Data[, 1])
            x = u[1]:u[length(u)]
            y = approx(u, v, xout = x, method = "linear", f = 0.5)$y
            y[!(x %in% u)] = NA
            poschar = as.character(positions)
            td = timeSeries(y, timeSequence(from = poschar[1],
                to = poschar[length(poschar)], FinCenter = FinCenter,
                format = "%Y-%m-%d"), FinCenter = FinCenter)
            td@format = "%Y-%m-%d"
        } else {
            # Interpolate with real Values:
            # Wich method ?
            if (method == "interp") {
                method = "linear"
                f = 0.5 }
            if (method == "before") {
                method = "constant"
                f = 0 }
            if (method == "after") {
                method = "constant"
                f = 1 }
            # Get Positions and Data:
            positions = time(x)
            u = as.integer(julian(positions))
            v = as.vector(x@Data[, 1])
            # Approximate:
            #   method - specifies the interpolation method to be used.
            #       Choices are "linear" or "constant".
            #   f - For method="constant" a number between 0 and 1 inclusive,
            #       indicating a compromise between left- and right-continuous
            #       step functions. If 'y0' and 'y1' are the values to the left
            #       and right of the point then the value is 'y0*(1-f)+y1*f' so
            #       that 'f=0' is right-continuous and 'f=1' is left-continuous.
            x = u[1]:u[length(u)]
            y = approx(u, v, xout = x, method = method, f = f)$y
            # Create timeSeries:
            poschar = as.character(positions)
            td = timeSeries(y, timeSequence(from = poschar[1],
                to = poschar[length(poschar)], FinCenter = FinCenter,
                format = "%Y-%m-%d"), FinCenter = FinCenter)
            td@format = "%Y-%m-%d" }
        # Handle Weekends:
        if (!include.weekends) {
            # Internal Functions:
            is.weekday = function(x) {
                # x - a 'timeDate' object
                wday = as.POSIXlt(as.POSIXct(x))$wday
                return(!(wday == 0 | wday == 6)) }
            # Test:
            test = is.weekday(time(td))
            td@Data = as.matrix(td@Data[test, 1])
            td@positions = td@positions[test] }
        # Units:
        td@units = units
        colnames(td@Data) = units
        ans = td
        # Return Value:
        ans
    }

    # First Column:
    ans = alignDailySeries.OneColumn(x = x[, 1], method = method,
        include.weekends = include.weekends, zone = zone,
        FinCenter = FinCenter)

    # Next Columns:
    DimX = dim(x@Data)[2]
    if ( DimX > 1 ) {
        for ( i in 2:DimX ) {
            ans.add = alignDailySeries.OneColumn(x = x[, i],
                method = method, include.weekends = include.weekends,
                zone = zone, FinCenter = FinCenter)
            ans = merge(ans, ans.add) }
    }

    # Add New Units:
    if (!is.null(units)){
        ans@units = units
        colnames(ans@Data) = units
    }

    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


rollDailySeries =
    function(x, period = "7d", FUN, ...)
    {   # A function implemented by Diethelm Wuertz

        # Description:
        #   Rolls daily a 'timeSeries' on a given period

        # Arguments:
        #   x - an univariate "timeSeries" object or a numeric vector.
        #   n - an integer specifying the number of periods or
        #       terms to use in each rolling/moving sample.
        #   trim - a logical flag: if TRUE, the first n-1 missing values in
        #       the returned object will be removed; if FALSE, they will
        #       be saved in the returned object. The default is TRUE.
        #   FUN - the rolling function, arguments to this function can be
        #       passed through the \code{\dots} argument.

        # FUNCTION:

        # Fix missing matrix method for quantile(), still to do ...
        .quantile.matrix = function(x, probs = 0.95, ...) {
            apply(as.matrix(x), 2, quantile, probs = probs) }

        # Settings:
        periodLength = as.numeric(substr(period, 1, nchar(period) - 1))
        periodUnit = substr(period, nchar(period), nchar(period))
        N = nrow(x)
        Start = start(x) + (periodLength-1)*24*3600
        Positions = time(x)
        to = Positions[Positions > Start]
        from = to - periodLength*24*3600

        # Apply Function:
        ans = applySeries(x = x, from = from, to = to, FUN = FUN, ...)

        # Return Value:
        ans
    }


# ------------------------------------------------------------------------------


ohlcDailyPlot =
function(x, volume = TRUE, colOrder = c(1:5), units = 1e6, xlab =
c("Date", "Date"), ylab = c("Price", "Volume"), main = c("O-H-L-C", "Volume"),
grid.nx = 7, grid.lty = "solid", ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Plots open–high–low–close bar chart

    # Arguments:
    #   x - an S4 object of class 'timeSeries' with named entries:
    #       Open, High, Low, Close, and Volume

    # Reference:
    #   Build on top of Adrian Trapletti's plotOHLC()
    #   function from his R-package "tseries".

    # FUNCTION:

    # Next:
    x.filled = alignDailySeries(x, method = "fillNA", include.weekends = TRUE)
    jul = as.integer(julian(time(x.filled)))
    X = ts(x.filled@Data[, 1:4], start = min(jul), end = max(jul))

    # Plot OHLC:
    .plotOHLC(X, origin = "1970-01-01", xlab = xlab[1], ylab = ylab[1])
    # print(axTicks(1))
    # print(axTicks(2))
    title(main = main[1])
    grid(nx = grid.nx, ny = NULL, lty = grid.lty, ...)

    # Include Volume?
    if (volume) {
        Volume = x[, 5]/units
        plot(Volume, type = "h", xlab = xlab[2], ylab = ylab[2])
        title(main = main[2])
        grid(nx = grid.nx, ny = NULL, lty = grid.lty, ...) }

    # Return value:
    invisible()
}


# ------------------------------------------------------------------------------


.plotOHLC =
function (x, xlim = NULL, ylim = NULL, xlab = "Time", ylab, col = par("col"),
    bg = par("bg"), axes = TRUE, frame.plot = axes, ann = par("ann"),
    main = NULL, date = c("calendar", "julian"), format = "%Y-%m-%d",
    origin = "1899-12-30", ...)
{   # A Copy from Contributed R Package 'tseries'

    # Description:

    # FUNCTION:

    # Check for mts:
    if ((!is.mts(x)) || (colnames(x)[1] != "Open") || (colnames(x)[2] !=
        "High") || (colnames(x)[3] != "Low") || (colnames(x)[4] !=
        "Close"))
        stop("x is not a open/high/low/close time series")
    xlabel <- if (!missing(x)) deparse(substitute(x)) else NULL
    if (missing(ylab)) ylab <- xlabel
    date <- match.arg(date)
    time.x <- time(x)
    dt <- min(lag(time.x) - time.x)/3
    if (is.null(xlim)) xlim <- range(time.x)
    if (is.null(ylim)) ylim <- range(x[is.finite(x)])
    plot.new()
    plot.window(xlim, ylim, ...)
    segments(time.x, x[, "High"], time.x, x[, "Low"], col = col[1],
        bg = bg)
    segments(time.x - dt, x[, "Open"], time.x, x[, "Open"], col = col[1],
        bg = bg)
    segments(time.x, x[, "Close"], time.x + dt, x[, "Close"],
        col = col[1], bg = bg)
    if (ann) title(main = main, xlab = xlab, ylab = ylab, ...)
    if (axes) {
        if (date == "julian") {
            axis(1, ...)
            axis(2, ...)
        }
        else {
            n <- NROW(x)
            lab.ind <- round(seq(1, n, length = 5))
            D <- as.vector(time.x[lab.ind] * 86400) + as.POSIXct(origin,
                tz = "GMT")
            DD <- format.POSIXct(D, format = format, tz = "GMT")
            axis(1, at = time.x[lab.ind], lab = DD, ...)
            axis(2, ...)
        }
    }
    if (frame.plot) box(...)

    # Return Value:
    invisible()
}


################################################################################

