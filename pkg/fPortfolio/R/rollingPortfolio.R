
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR Description. See the
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General
# Public License along with this library; if not, write to the
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston,
# MA 02111-1307 USA

# Copyrights (C)
# for this R-port:
#   1999 - Diethelm Wuertz, GPL
#   2007 - Rmetrics Foundation, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
# for code accessed (or partly included) from other sources:
#   see Rmetric's copyright and license files


################################################################################
# FUNCTION:                         DESCRIPTION:
#  rollingWindows                    Returns a list of rolling window frames
# FUNCTION:                         DESCRIPTION:
#  rollingCmlPortfolio               Rolls a CML portfolio
#  rollingTangencyPortfolio          Rolls a tangency portfolio
#  rollingMinvariancePortfolio       Rolls a minimum risk portfolio
# FUNCTION:                         DESCRIPTION:
#  rollingPortfolioFrontier          Rolls a portfolio frontier
################################################################################


rollingWindows <-
    function(x, period = "12m", by = "1m")
{
    # A function implemented by Rmetrics
    # modified by Yohan Chalabi 2008-02-18

    # Description:
    #   Returns vectors of start and end dates for a rolling time series

    # Arguments:
    #   x - a timeSeries object of asset returns
    #   period - a character string denoting the length of the rolling
    #       window, e.g. "24m" means 24 months
    #   by - a character string denoting the shift of the rolling window,
    #       e.g. "1m" means one month

    # Note:
    #   Only "monthly" frequencies are currently supported.

    # Example:
    #   x = sort(as.timeSeries(data(smallcap.ts))); rollingWindows(x)

    # FUNCTION:

    # Get Window Parameter:
    periodLength = as.numeric(substr(period, 1, nchar(period)-1))
    periodUnit = substr(period, nchar(period), nchar(period))
    byLength = as.numeric(substr(by, 1, nchar(by)-1))
    byUnit = substr(by, nchar(by), nchar(by))
    stopifnot(periodUnit == "m")
    stopifnot(byUnit == "m")

    # Make Windows - expand series x to a monthly series
    positions = time(x)
    startPositions = unique(timeFirstDayInMonth(positions))
    # for non monthly data
    # startPositions@Data[1] <- start(x)@Data
    endPositions = unique(timeLastDayInMonth(positions))
    # for non monthly data
    # endPositions@Data[length(endPositions)] <- end(x)@Data
    numberOfPositions = length(startPositions)
    startSeq <- seq(from = 1,
                    to = (numberOfPositions-periodLength + 1),
                    by = byLength)
    startDates = startPositions[startSeq]
    endSeq <- seq(from = periodLength,
                  to = numberOfPositions,
                  by = byLength)
    endDates = endPositions[endSeq]

    # Windows:
    windows = list(from = startDates, to = endDates)
    attr(windows, "control") = c(start = start(positions), end = end(positions))

    # Return Value:
    windows
}


# ------------------------------------------------------------------------------


rollingCmlPortfolio <-
    function(data, spec, constraints, from, to, action = NULL,
    title = NULL, description = NULL, ...)
{
    # A function implemented by Rmetrics

    # Description:
    #   Computes EF on a rolling timeSeries Windows

    # Arguments:

    # FUNCTION:

    # Roll the Frontier and return it in a list:
    roll = list()
    for (i in 1:length(from)) {

        # Data must be a multivariate timeSeries object ...
        series = cut(data, from = from[i], to = to[i])

        # Calculation efficient frontiers and save them all in a list:
        portfolio = tangencyPortfolio(data = series, spec, constraints)
        roll[[i]] = portfolio

        # Now you can do any "action" you want to do with the EFs:
        if (!is.null(action)) {
            fun = match.fun(action)
            fun(roll, from, to, ...)
        }
    }

    # Return Value:
    invisible(roll)
}


# ------------------------------------------------------------------------------


rollingTangencyPortfolio <-
    function(data, spec, constraints, from, to, action = NULL,
    title = NULL, description = NULL, ...)
{   # A function implemented by Rmetrics

    # Description:
    #   Computes EF on a rolling timeSeries Windows

    # Arguments:
    #   windows - a list with two named 'timeDate' entries, "from" and
    #       "to", defining the start and end dates of your windows.
    #   ... - optional parameters which can be directed to the optional
    #       function action().

    # FUNCTION:

    # Roll the Frontier and return it in a list:
    roll = list()
    for (i in 1:length(from)) {

        # Data must be a multivariate timeSeries object ...
        series = cut(data, from = from[i], to = to[i])

        # Calculation efficient frontiers and save them all in a list:
        portfolio = tangencyPortfolio(data = series, spec, constraints)
        roll[i] = portfolio

        # Now you can do any "action" you want to do with the EFs:
        if (!is.null(action)) {
            fun = match.fun(action)
            fun(roll, from, to, ...)
        }
    }

    # Return Value:
    invisible(roll)
}


# ------------------------------------------------------------------------------


rollingMinvariancePortfolio <-
    function(data, spec, constraints, from, to, action = NULL,
    title = NULL, description = NULL, ...)
{
    # A function implemented by Rmetrics

    # Description:
    #   Computes EF on a rolling timeSeries Windows

    # Arguments:
    #   windows - a list with two named 'timeDate' entries, "from" and
    #       "to", defining the start and end dates of your windows.
    #   ... - optional parameters which can be directed to the optional
    #       function action().

    # FUNCTION:

    # Roll the Frontier and return it in a list:
    roll = list()
    for (i in 1:length(from)) {

        # Data must be a multivariate timeSeries object ...
        series = cut(data, from = from[i], to = to[i])

        # Calculation efficient frontiers and save them all in a list:
        portfolio = minvariancePortfolio(data = series, spec, constraints)
        roll[i] = portfolio

        # Now you can do any "action" you want to do with the EFs:
        if (!is.null(action)) {
            fun = match.fun(action)
            fun(roll, from, to, ...)
        }
    }

    # Return Value:
    invisible(roll)
}


################################################################################


rollingPortfolioFrontier <-
    function(data, spec, constraints, from, to, action = NULL,
    title = NULL, description = NULL, ...)
{   # A function implemented by Rmetrics

    # Description:
    #   Computes EF on a rolling timeSeries Windows

    # Arguments:
    #   windows - a list with two named 'timeDate' entries, "from" and
    #       "to", defining the start and end dates of your windows.
    #   ... - optional parameters which can be directed to the optional
    #       function action().

    # FUNCTION:

    # Roll the Frontier and return it in a list:
    roll = list()
    for (i in 1:length(from)) {

        # Data must be a multivariate timeSeries object ...
        series = cut(data, from = from[i], to = to[i])

        # Calculation efficient frontiers and save them all in a list:
        frontier = portfolioFrontier(data = series, spec, constraints,
            title = title, description = description)
        roll[i] = frontier

        # Now you can do any "action" you want to do with the EFs:
        if (!is.null(action)) {
            fun = match.fun(action)
            fun(roll, from, to, ...)
        }
    }

    # Return Value:
    invisible(roll)
}


################################################################################

