
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General
# Public License along with this library; if not, write to the
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston,
# MA  02111-1307  USA


################################################################################


### DO NOT USE !!!


.rollingDrawdowns <-
function(x, period = "24m", by = "1m", ...)
{
    # Example:
    #   DJ = as.timeSeries(data(DowJones30))
    #   DJ = DJ[, c("CAT", "IBM", "GE", "JPM")]
    #   par(mfrow=c(2, 2)) 
    #   for (i in 1:4) plot(.rollingDrawdowns(DJ[, i], "3m"), type = "l")
    
    # FUNCTION:
    
    # Check:
    stopifnot(isUnivariate(x))

    # Create Rolling Windows:
    rW = rollingWindows(x, period, by)

    # Roll the Function and Save in List:
    ans = NULL
    for (i in 1:length(rW$from)) {

        X = window(x, start = rW$from[i], end = rW$to[i])
        Y = min(drawdowns(X))
        ans = c(ans, Y)
    }

    # Return Value
    invisible(ans)
}


# ------------------------------------------------------------------------------


.rollingEMA <-
    function(x, period = "24m", by = "1m", lambda = 0.1, ...)
{
    # Example:
    #   DJ = returns(as.timeSeries(data(DowJones30)))
    #   DJ = DJ[, c("CAT", "IBM", "GE", "JPM")]
    #   par(mfrow=c(2, 2))
    #   for (i in 1:4) plot(.rollingEMA(abs(DJ[, i]), "3m"), type = "b")
    
    # FUNCTION:
    
    # Check:
    stopifnot(isUnivariate(x))

    # Create Rolling Windows:
    rW = rollingWindows(x, period, by)

    # Roll the Function and Save in List:
    ans = NULL
    for (i in 1:length(rW$from)) {

        X = window(x, start = rW$from[i], end = rW$to[i])
        Y = rev(emaTA(X, lambda))[1]
        ans = c(ans, Y)
    }

    # Return Value
    invisible(ans)
}


################################################################################

