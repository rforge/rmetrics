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
# S4 METHOD                 FINANCIAL TIME SERIES:
#  returns,ANY               Computes returns from a 'matrix' object
#  returns,timeSeries        Computes returns from a 'timeSeries' object
# OLD FUNCTIONS:            KEEP THESE FUNCTIONS FOR COMPATIBILIT:
#  returnSeries              <- returns.timeSeries
#  getReturns                <- returns.timeSeries
################################################################################



# ------------------------------------------------------------------------------

setMethod("returns", "ANY",
          function(x,
                   method = c("continuous", "discrete", "compound", "simple"),
                   percentage = FALSE, ...)
{
    # A function implemented by Diethelm Wuertz and Yohan  Chalabi

    # Description:
    #   Computes returns from a 'matrix' object

    # Arguments:
    #   x - data object containing ordered price observations
    #   method - "continuous == "compound" and "discrete" == "simple"

    # Note:
    #   To make it conform with PortfolioAnalytics:
    #   "compound" == "continuous"
    #   "simple" == "discrete"

    # FUNCTION:

    # Settings:
    method <- match.arg(method)

    # Calculate Returns:
    data <- as(x, "matrix")
    positions <- time(x)

    if(method == "compound" || method == "continuous") {
        data <- rbind( data[1, , drop = FALSE]*NA, apply(log(data), 2, diff))
    }
    if(method == "simple" || method == "discrete") {
        data <- apply(rbind(data, NA*data[1,]), 2, diff) / data
        data <- rbind(data[1, , drop = FALSE]*NA, data)
        data <- data[-(length(positions) + 1), , drop = FALSE]
    }
    if (percentage) data <- 100*data

    # Return Value:
    data
})


# ------------------------------------------------------------------------------


setMethod("returns", "timeSeries",
          function(x,
                   method = c("continuous", "discrete", "compound", "simple"),
                   percentage = FALSE, na.rm = TRUE, trim = TRUE, ...)
{
    # A function implemented by Diethelm Wuertz and Yohan Chalabi

    # Description:

    # Arguments:

    # FUNCTION:

    # make sure that series is ordered
    x <- sort(x)

    # Get Returns:
    if (na.rm) x <- na.omit(x, ...)
    series(x) <- returns(as(x, "matrix"), method, percentage)
    if (trim) x <- na.omit(x, "r")

    # Return Value:
    x

})


# ------------------------------------------------------------------------------


returnSeries <-
    function(...)
{
    # A function implemented by Diethelm Wuertz

    # Description:

    # Arguments:

    # FUNCTION:
    # .Deprecated("returns", "timeSeries")

    returns(...)
}


# ------------------------------------------------------------------------------


getReturns <-  function(...)
{
    # A function implemented by Diethelm Wuertz

    # Description:

    # Arguments:

    # FUNCTION:
    # .Deprecated("returns", "timeSeries")
    returns(...)
}


################################################################################

