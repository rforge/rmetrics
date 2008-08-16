
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
# FUNCTION:                 FINANCIAL TIME SERIES:
#  returns                   Computes returns from a 'timeSeries' object
#  returns.default
#  returns.timeSeries
#  returns.zoo
# OLD FUNCTIONS:
#  returnSeries <- returns.timeSeries
#  getReturns <- returns.timeSeries
################################################################################


returns =
function(x, ...)
{
    UseMethod("returns")
}


# ------------------------------------------------------------------------------


returns.default <-
function(x, method = c("continuous", "discrete", "compound", "simple"),
percentage = FALSE, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Calculates returns from a price stream

    # Arguments:
    #   x - data object containing ordered price observations
    #   method - "simple", "compound"

    # Note:
    #   To make it conform with PortfolioAnalytics:
    #   "compound" == "continuous"
    #   "simple" == "discrete"

    # FUNCTION:

    # Settings:
    method = match.arg(method)

    # Calculate Returns:
    data = as.matrix(x)
    positions = time(x)

    if(method == "compound" || method == "continuous") {
        data = rbind( data[1, , drop = FALSE]*NA, apply(log(data), 2, diff))
    }
    if(method == "simple" || method == "discrete") {
        data = apply(rbind(data, NA*data[1,]), 2, diff) / data
        data = rbind(data[1, , drop = FALSE]*NA, data)
        data = data[-(length(positions) + 1), , drop = FALSE]
    }
    if (percentage) data = 100*data

    # Return Value:
    data
}


# ------------------------------------------------------------------------------


returns.timeSeries =
function(x, method = c("continuous", "discrete", "compound", "simple"),
percentage = FALSE, na.rm = TRUE, trim = TRUE, ...)
{   # A function implemented by Diethelm Wuertz

    # Get Returns:
    if (na.rm) x = na.omit(x, ...)
    x@Data = returns(as.matrix(x), method, percentage)
    if (trim) x = na.omit(x, "r")

    # Return Value:
    x

}


# ------------------------------------------------------------------------------


## returns.zoo =
## function(x, method = c("continuous", "discrete", "compound", "simple"),
## percentage = FALSE, na.rm = TRUE, trim = TRUE, ...)
## {   # A function implemented by Diethelm Wuertz

##     # Get Returns:
##     if (na.rm) x = na.omit(x, ...)
##     x = zoo(returns(as.matrix(x), method, percentage), time(x))
##     if (trim) x = na.omit(x, "r")

##     # Return Value:
##     x
## }



# ------------------------------------------------------------------------------


returnSeries = function(...)
{
    returns.timeSeries(...)
}


# ------------------------------------------------------------------------------


getReturns <-  function(...)
{
    returns.timeSeries(...)
}


################################################################################

