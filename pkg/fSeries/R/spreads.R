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
# FUNCTION:                 FINANCIAL TIME SERIES:
#  midquotes                 Computes mid quotes from a 'timeSeries' object
#  spreads                   Computes spreads from a 'timeSeries' object
# OLD FUNCTIONS:
#  midquoteSeries <- midquotes
#  spreadSeries <- spreads
################################################################################


midquotes <- 
    function(x, which = c("Bid", "Ask"))
{
    # Compute Mid Quotes:
    midQuotes = 0.5 * ( x[, which[1]] + x[, which[2]] )

    # Return Value:
    midQuotes
}


# ------------------------------------------------------------------------------


midquoteSeries <- 
    function(...)
{
    midquotes(...)
}


# ------------------------------------------------------------------------------


spreads <- 
    function(x, which = c("Bid", "Ask"), tickSize = NULL)
{
    # Compute Spread:
    Spread = x[, which[2]] - x[, which[1]]
    if (!is.null(tickSize)) Spread@Data = round(Spread@Data/tickSize)

    # Return Value:
    Spread
}


# ------------------------------------------------------------------------------


spreadSeries <- 
    function(...)
{
    spreads(...)
}


################################################################################

