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
#  orderStatistics           Compute order statistic of a 'timeSeries'
################################################################################


orderStatistics <-
    function(x)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Compute the order statistics for a 'timeSeries object

    # Value:
    #   A named list with the order statistics for each column of
    #   the inputted series.

    # FUNCTION:

    # Order Statistics
    Units = x@units
    nUnits = length(Units)
    ans = list()
    for (i in 1:nUnits) {
        X = x[, i]
        positions = X@positions
        S = sort(series(X), index.return = TRUE)
        series(X) = matrix(S$x, ncol = 1)
        X@positions = rownames(X) = positions[S$ix]
        colnames(X) = Units[i]
        TEXT = paste("ans$", Units[i], "=X", sep = "")
        eval(parse(text = TEXT))
    }

    # Return Value:
    ans

}


################################################################################

