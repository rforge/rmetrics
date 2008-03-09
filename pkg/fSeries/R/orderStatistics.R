
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
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


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
        S = sort(X@Data, index.return = TRUE)
        X@Data = matrix(S$x, ncol = 1)
        X@positions = rownames(X@Data) = positions[S$ix]
        colnames(X@Data) = Units[i]
        TEXT = paste("ans$", Units[i], "=X", sep = "")
        eval(parse(text = TEXT))
    }

    # Return Value:
    ans

}


################################################################################

