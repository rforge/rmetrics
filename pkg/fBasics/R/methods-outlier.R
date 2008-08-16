
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
# METHOD:                   SUBSETTING METHODS ON DATA:
#  outlier,ANY               Removes outliers from a 'timeSeries' object
################################################################################

setMethod("outlier", "ANY",
    function(x, sd = 5, complement = TRUE, ...)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns outliers

    # Arguments:
    #   x - a numeric vector
    #   sd - a numeric value of standard deviations, e.g. 5
    #       means that values larger or smaller tahn five
    #       times the standard deviation of the series will
    #       be detected.
    #   complement - a logical flag, should the outlier series
    #       or its complements be returned.

    # Note:
    #   This function is thought to find splits in financial
    #   price or index data. If a price or index is splitted we
    #   observe in the returns a big jump of several standard
    #   deviations.

    # FUNCTION:

    # Find Outliers:
    SD = sd * sd(x)
    if (complement) {
        ans  = x[x <= SD]
    } else {
        ans = x[x > SD]
        names(ans) = as.character(which(x > SD))
    }

    # Return Value:
    ans
})
