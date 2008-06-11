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
#  outlier.timeSeries        Removes outliers from a 'timeSeries' object
################################################################################

outlier.timeSeries =
    function(x, sd = 10, complement = TRUE, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns outliers in a timeSeries object or the complement

    # Arguments:
    #   x - a 'timeSeries' object.
    #   sd - a numeric value of standard deviations, e.g. 10
    #       means that values larger or smaller tahn ten
    #       times the standard deviation of the series will
    #       be removed.
    #   complement - a logical flag, should the outler series
    #       or its complement be returns.

    # FUNCTION:

    # Check if univariate Series:
    if (!isUnivariate(x))
        stop("Supports only univariate timeSeries Objects")

    # Find Outliers:
    SD = sd * sd(x)
    if (complement) {
        x  = x[abs(x) <= SD,]
    } else {
        x = x[abs(x) > SD,]
    }

    # Return Value:
    x
}
