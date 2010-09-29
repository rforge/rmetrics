
# This R package is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This R package is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General
# Public License along with this R package; if not, write to the
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston,
# MA  02111-1307  USA

# Copyrights (C)
# for this R-port:
#   1999 - Diethelm Wuertz, GPL
#   2007 - Rmetrics Foundation, GPL
#   Diethelm Wuertz <wuertz@phys.ethz.ch>
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# METHODS:                  DESCRIPTION:
#  isWeekday                 Tests if a date is a weekday or not
#  isWeekend                 Tests if a date falls on a weekend or not
################################################################################


isWeekday <- function(x)
{
    # A function implemented by Diethelm Wuertz
    # and improved by Yohan Chalabi

    # Description:
    #   Test if a date is a weekday day or not

    # Arguments:
    #   x - an object of class "timeDate"

    # Value:
    #   returns a logical or a vector of logicals

    # Example:
    #   isWeekday(timeDate("2004-07-01"))
    #   isWeekday(Sys.timeDate())

    # FUNCTION:

    # Test for Weekdays:
    wday <- as.POSIXlt(x, tz = "GMT")$wday
    ans <- (!(wday == 0 | wday == 6))
    names(ans) <- format(x)

    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


isWeekend <- function(x)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Tests if a date is a weekend day or not

    # Arguments:
    #   x - an object of class "timeDate"

    # Value:
    #   Returns a logical or a vector of logicals

    # Example:
    #   isWeekend(timeDate("2004-07-01"))
    #   isWeekend(Sys.timeDate())

    # Changes:
    #

    # FUNCTION:

    # Return Value:
    return(!isWeekday(x))
}


################################################################################

