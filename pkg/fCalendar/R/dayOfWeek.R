
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
# FUNCTION:                 DESCRIPTION:
#  dayOfWeek                 Returns the day of the week to a 'timeDate' object
################################################################################


dayOfWeek <-
    function(x)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns day of week for time date objects

    # Arguments:
    #   x - an object of class "timeDate"

    # Example:
    #   weekDay(Sys.timeDate())
    #   weekDay(timeSequence("2005-05-15", "2005-07-15"))

    # FUNCTION:
    stopifnot(is(x, "timeDate"))

    # Get Day of Week:
    wd = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
    n = as.POSIXlt(x@Data)$wday + 1
    wdays = wd[n]
    names(wdays) = as.character(x@Data)

    # Return Value:
    wdays
}


################################################################################

