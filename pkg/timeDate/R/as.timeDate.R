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
# MEHOD:                    COERCION AND OBJECT TRANSFORMATIONS:
#  as.timeDate               Implements Use Method
#  as.timeDate.default       Default Method
#  as.timeDate.POSIXt        Returns a 'POSIX' object as 'timeDate' object
#  as.timeDate.Date          Returns a 'POSIX' object as 'timeDate' object
################################################################################


as.timeDate <-
    function(x, zone = NULL, FinCenter = NULL)
{
    UseMethod("as.timeDate")
}


# ------------------------------------------------------------------------------


as.timeDate.default <-
    function(x, zone = "", FinCenter = "")
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns default object as 'timeDate' object

    # Arguments:
    #   x - a 'timeDate' object

    # Value:
    #   Returns 'x' as a 'timeDate' object.

    # FUNCTION:

    # as timeDate:
    if (zone == "")
        zone <- getRmetricsOptions("myFinCenter")
    if (FinCenter == "")
        FinCenter <- getRmetricsOptions("myFinCenter")

    # Return Value:
    timeDate(as.character(x), zone = zone, FinCenter = FinCenter)
}


# ------------------------------------------------------------------------------


as.timeDate.timeDate <-
    function(x, zone = x@FinCenter, FinCenter = "")
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns default object as 'timeDate' object

    # Arguments:
    #   x - a 'timeDate' object

    # Value:
    #   Returns 'x' as a 'timeDate' object.

    stopifnot(is(x, "timeDate"))

    if (FinCenter == "")
        FinCenter <- getRmetricsOptions("myFinCenter")
    if (zone != x@FinCenter)
        warning("argument zone is ignored and FinCenter\n of timeDate is used as zone")
    ## FIXME : and now?   'zone' is *NOT* ignored!

    # Return as timeDate:
    timeDate(as.character(x), zone = zone, FinCenter = FinCenter)
}


# ------------------------------------------------------------------------------


as.timeDate.Date <- function(x, zone = "", FinCenter = "")
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns a 'Date' object as 'timeDate' object

    # Arguments:
    #   x - a 'Date' (or 'POSIXt') object

    # Value:
    #   Returns 'x' as  timeDate object.

    # FUNCTION:
    if (zone == "")
        zone <- getRmetricsOptions("myFinCenter")
    if (FinCenter == "")
        FinCenter <- getRmetricsOptions("myFinCenter")

    # Return as timeDate:
    timeDate(x, zone = zone, FinCenter = FinCenter)
}

as.timeDate.POSIXt <- as.timeDate.Date


################################################################################

