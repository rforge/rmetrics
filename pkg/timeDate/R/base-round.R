
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
# MEHODS:                   DESCRIPTION:
#  round.timeDate            Rounds objects of class 'timeDate'
#  trunc.timeDate            Truncates objects of class 'timeDate'
################################################################################


round.timeDate =
function(x, digits = c("days", "hours", "mins"))
{   # A function implemented by Diethelm Wuertz

    # Note:
    #   round.timeDate(x, units = c("days", "hours", "mins"))    # FAILS !!!

    # FUNCTION:

    # Get Units:
    units = match.arg(digits)

    # Use:
    lt <- round.POSIXt(as.POSIXlt(format(x), tz = "GMT"), units = units)
    ans = timeDate(lt, zone = x@FinCenter, FinCenter = x@FinCenter)

    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


trunc.timeDate =
function(x, units = c("days", "hours", "mins"), ...)
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:

    # Get Units:
    units = match.arg(units)

    # Sorting under GMT is not what we want!
    # GMT = timeDate(x, zone = x@FinCenter, FinCenter = "GMT")
    # lt = trunc.POSIXt(GMT@Data, units = units)
    # ans = timeDate(lt, zone = "GMT", FinCenter = x@FinCenter)

    # Use:
    lt <- trunc.POSIXt(as.POSIXlt(format(x), tz = "GMT"), units = units)
    ans = timeDate(lt, zone = x@FinCenter, FinCenter = x@FinCenter)

    # Return Value:
    ans
}


################################################################################

