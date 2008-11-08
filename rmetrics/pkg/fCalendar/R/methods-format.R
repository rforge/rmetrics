
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
#  format.timeDate           Formats 'timeDate' as ISO conform string
################################################################################


format.timeDate =
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Formats 'timeDate' as ISO conform character string

    # Arguments:
    #   x - a 'timeDate' object

    # Value:
    #   Returns an ISO conform formatted character string.

    # Check Time Zone:
    TZ <- Sys.getenv("TZ")
    if(TZ[[1]] != "GMT") {
        Sys.setenv(TZ = "GMT")
        on.exit(Sys.setenv(TZ = TZ))
    }

    # Format - format.POSIXlt(x, format = "", usetz = FALSE, ...)
    format.POSIXct(x@Data, ...)
}


################################################################################

