
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
# MEHODS:                   SUBSETTING TIMEDATE OBJECTS:
#  [.timeDate                Extracts/replaces subsets from 'timeDate' objects
################################################################################


"[.timeDate" <-
    function(x, ..., drop = TRUE)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Extracts or replaces subsets from 'timeDate' objects

    # Arguments:
    #   x - a 'timeDate' object

    # Value:
    #   Returns a subset from a 'timeDate' object.

    # Changes:
    #

    # FUNCTION:

    # Set Timezone to GMT:
    myTZ = Sys.getenv("TZ")
    Sys.setenv(TZ = "GMT")

    # Subsets:
    z = as.POSIXlt(x@Data)
    val <- lapply(z, "[", ..., drop = drop)
    attributes(val) <- attributes(z)
    val = as.POSIXct(val)

    # Return Value:
    Sys.setenv(TZ = myTZ)
    new("timeDate",
        Data = val,
        format = x@format,
        FinCenter = x@FinCenter)
}


################################################################################

