
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
# FUNCTION:                 GENERATION OF TIMEDATE OBJECTS:
#  'timeDate'                S4 Class representation for timeDate objects
################################################################################

.timeDateValidity <-
    function(object)
{
    if(identical(attr(object@Data, "tzone"), "GMT"))
        TRUE
    else
        stop("@Data must be in \"GMT\" timezone.")
}

# ------------------------------------------------------------------------------

setClass("timeDate",
         # A class implemented by Diethelm Wuertz and Yohan Chalabi

         # Description:
         #   Class representatation for 'timeDate' Objects.

         # CLASS:
         representation(
                        Data = "POSIXct",
                        format = "character",
                        FinCenter = "character"
                        ),
         validity = .timeDateValidity
         )

################################################################################

