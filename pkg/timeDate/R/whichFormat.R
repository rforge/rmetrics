
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
# FUNCTION:                 DESCRIPTION:
#  whichFormat               Returns format string called by 'timeDate'
################################################################################


whichFormat <-
    function(charvec, silent = FALSE)
{
    # A function implemented by Diethelm Wuertz

    # Charvec String:
    if (is.null(charvec)) # avoid problems in timeSeries() when rownames NULL
        return("unknown")
    charvec = as.character(charvec)

    # Specifications:
    NCHAR = mean(nchar(charvec))
    NCHAR = nchar(charvec[1])
    SUBSTR = (substring(charvec[1], 5, 5) == "-")

    # American Format:
    if (regexpr("/....", charvec[1])[[1]] > 0) return("%m/%d/%Y")
    if (regexpr("-...-....", charvec[1])[[1]] > 0) return("%d-%b-%Y")

    # DW:
    #   There should be better checks on the format identification ...

    # Human readable ISO:
    if (NCHAR ==  4 & !SUBSTR) return("%Y")
    if (NCHAR ==  7 &  SUBSTR) return("%Y-%m")
    if (NCHAR == 10 &  SUBSTR) return("%Y-%m-%d")
    if (NCHAR == 13 &  SUBSTR) return("%Y-%m-%d %H")
    if (NCHAR == 16 &  SUBSTR) return("%Y-%m-%d %H:%M")
    if (NCHAR == 19 &  SUBSTR) return("%Y-%m-%d %H:%M:%S")

    # Short ISO:
    if (NCHAR ==  6 & !SUBSTR) return("%Y%m")
    if (NCHAR ==  8 & !SUBSTR) return("%Y%m%d")
    if (NCHAR == 10 & !SUBSTR) return("%Y%m%d%H")
    if (NCHAR == 12 & !SUBSTR) return("%Y%m%d%H%M")
    if (NCHAR == 14 & !SUBSTR) return("%Y%m%d%H%M%S")

    # Otherwise:
    if (!silent)
    warning("Could not determine time(date) format")

    # Return Value:
    "unknown"
}


# ------------------------------------------------------------------------------


.whichFormat <- whichFormat


################################################################################

