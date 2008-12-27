
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
#  format.timeDate           Formats 'timeDate' as ISO conform string
################################################################################

## format.timeDate <-
##     function(x, format = "", tz = "", usetz = FALSE, ...)
## {
##     if (!inherits(x, "timeDate"))
##         stop("wrong class")

##     isoFormat <- "%Y-%m-%d %H:%M:%S"
##     isoDate <- "%Y-%m-%d"
##     FinCenter <- if (tz != "") tz else x@FinCenter

##     # tz = "GMT" important to avoid confusion when DST in force
##     charvec <- format(x@Data, isoFormat, tz = "GMT")

##     ans <- .formatFinCenter(charvec, FinCenter, type = "gmt2any")

##     # tz = "GMT" important to avoid confusion when DST in force
##     ans <- format(as.POSIXct(ans, isoFormat, tz = "GMT"),
##                   format = format, tz = "GMT")

##     # should add tz from table in formatFinCenter
##     if (usetz)
##         ans <- paste(ans, x@FinCenter)

##     # Return
##     ans
## }


format.timeDate <-
    function(x, format = "", tz = "", usetz = FALSE, ...)
{
    if (!inherits(x, "timeDate"))
        stop("wrong class")

    FinCenter <- if (tz != "") tz else x@FinCenter

    num <- .formatFinCenterNum(as.numeric(x@Data), FinCenter, type = "gmt2any")
    ans <- format(as.POSIXct(num, origin = "1970-01-01", tz = "GMT"),
                  tz = "GMT", format = format)

    # should add tz from table in formatFinCenter
    if (usetz)
        ans <- paste(ans, x@FinCenter)

    # Return
    ans
}

################################################################################
