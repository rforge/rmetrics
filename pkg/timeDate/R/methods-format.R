
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

format.timeDate <-
    function(x, format = "", tz = "", usetz = FALSE, ...)
{
    if (!inherits(x, "timeDate"))
        stop("wrong class")

    if (format != "") x@format <- format
    if (tz != "") x@FinCenter <- tz

    charvec = .timeDateData2charvec(x)
    # should add tz from table in formatFinCenter
    # ans <- ifelse(usetz, paste(charvec, x@FinCenter), charvec)

    # Return
    charvec
}

# ------------------------------------------------------------------------------

.timeDateData2charvec <-
    function(object)
{
    isoFormat <- "%Y-%m-%d %H:%M:%S"
    FinCenter <- object@FinCenter
    format <- object@format

    # tz = "GMT" important to avoid confusion when DST in force
    charvec <- format.POSIXct(object@Data, isoFormat)

    ans <- .formatFinCenter(charvec, FinCenter, type = "gmt2any")
    # ans <- format(as.POSIXct(ans))
    # tz = "GMT" important to avoid confusion when DST in force
    if (!identical(format, isoFormat))
        ans <- format(strptime(ans, isoFormat, tz = "GMT"), format = format)

    ans
}


################################################################################

