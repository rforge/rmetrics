
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


################################################################################
# FUNCTION:               DESCRIPTION:
#  align,timeDate          Aligns a 'timeDate' object to regular time stamps
################################################################################

setMethod("align", "timeDate",
    function(x, by = "1d", offset = "0s")
    # , include.weekends = TRUE)
{
    # Description:
    #   Aligns a 'timeDate' object to regular time stamps

    # Example:
    #   align.timeDate(timeCalendar(), "1w")        # Weekly
    #   align.timeDate(timeCalendar(), "2w", "3d")  # Bi-Weekly with offset

    # FUNCTION:

    # Settings:
    periods = c(7*24*3600, 24*3600, 3600, 60, 1)
    names(periods) = c("w", "d", "h", "m", "s")
    offset = as.integer(gsub("[a-z]", "", offset, perl = TRUE)) *
        periods[gsub("[ 0-9]", "", offset, perl = TRUE)]
    by = as.integer(gsub("[a-z]", "", by, perl = TRUE)) *
        periods[gsub("[ 0-9]", "", by, perl = TRUE)]

    # Convert timeDate to GMT-POSIX
    posixGMT = as.POSIXct(
        timeDate(x, zone = x@FinCenter, FinCenter = "GMT"), tz = "GMT")

    # Compute Julian counts (x) and series values (y)
    Origin = as.POSIXct("1970-01-01", tz = "GMT")
    u <- as.integer(difftime(posixGMT, Origin, tz = "GMT", units = "secs"))
    xout = seq(u[1] + offset, u[length(u)], by = by)
    posixGMT = Origin + as.integer(xout)

    x = timeDate(as.character(posixGMT), zone = "GMT", FinCenter = x@FinCenter)

    # Remove Weekends:
    # if(!include.weekends) x = x[isWeekday(x), ]

    # Return Value:
    x
})

# ------------------------------------------------------------------------------

setMethod("align", "ANY",
     function(x, y, xout, method = "linear", n = 50, rule = 1, f = 0, ties = mean, ...)
 {
     # A function implemented by Diethelm Wuertz

     # FUNCTION:

     # Align by Approximation:
     ans = approx(x = x, y = y, xout = xout, method = method, n = n,
         rule = rule, f = f, ties = ties, ...)

     # Return Value:
     ans
 })


################################################################################


