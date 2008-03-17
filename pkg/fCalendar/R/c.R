
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
# MEHODS:                   DESCRIPTION:
#  c.timeDate                Concatenates 'timeDate' objects
################################################################################


c.timeDate <-
    function(..., recursive = FALSE)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Concatenates objects of class 'timeDate'

    # Arguments:
    #   ... - objects to be concatenated.
    #   recursive - a logical. If 'recursive=TRUE', the function
    #       recursively descends through lists combining all their
    #       elements into a vector.

    # Value:
    #   Returns all arguments to be coerced to a 'timeDate' object
    #   which is the type of the returned value.

    # Example:
    #   timeCalendar()[0]; c(timeCalendar()[0], timeCalendar())

    # Details:
    #   This is a generic function which combines its arguments.

    # FUNCTION:

    # Set Timezone to GMT:
    myTZ = Sys.getenv("TZ")
    Sys.setenv(TZ = "GMT")

    # List all:
    z = list(...)

    # Convert to GMT character vectors:
    all = NULL
    for (i in 1:length(z)) {
        # DW added if:
        if (length(z[[i]]) > 0) {
            new = format(timeDate(z[[i]], zone = z[[i]]@FinCenter,
                FinCenter = "GMT")@Data, "%Y-%m-%d %H:%M:%S")
            all = c(all, new)
        }
    }

    # Convert to Financial Center of the first element:
    ans = timeDate(all, zone = "GMT", FinCenter = z[[1]]@FinCenter)

    # Return Value:
    Sys.setenv(TZ = myTZ)
    ans
}


################################################################################

