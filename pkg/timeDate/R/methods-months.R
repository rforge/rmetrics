
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


################################################################################
# METHOD:                   DESCRIPTION:
#  months,timeDate           Extracts months atom from a 'timeDate' object
################################################################################


setMethod("months", "timeDate",
    function(x, abbreviate = NULL)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Extracts months atom from a timeDate object

    # Arguments:
    #   x - a 'timeDate' object from which to extract the
    #       month "atom".

    # Value:
    #   Returns the month from a 'timeDate' object as an integer
    #   value or vector with elements ranging between 1 and 12,
    #   numbering the months from January to December.

    # FUNCTION:

    # Check Class Type:
    if (!inherits(x, "timeDate")) stop("Wrong class type")

    # Month:
    ans = as.POSIXlt(x@Data)$mon+1
    attr(ans, "control") = c(FinCenter = x@FinCenter)

    # Return Value:
    ans
})


################################################################################
