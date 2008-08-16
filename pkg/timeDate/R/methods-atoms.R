
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
# METHOD:                   DESCRIPTION:
#  atoms,timeDate            Returns date/time atoms from a 'timeDate' object
################################################################################

setMethod("atoms", "timeDate",
    function(x, ...)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Extracts atoms from a 'timeDate' object.

    # Arguments:
    #   x - a 'timeDate' object from which to extract the
    #       calendar "atoms".

    # Value:
    #   Returns a data.frame with the following calendar atoms:
    #   Y(ear), m(onth), d(ay), H(our), M(inutes), S(econds).

    # FUNCTION:

    # Check Class Type:
    if (!inherits(x, "timeDate")) stop("Wrong class type")

    # mdy:
    X = as.POSIXlt(x@Data)
    Y = X$year + 1900
    m = X$mon + 1
    d = X$mday
    H = X$hour
    M = X$min
    S = X$sec

    # Data Frame:
    ans = data.frame(Y = Y, m = m, d = d, H = H, M = M, S = S)
    attr(ans, "control") = c(FinCenter = x@FinCenter)

    # Return Value:
    ans
})
