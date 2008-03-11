
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

signalCounts <-
function(int)
{
    # A function implemented by Diethelm Wuertz and Yohan Chalabi

    # Descriptions:
    #   Creates the charvec for integer indexed time stamps

    # Arguments:
    #   int - a vector of integers, the counts.

    # FUNCTION:

    # Check that int is an integer
    #   ...

    # Check that all int's are positive ...
    #   ...

    # Format:
    cint = as.character(int)
    ans = format(cint, width = max(nchar(cint)), justify = "right")

    # Return Value:
    ans
}


