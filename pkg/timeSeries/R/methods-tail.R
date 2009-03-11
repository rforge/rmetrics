#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  ../../COPYING


################################################################################
# METHOD:                   SUBSETTING METHODS ON DATA:
#  tail,timeSeries           Returns the tail of a 'timeSeries' object
################################################################################

setMethod("tail",
          "timeSeries",
    function(x, n = 6, recordIDs = FALSE, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns the tail of a 'timeSeries' object

    # Arguments:
    #   x - a 'timeSeries' object.

    # Value:
    #   Returns the tail of an object of class 'timeSeries'.

    # FUNCTION:

    # Tail:
    if (recordIDs & dim(x)[1] == dim(x@recordIDs)[1])
        cbind(tail.matrix(x, n = n, addrownums = FALSE, ...),
              tail(x@recordIDs, n = n, addrownums = FALSE, ...))
    else
        tail.matrix(x, n = n, addrownums = FALSE, ...)
})
