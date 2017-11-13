
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General
# Public License along with this library; if not, write to the
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston,
# MA  02111-1307  USA


################################################################################
# METHOD:                 EXTRACTORS:
#  fitted.fGARCH           S3 fitted values for an object of class 'fGARCH'
################################################################################


setMethod(f = "fitted", signature(object = "fGARCH"), definition =
    function(object)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   S3 Fitted values method for an object of class fGARCH

    # Arguments:
    #   object - an object of class fGarch as returned by the function
    #       garchFit

    # FUNCTION:

    # Get numeric vector of fitted, optionally standardized
    fitted = object@fitted

    # Get original time series class:
    ans = slot(object, "data")
    Name = as.character(object@formula[2])
    attr(ans, "Name") <- Name
    # Return Value:
    ans
})


################################################################################

