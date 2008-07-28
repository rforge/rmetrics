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
# CLASS:                    REPRESENTATION:
#  'timeSeries'              S4 Class representation
################################################################################


.timeSeriesValidity <-
    function(object)
{

    if (NROW(object@.Data) != length(object@positions))
        stop("@positions and rows of @.Data are not equal")
    if (NCOL(object@.Data) != length(object@units))
        stop("@units and columns of @.Data are not equal")

}


# ------------------------------------------------------------------------------

setClass("timeSeries",
         # A class implemented by Diethelm Wuertz and Yohan Chalabi

         # Description:
         #   Class representatation for 'timeSeries' Objects.

         # CLASS:

         representation(
                        .Data = "matrix",
                        positions = "character",
                        format = "character",
                        FinCenter = "character",
                        units = "character",
                        recordIDs = "data.frame",
                        title = "character",
                        documentation = "character"),
         contains = "structure",
         validity = .timeSeriesValidity
         )
