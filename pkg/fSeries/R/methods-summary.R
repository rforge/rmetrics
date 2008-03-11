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
# METHODS:                  PRINT AND PLOT FUNCTIONS:
#  summary.timeSeries        Summarizes a 'timeSeries' object
################################################################################


summary.timeSeries =
function(object, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   S3 Summary method for objects of class "timeDate"

    # Arguments
    #   x - an object of class "timeDate"

    # FUNCTION:

    # Series Name:
    cat("Time Series:          ")
    cat("\n Name:              ", substitute(object))
    # Data Matrix:
    Dim = dim(object@Data)
    cat("\nData Matrix:        ")
    cat("\n Dimension:         ", Dim)
    cat("\n Column Names:      ", colnames(object@Data) )
    firstName = rownames(object@Data)[1]
    lastName = rownames(object@Data)[Dim[1]]
    cat("\n Row Names:         ", firstName, " ... ", lastName)
    # Date/Time Positions:
    positions = time(object)
    cat("\nPositions:          ")
    cat("\n Start:             ", as.character(start(positions)))
    cat("\n End:               ", as.character(end(positions)))
    # Other Attributes:
    cat("\nAttributes:         ")
    cat("\n Format:            ", object@format)
    cat("\n FinCenter:         ", object@FinCenter)
    cat("\n Units:             ", object@units)
    cat("\n Title:             ", object@title)
    cat("\n Documentation:     ", object@documentation)
    cat("\n")

    # Return Value:
    invisible()
}


################################################################################

