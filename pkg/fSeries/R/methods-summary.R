
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
#   1999 - 2007, Diethelm Wuertz, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


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

