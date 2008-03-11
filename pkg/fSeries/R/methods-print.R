
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
# CLASS:                    REPRESENTATION:
#  'timeSeries'              S4 Class representation
# METHODS:                  PRINT AND PLOT FUNCTIONS:
#  show.timeSeries           Prints a 'timeSeries' object
#  .print.timeSeries         For internal use
################################################################################


setClass("timeSeries",
    # A class implemented by Diethelm Wuertz

    # Description:
    #   Class representatation for 'timeSeries' Objects.

    # CLASS:

    representation(
        Data = "matrix",
        positions = "character",
        format = "character",
        FinCenter = "character",
        units = "character",
        recordIDs = "data.frame",
        title = "character",
        documentation = "character")
)


# ------------------------------------------------------------------------------


show.timeSeries =
function(object)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Print method for an S4 object of class "timeSeries"

    # FUNCTION:

    # Unlike print the argument for show is 'object'.
    x = object
    recordIDs = FALSE

    # Series:
    cat(x@FinCenter, "\n", sep = "")
    if (recordIDs) {
        if (dim(x@Data)[1] == dim(x@recordIDs)[1]) {
            print(cbind(x@Data, as.matrix(x@recordIDs)), quote = FALSE)
        } else {
            print(x@Data)
        }
    } else {
        print(x@Data)
    }

    # Control:
    control = attr(x, "control")
    if (!is.null(control)) print(control)

    # Return Value:
    invisible(x)
}


# ------------------------------------------------------------------------------


setMethod("show", "timeSeries", show.timeSeries)


# ------------------------------------------------------------------------------


print.timeSeries <-
function(x, style = c("tS", "h", "ts"), by = c("month", "quarter"), ...)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Allows for horizontal and ts like print output.

    # Arguments:
    #   x - an object of class timeSeries
    #   style - a character value specifying how to print:
    #       "tS" Rmetrics' default vertical print style
    #       "h" horizontal print style,
    #       "ts" R's base style for regular time series
    #   by - specifies the period for a regular time serie,
    #       note only active for style="ts".

    # FUNCTION:

    # Print:
    style = match.arg(style)
    by = match.arg(by)
    if (style == "tS") {
        show(x)
    } else if (style == "h") {
        stopifnot(isUnivariate(x))
        print(as.vector(x))
    } else if (style == "ts") {
        freq = c(month = 12, quarter = 4)
        start(x)
        start = unlist(atoms(start(x)))
        end = unlist(atoms(end(x)))
        ts = ts(as.vector(x@Data), start[1:2], end[1:2], freq[by])
        print(ts)
    }

    # Return Value:
    return(invisible(x))
}


################################################################################

