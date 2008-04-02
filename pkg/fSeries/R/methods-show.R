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
# S4 METHODS:               DESCRIPTION:
#  show,timeSeries           Prints a 'timeSeries' object
#  print,timeSeries          For internal use
################################################################################


setMethod("show", "timeSeries", 
    function(object)
{   
    # A function implemented by Diethelm Wuertz and Yohan Chalabi

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
    invisible(NULL)
})

      
# ------------------------------------------------------------------------------


.print.timeSeries <-
    function(x, FinCenter = NULL, format = NULL,
    style = c("tS", "h", "ts"), by = c("month", "quarter"))
{
    # A function implemented by Diethelm Wuertz and Yohan Chalabi
    
    # Description:
    #   Allows for horizontal and ts like print output.
    
    # Arguments:
    #   x - an object of class timeSeries
    #   FinCenter - print with time stamps according to FinCenter
    #   format - use specified format for printing
    #   style - a character value specifying how to print:
    #       "tS" Rmetrics' default vertical print style
    #       "h" horizontal print style,
    #       "ts" R's base style for regular time series
    #   by - specifies the period for a regular time serie,
    #       note only active for style="ts".
    
    # Example:
    #   x = timeSeries(); print(x, format = "%d %b %Y"); x
    
    # FUNCTION:
    
    # Change Financial Center:
    if (!is.null(FinCenter)) finCenter(x) <- FinCenter
    
    # Match Arguments:
    style = match.arg(style)
    by = match.arg(by)
    
    # Print:
    if (style == "tS") {
        # Change Format:
        if (!is.null(format)) {
            charvec = rownames(series(x))
            charvec = format(as.POSIXct(charvec, tz = "GMT"), 
                format = format, tz = "GMT")
            rownames(x@Data) <- charvec
        }
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


setMethod("print", "timeSeries", .print.timeSeries)


################################################################################

