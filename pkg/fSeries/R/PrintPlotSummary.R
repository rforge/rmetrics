
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
#  plot.timeSeries           Plots a 'timeSeries' object
#  points.timeSeries         Adds points to a 'timeSeries' plot
#  lines.timeSeries          Adds lines to a 'timeSeries' plot
#  summary.timeSeries        Summarizes a 'timeSeries' object
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
   

################################################################################


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
    
    
setMethod("show", "timeSeries", show.timeSeries)   
    

# ------------------------------------------------------------------------------


plot.timeSeries =
function(x, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   NEW Plot method for an object of class "timeSeries"
   
    # Arguments:
    #   x - a "timeSeries" object 
    
    # FUNCTION:
    
    # Plot:
    plot(x = as.POSIXct(x@positions), y = x@Data, ...)
    
    # Return Value:
    invisible(x)
}


# ------------------------------------------------------------------------------


lines.timeSeries =
function(x, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   NEW Lines method for an object of class "timeSeries"
   
    # Arguments:
    #   x - a "timeSeries" object 
    
    # Example:
    #   plot(MSFT[,1]); lines(MSFT[,1], col = "red")
    
    # FUNCTION:
    
    # Plot:
    lines(x = as.POSIXct(x@positions), y = x@Data, ...)
    
    # Return Value:
    invisible(x)
}


# ------------------------------------------------------------------------------


points.timeSeries =
function(x, ...) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Plot method for an object of class "timeSeries"
        
    # Arguments:
    #   x - a "timeSeries" object
        
    # Value:
    #   Plots a 'timeSeries' object.

    # FUNCTION:
   
    # Add to Plot:
    points(x = as.POSIXct(x@positions), y = x@Data, ...)
            
    # Return Value:
    invisible(x)
}


# ------------------------------------------------------------------------------


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
    positions = seriesPositions(object)
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

