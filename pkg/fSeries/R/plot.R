
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
#  plot.timeSeries           Plots a 'timeSeries' object
#  points.timeSeries         Adds points to a 'timeSeries' plot
#  lines.timeSeries          Adds lines to a 'timeSeries' plot
################################################################################


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


################################################################################

