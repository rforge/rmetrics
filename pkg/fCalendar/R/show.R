
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
#   1999 - 2008, Diethelm Wuertz, Rmetrics Foundation, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# METHODS:                  REPRESENTATION OF TIMEDATE OBJECTS:
#  show.timeDate             Prints 'timeDate' object
#  plot.timeDate             Plots 'timeDate' object
#  points.timeDate           Adds points to a 'timeDate' plot
#  lines.timeDate            Adds lines to a 'timeDate' plot
#  summary.timeDate          Summarizes details of a 'timeDate' object
#  format.timeDate           Formats 'timeDate' as ISO conform string
################################################################################


show.timeDate = 
function(object)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Print method for an S4 object of class "timeDate"
 
    # FUNCTION:
       
    # Unlike print the argument for show is 'object'.
    x = object
    
    # Set Timezone to GMT:
    myTZ = Sys.getenv("TZ")  
    Sys.setenv(TZ = "GMT")
    
    # Print:
    cat(x@FinCenter, "\n", sep = "")
    layout = paste("[", as.character(x@Data), "]", sep = "")
    
    # timeDate:
    Sys.setenv(TZ = myTZ)
    print(layout, quote = FALSE)
    
    # Control:
    control = attr(x, "control")
    if (!is.null(control)) print(control)
    
    # Return Value:
    invisible(x)
}


setMethod("show", "timeDate", show.timeDate)


# ------------------------------------------------------------------------------


plot.timeDate =
function(x, y, ...)
{   # A function implemented by Diethelm Wuertz

    # Note:
    #   Doesn't yet support the features of timeDate objects ...
 
    # Plot:
    plot(as.POSIXct(x), y, ...)
}


# ------------------------------------------------------------------------------


points.timeDate =
function(x, y, ...)
{   # A function implemented by Diethelm Wuertz

    # Note:
    #   Doesn't yet support the features of timeDate objects ...
    
    # Add Points:
    points(as.POSIXct(x), y, ...)
}


# ------------------------------------------------------------------------------


lines.timeDate =
function(x, y, ...)
{   # A function implemented by Diethelm Wuertz

    # Note:
    #   Doesn't yet support the features of timeDate objects ...
    
    # Add Lines:
    lines(as.POSIXct(x), y, ...)
}


# ------------------------------------------------------------------------------


summary.timeDate =
function(object, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Summarizes details of a 'timeDate' object

    # Arguments:
    #   x - a 'timeDate' object to be summarized.

    # Effect:
    #   Produce a summary report of the details of a 'timeDate'
    #   object.

    # Check Time Zone:
    TZ <- Sys.getenv("TZ")
    if(TZ[[1]] != "GMT") {
        Sys.setenv(TZ = "GMT")
        on.exit(Sys.setenv(TZ = TZ))
    }

    # Print:
    x = object
    cat(  "Object:       ", as.character(match.call())[2])
    cat("\nStart Record: ", as.character(start(x)))
    cat("\nEnd Record:   ", as.character(end(x)))
    cat("\nObservations: ", length(as.character(x)))
    cat("\nFormat:       ", x@format)
    cat("\nFinCenter:    ", x@FinCenter)
    cat("\n")

    # Return Value:
    invisible(object)
}


# ------------------------------------------------------------------------------


format.timeDate =
function(x, ...)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Formats 'timeDate' as ISO conform character string

    # Arguments:
    #   x - a 'timeDate' object

    # Value:
    #   Returns an ISO conform formatted character string.

    # Check Time Zone:
    TZ <- Sys.getenv("TZ")
    if(TZ[[1]] != "GMT") {
        Sys.setenv(TZ = "GMT")
        on.exit(Sys.setenv(TZ = TZ))
    }

    # Format - format.POSIXlt(x, format = "", usetz = FALSE, ...)
    format.POSIXct(x@Data, ...)
}


################################################################################

