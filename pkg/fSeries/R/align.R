
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
# FUNCTION:                 DESCRIPTION:
#  .align.timeSeries         Aligns a timeSeries object
################################################################################


.align.timeSeries = 
function(x, method = c("before", "after", "interp"), startOn = "hours",
by = "30 m")
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Aligns a 'timeSeries' object
    
    # FUNCTION:
 
    # Settings:
    method = match.arg(method)
    numberOfColumns = dim(x)[2]
    typeMethod = c(interp = "linear", before = "constant", after = "constant")
    fMethod = c(interp = 0.5, before = 0, after = 1)
    by = .by2seconds(by)
    
    # Convert to GMT:
    tD = timeDate(x@positions, zone = x@FinCenter, FinCenter = "GMT")

    # Convert to POSIX:
    Positions = as.POSIXct(tD, tz = "GMT")
    N = length(Positions)
    Start = as.POSIXct(trunc(Positions[1], startOn), tz = "GMT")  
    End   = as.POSIXct(trunc(Positions[N], startOn), tz = "GMT") + 24*3600
    print(Start)
    print(End)
    
    # Compute Positions:
    X = as.integer(difftime(Positions, Start, units = "sec"))
    
    # Compute New Positions:
    timeDiff = as.integer(difftime(End, Start, units = "secs"))
    lengthOut = trunc(timeDiff/by) + 1
    posix = seq(from = Start, by = paste(by, "sec"), length.out = lengthOut)
    newX = as.integer(difftime(posix, Start, units = "secs"))  
    
    # Align:  
    matY = NULL
    for (i in 1:numberOfColumns) {
        Y = as.vector(x[, i])
        newY = approx(X, Y, xout = newX, method = typeMethod[method], 
            f = fMethod[method])$y
        matY = cbind(matY, newY)
        
    }

    # Create Series in local time:
    print(head(as.character(posix)))
    ans = timeSeries(matY, as.character(posix), 
        units = x@units, zone = "GMT", FinCenter = x@FinCenter,
        recordIDs = data.frame())
    
    # Return Value:
    ans       
}

   
################################################################################

