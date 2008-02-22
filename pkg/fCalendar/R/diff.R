
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
# MEHODS:                   MATHEMATICAL OPERATIONS:
#  diff.timeDate             Returns suitably lagged and iterated differences
#  difftimeDate              Returns a difference of two 'timeDate' objects
################################################################################


diff.timeDate <- 
    function (x, lag = 1, differences = 1, ...) 
{   
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns suitably lagged and iterated differences
    
    # Arguments:
    #   x - a 'timeDate' object.
    #   lag - an integer indicating which lag to use, by 
    #       default 1.
    #   differences - an integer indicating the order of the 
    #       difference, by default 1.
    #   ... - further arguments to be passed to or from methods.
  
    # Value:
    #   If 'x' is a vector of length 'n' and 'differences=1', then 
    #   the computed result is equal to the successive differences
    #   'x[(1+lag):n] - x[1:(n-lag)]'.
    #   If 'difference' is larger than one this algorithm is applied
    #   recursively to 'x'. Note that the returned value is a vector 
    #   which is shorter than 'x'.

    # FUNCTION:
    
    # Set Timezone to GMT:
    myTZ = Sys.getenv("TZ")  
    Sys.setenv(TZ = "GMT")
    
    # Convert to GMT:
    GMT = timeDate(x, zone = x@FinCenter, FinCenter = "GMT") 
    ans = diff.POSIXt(as.POSIXct(GMT@Data), 
        lag = lag, differences = differences, ...) 
        
    # Reset Timezone:
    Sys.setenv(TZ = myTZ)

    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


difftimeDate <- 
    function(time1, time2, 
    units = c("auto", "secs", "mins", "hours", "days", "weeks")) 
{   
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Takes a difference of two 'timeDate' objects
    
    # Arguments:
    #   time1, time2 - 'timeDate' objects.
    #   units - a character string. The units in which the results 
    #       are desired, one of the following: "auto", "secs", 
    #       "mins", "hours", "days", "weeks"
    
    # Value:
    #   'difftimeDate' takes a difference of two 'timeDate' 
    #   objects and returns an object of class 'difftime' with
    #   an attribute indicating the units. 

    # FUNCTION:
    
    # Set Timezone to GMT:
    myTZ = Sys.getenv("TZ")  
    Sys.setenv(TZ = "GMT")
    
    # Convert to GMT:
    time1GMT = timeDate(time1, zone = time1@FinCenter, 
        FinCenter = "GMT") 
    time2GMT = timeDate(time2, zone = time2@FinCenter, 
        FinCenter = "GMT") 

    # Return Value:
    Sys.setenv(TZ = myTZ)
    difftime(time1GMT@Data, time2GMT@Data, tz = "GMT", units = units[1]) 
}

    
################################################################################

