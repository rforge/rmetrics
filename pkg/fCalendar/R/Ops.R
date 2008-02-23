
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
#   1999 - Diethelm Wuertz, GPL
#   2007 - Rmetrics Foundation, GPL
#   Diethelm Wuertz <wuertz@phys.ethz.ch>
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# MEHODS:                   DESCRIPTION:
#  Ops.timeDate              Group 'Ops' operations on 'timeDate' objects
#  +.timeDate                Performs + operation on 'timeDate' objects
#  -.timeDate                Performs - operation on 'timeDate' objects
################################################################################


Ops.timeDate <- 
    function(e1, e2)
{   
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Uses group 'Ops' generic functions for 'timeDate' objects

    # Arguments:
    #   e1 - an object of class 'timeDate'
    #   e2 - an object of class 'timeDate' 
    
    # Value:
    #   Returns the 'Ops' grouped object.
    
    # FUNCTION:
    
    # Set Timezone to GMT:
    myTZ = Sys.getenv("TZ")  
    Sys.setenv(TZ = "GMT")
    
    # Check Logical Operators:
    if (nargs() == 1)
        stop(paste("unary", .Generic, "not defined for timeDate objects"))
    boolean <- switch(.Generic, "<" = , ">" = , "==" = ,
        "!=" = , "<=" = , ">=" = TRUE, FALSE)
    if (!boolean) 
        stop(paste(.Generic, "not defined for timeDate XXX objects"))   
        
    # Convert to GMT:
    e1GMT = timeDate(e1, zone = e1@FinCenter, FinCenter = "GMT")@Data
    e2GMT = timeDate(e2, zone = e2@FinCenter, FinCenter = "GMT")@Data
    
    # Convert to Julian:
    if (inherits(e1GMT, "POSIXct")) e1 <- as.POSIXct(e1GMT)
    if (inherits(e2GMT, "POSIXct")) e2 <- as.POSIXct(e2GMT)
    
    # Return Value:
    Sys.setenv(TZ = myTZ)
    NextMethod(.Generic)
}


# ------------------------------------------------------------------------------


"+.timeDate" <- 
    function(e1, e2)
{   
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Performs arithmetic "+" operation on 'timeDate' objects.
    
    # Arguments:
    #   e1 - an object of class 'timeDate'
    #   e2 - an object of class 'numeric'
    
    # Value:
    #   Returns a 'timeDate' object "e2" seconds later than the
    #   'timeDate' object "e1". 
    
    # FUNCTION:
    
    # Set Timezone to GMT:
    myTZ = Sys.getenv("TZ")  
    Sys.setenv(TZ = "GMT")
    
    # Check Class Types:
    test1 = test2 = 1
    if (inherits(e1, "timeDate")) test1 = 0
    if (inherits(e2, "numeric"))  test2 = 0
    if (test1 + test2 != 0) stop("Wrong class types") 
    
    # Convert to GMT:
    e1GMT = timeDate(e1, zone = e1@FinCenter, FinCenter = "GMT")@Data
    
    # Add and Convert back to FinCenter:
    ans = timeDate(e1GMT+e2, zone = "GMT", FinCenter = e1@FinCenter)
    
    # Return Value:
    Sys.setenv(TZ = myTZ)
    ans
}


# ------------------------------------------------------------------------------


"-.timeDate" <-  
    function(e1, e2)
{   
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Performs arithmetic "-" operation on 'timeDate' objects
    
    # Arguments:
    #   e1 - an object of class 'timeDate'
    #   e2 - an object of class 'timeDate' or of class 'numeric'
    
    # Value:
    #   Returns a 'difftime' object if both "e1" and "e2" are
    #   'timeDate' objects, or returns a 'timeDate' object "e2"
    #   seconds earlier than "e1".
    
    # Example:
    #   charvec = c("2004-01-01 16:00:00", "2004-01-01 18:00:00")
    #   x = timeDate(charvec, zone = "GMT", FinCenter = "Europe/Zurich")
    
    # FUNCTION:
     
    ## Set Timezone to GMT:
    myTZ = Sys.getenv("TZ")  
    Sys.setenv(TZ = "GMT")
    
    # Check Class Types:
    test1 = test2 = 1
    if (inherits(e1, "timeDate")) test1 = 0
    if (inherits(e2, "timeDate")) test2 = 0
    if (inherits(e2, "numeric"))  test2 = 0
    if (test1 + test2 != 0) stop("Wrong class types") 
    
    # First Object:
    e1GMT = timeDate(e1, zone = e1@FinCenter, FinCenter = "GMT")@Data
    if (inherits(e2, "timeDate")) {
        e2 = timeDate(e2, zone = e2@FinCenter, FinCenter = "GMT")@Data
        # Returns difftime:
        return(e1GMT-e2) }
    if (inherits(e2, "numeric")) {
        # Returns timeDate:
        return(timeDate(e1GMT-e2, zone = "GMT", FinCenter = e1@FinCenter)) }
        
    # Return Value:
    Sys.setenv(TZ = myTZ)
    invisible()         
}


################################################################################

