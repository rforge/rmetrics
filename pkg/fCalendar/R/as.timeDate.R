
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
# MEHOD:                    COERCION AND OBJECT TRANSFORMATIONS:
#  as.timeDate               Implements Use Method
#  as.timeDate.default       Default Method
#  as.timeDate.POSIXt        Returns a 'POSIX' object as 'timeDate' object
#  as.timeDate.Date          Returns a 'POSIX' object as 'timeDate' object
################################################################################


as.timeDate = 
function(x, zone = NULL, FinCenter = NULL) 
{
    UseMethod("as.timeDate")
}


# ------------------------------------------------------------------------------


as.timeDate.default = 
function(x, zone = myFinCenter, FinCenter = myFinCenter) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns default object as 'timeDate' object
    
    # Arguments:
    #   x - a 'timeDate' object
    
    # Value:
    #   Returns 'x' as a 'timeDate' object. 
    
    # FUNCTION:
    
    # as timeDate:
    ans = timeDate(charvec = as.character(x), 
        zone = zone, FinCenter = FinCenter)
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


as.timeDate.POSIXt = 
function(x, zone = myFinCenter, FinCenter = myFinCenter) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns a 'POSIXt' object as 'timeDate' object
    
    # Arguments:
    #   x - a 'timeDate' object
    
    # Value:
    #   Returns 'x' as a 'timeDate' object. 
    
    # FUNCTION:
    
    # as timeDate:
    ans = timeDate(charvec = x, zone = zone, FinCenter = FinCenter)
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


 
as.timeDate.Date = 
function(x, zone = myFinCenter, FinCenter = myFinCenter) 
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns a 'Date' object as 'timeDate' object
    
    # Arguments:
    #   x - a 'timeDate' object
    #   ... - arguments passed to other methods.
    
    # Value:
    #   Returns 'x' as a character vector. 
    
    # FUNCTION:
    
    # as timeDate:
    ans = timeDate(charvec = x, zone = zone, FinCenter = FinCenter)
    
    # Return Value:
    ans
}
    

################################################################################

