
# This R package is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This R package is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the 
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General 
# Public License along with this R package; if not, write to the 
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
# MA  02111-1307  USA


################################################################################
# FUNCTION:                 DESCRIPTION:
#  dayOfYear                 Returns the day of the year to a 'timeDate' object
################################################################################


dayOfYear <- 
    function(x)
{   
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Returns day of week for time date objects
    
    # Arguments:
    #   x - an object of class "timeDate"
     
    # FUNCTION:
    
    # Assign:
    yd = 1:366
    n = as.POSIXlt(x@Data)$yday + 1
    ydays = yd[n]
    names(ydays) = as.character(x@Data)
    
    # Return Value:
    ydays
}   


################################################################################

