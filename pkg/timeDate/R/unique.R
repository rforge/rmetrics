
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

# Copyrights (C)
# for this R-port: 
#   1999 - 2008, Diethelm Wuertz, Rmetrics Foundation, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# MEHODS:                   DESCRIPTION:
#  unique.timeDate           Makes a 'timeDate' object unique
################################################################################


unique.timeDate <- 
    function(x, ...) 
{   
    # A function Implemented by Diethelm Wuertz

    # Description:
    #   Returns a timeDate object with duplicate entries removed
    
    # Arguments:
    #   x - an object of class timeDate
    #   incomparables - not used

    # FUNCTION:
    
    # Remove Duplicates:    
    GMT = timeDate(x, zone = x@FinCenter, FinCenter = "GMT")
    charvec = unique(as.character(GMT@Data), ...)
    ans = timeDate(charvec, zone = "GMT", FinCenter = x@FinCenter)
    
    # Return Value:
    ans
}

    
################################################################################

