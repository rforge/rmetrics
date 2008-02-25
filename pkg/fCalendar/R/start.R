
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
#  start.timeDate            Extracts the first entry of a 'timeDate' object
#  end.timeDate              Extracts the last entry of a 'timeDate' object
#  range.timeDate            Extracts range of a 'timeDate' object
################################################################################


start.timeDate <- 
    function(x, ...)
{   
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Extracts the first object of a 'timeDate' vector

    # Arguments:
    #   x - a 'timeDate' object
    
    # Value:
    #   Returns from 'x' the earliest entry as an object of class 
    #   'timeDate'.
    
    # FUNCTION:
    
    # Start:
    ans = sort(x)[1]
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


end.timeDate <- 
    function(x, ...)
{   
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Extracts the last object of a 'timeDate' vector

    # Arguments:
    #   x - a 'timeDate' object
    
    # Value:
    #   Returns an object of class 'timeDate'.

    # FUNCTION:
    
    # Start:
    ans = sort(x)[length(x)]
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


min.timeDate <- function(..., na.rm = FALSE) start.timeDate(...)
min.timeDate <- function(..., na.rm = FALSE) end.timeDate(...)

range.timeDate <- function(..., na.rm = FALSE) {
    c(start(..., na.rm = na.rm), end(..., na.rm = na.rm)) }


################################################################################

