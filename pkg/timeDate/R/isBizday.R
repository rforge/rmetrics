
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
# METHODS:                  DESCRIPTION:
#  isBizday                  Tests if a date is a business day or not
#  isHoliday                 Tests if a date is a non-business day or not
################################################################################


isBizday = 
    function(x, holidays = holidayNYSE()) 
{   
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Test if a date is a business day or not
    
    # Arguments:
    #   x - an object of class "timeDate"
    #   holidays - a holiday calendar
    
    # Value:
    #   Returns a logical or a vector of logicals
    
    # Example:
    #   x = timeSequence(from = "2005-05-15", to = "2005-07-15")
    #   h = holiday.NYSE(2005)
    #   cbind(as.character(x), is.bizday(x, h))
    
    # FUNCTION:
    
    # Test:
    char.x = substr(as.character(x), 1, 10)
    char.h = substr(as.character(holidays), 1, 10)
    Weekday = as.integer(isWeekday(x))
    nonHoliday = as.integer(!(char.x %in% char.h))
    
    # Business Days:
    bizdays = as.logical(Weekday*nonHoliday)
    names(bizdays) = x@Data
    
    # Return Value:
    bizdays
} 


# ------------------------------------------------------------------------------


isHoliday = 
    function(x, holidays = holidayNYSE()) 
{   
    # A function implemented by Diethelm Wuertz

    # Description:
    #   Test if a date is a holiday or not
    
    # Arguments:
    #   x - an object of class "timeDate"
    #   holidays - a holiday calendar
    
    # Value:
    #   Returns a logical or a vector of logicals
    
    # FUNCTION:
    
    # Return Value:
    return(!isBizday(x, holidays)) 
}   


################################################################################

