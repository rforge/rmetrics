
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
#  isMonthly                 Tests if a date/time vector has monthly time stamps
#  isMonthly.timeDate        Tests if a timeDate object has monthly time stamps
################################################################################


isMonthly <- 
function(x) 
{
    # A function implemented by Diethelm Wuertz
    
    UseMethod("isMonthly")
}


# ------------------------------------------------------------------------------


isMonthly.timeDate <-
function(x)
{
    # A function implemented by Diethelm Wuertz
    
    # Descriptions:
    #   Tests if a timeDate object has monthly time stamps
    
    # Arguments:
    #   x - an object of class timeDate
    
    # Details:
    #   Definition: A timeDate Object is a Monthly timeDate object 
    #   if we have not more than one date/time stamp per month.
    #   Note a monthly series is also a daily series.
    
    # Example:
    #   isMonthly(timeSequence(by = "day", length.out = 20))
    #   isMonthly(timeCalendar())
    #   isDaily(timeCalendar())
    #   isMonthly(timeSequence(by = "hour", length.out = 100))
    
    # FUNCTION:
    
    # Monthly ?
    isMonthly = FALSE
    charvec = as.character(x)
    s = substr(charvec, 1, 7)
    test = length(s)/length(unique(sort(s))) 
    if(test == 1) isMonthly = TRUE
    
    # Return Value:
    isMonthly
}



################################################################################

