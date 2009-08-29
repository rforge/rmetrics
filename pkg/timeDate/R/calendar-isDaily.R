
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
# FUNCTION:                DESCRIPTION:
#  isDaily                  Tests if a date/time vector has daily time stamps
#  isDaily.timeDate         Tests if a timeDate object has daily time stamps
################################################################################


isDaily <- 
function(x) 
{
    # A function implemented by Diethelm Wuertz
    
    UseMethod("isDaily")
}
    

# ------------------------------------------------------------------------------


isDaily.timeDate <-
function(x)
{
    # A function implemented by Diethelm Wuertz
    
    # Descriptions:
    #   Test if a timeDate object has daily time stamps
    
    # Example:
    #   isDaily(timeSequence(by = "day", length.out = 20))
    #   isDaily(timeCalendar())
    #   isDaily(timeSequence(by = "hour", length.out = 100))
    
    # Details:
    #   Definition: A timeDate Object is a Daily timeDate object 
    #   if we have not more than one date/time stamp per day.
    
    # Arguments:
    #   x - an object of class timeDate
    
    # FUNCTION:
    
    # Daily ?
    isDaily = FALSE
    charvec = as.character(x)
    s = substr(charvec, 1, 10)
    test = length(s)/length(unique(sort(s))) 
    if(test == 1) isDaily = TRUE
    
    # Return Value:
    isDaily
}


################################################################################

