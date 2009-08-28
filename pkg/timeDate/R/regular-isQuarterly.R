
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
# FUNCTION:               DESCRIPTION:
#  isQuarterly             Tests if a date/time vector has quarterly time stamps
#  isQuarterly.timeDate    Tests if a timeDate object has quarterly time stamps
################################################################################


isQuarterly <- 
function(x) 
{
    # A function implemented by Diethelm Wuertz
    
    UseMethod("isQuarterly")
}


# ------------------------------------------------------------------------------


isQuarterly.timeDate <-
function(x)
{
    # A function implemented by Diethelm Wuertz
    
    # Descriptions:
    #   Tests if a timeDate object has quarterly time stamps
    
    # Arguments:
    #   x - an object of class timeDate
    
    # Details:
    #   Definition: A timeDate Object is a Quarterly timeDate object 
    #   if we have not more than one date/time stamp per quarter
    #   Note a quarterly series is also a daily and a monthly series.
    
    # Example:
    #   isQuarterly(timeSequence(by = "day", length.out = 20))
    #   isQuarterly(timeCalendar())
    #   isQuarterly(timeSequence(by = "hour", length.out = 100))
    #   isQuarterly(timeCalendar()[(1:4)*3])
    #   isMonthly(timeCalendar()[(1:4)*3])
    #   isDaily(timeCalendar()[(1:4)*3])
    
    # FUNCTION:
    
    # Quartertly ?
    isQuarterly = FALSE
    charvec = as.character(x)
    s = substr(charvec, 1, 7)
    S = substr(as.character(timeLastDayInQuarter(x)), 1, 7)
    test = length(s)/length(unique(sort(S)))
    if(test == 1) isQuarterly = TRUE 
    
    # Return Value:
    isQuarterly
}


################################################################################

