
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
#  isDaily                  Tests if a time series is a daily series
#  isMonthly                Tests if a time series is a monthly series
#  isQuarterly              Tests if a time series is a quarterly series
#  isRegular                Tests if a time series is a regular series
#  frequency                Returns the frequency of a regular time series  
################################################################################


isDaily.timeSeries <- 
function(x) 
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Tests if a time series is a daily series   
    
    # FUNCTION:
    
    # Check:
    ans <- isDaily(time(x))
    
    # Return Value:
    ans
}
    

# ------------------------------------------------------------------------------


isMonthly.timeSeries <- 
function(x) 
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Tests if a time series is a monthly series  
    
    # FUNCTION:
    
    # Check:
    ans <- isMonthly(time(x))
    
    # Return Value:
    ans
}
    

# ------------------------------------------------------------------------------


isQuarterly.timeSeries <- 
function(x) 
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Tests if a time series is a quarterly series  
    
    # FUNCTION:
    
    # Check:
    ans <- isQuarterly(time(x))
    
    # Return Value:
    ans
}
    

# ------------------------------------------------------------------------------


isRegular.timeSeries <-
function(x)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Tests if a time series is a regular series  
    
    # FUNCTION:
    
    # Check:
    ans <- isRegular(time(x))
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


frequency.timeSeries <-
function(x, ...)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Returns the frequency of a regular time series  
    
    # FUNCTION:
    
    # Check:
    ans <- frequency(time(x))
    
    # Return Value:
    ans
}
        


################################################################################

