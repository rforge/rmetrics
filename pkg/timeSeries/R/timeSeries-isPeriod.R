#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  ../../COPYING


################################################################################
# FUNCTION:                 DESCRIPTION:
#  .isDaily
#  .isDaily.default           Default Method returning NA
#  .isDaily.timeDate          Decides if a timeDate has daily time stamps
#  .isDaily.timeSeries        Decides if a timeSeries has daily time stamps
#  .isMonthly
#  .isMonthly.default         Default Method returning NA
#  .isMonthly.timeDate        Decides if a timeDate has monthly time stamps
#  .isMonthly.timeSeries      Decides if a timeSeries has monthly time stamps
#  .isQuarterly
#  .isQuarterly.default       Default Method returning NA
#  .isQuarterly.timeDate      Decides if a timeDate has quarterly time stamps
#  .isQuarterly.timeSeries    Decides if a timeSeries has quarterly time stamps
#
#  .frequency.timeDate        Returns the frequency of a timeDate object
#  .frequency.timeSeries      Returns the frequency of a timeSeries object
################################################################################


# DW:
#   timeDate functions should be moved to package timeDate


# ------------------------------------------------------------------------------


.isDaily <- 
function(x) 
{
    # A function implemented by Diethelm Wuertz
    
    # FUNCTION:
    
    # Return Value:
    UseMethod(".isDaily")
}
  

# ------------------------------------------------------------------------------


.isDaily.default <- 
function(x) 
{
    # A function implemented by Diethelm Wuertz
    
    # FUNCTION:
  
    # Return Value:  
    NA
}
  

# ------------------------------------------------------------------------------  


.isDaily.timeDate <-
function(x)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Decides if a timeDate object has daily time stamps
    
    # Arguments:
    #   x - an object of class timeDate
    
    # FUNCTION:
    
    # Daily ?
    .isDaily = FALSE
    charvec = as.character(x)
    s = substr(charvec, 1, 10)
    test = length(s)/length(unique(sort(s))) 
    if(test == 1) .isDaily = TRUE
    
    # Return Value:
    .isDaily
}


# ------------------------------------------------------------------------------


.isDaily.timeSeries <- 
function(x) 
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Decides if a timeSeries object has daily time stamps
    
    # FUNCTION:
    
    # Return Value:
    .isDaily.timeDate(time(x))
}



################################################################################


.isMonthly <- 
function(x) 
{
    # A function implemented by Diethelm Wuertz
    
    # FUNCTION:
    
    # Return Value:
    UseMethod(".isMonthly")
}


# ------------------------------------------------------------------------------


.isMonthly.default <- 
function(x) 
{
    # A function implemented by Diethelm Wuertz
    
    # FUNCTION:
    
    # Return Value:
    NA
}


# ------------------------------------------------------------------------------


.isMonthly.timeDate <-
function(x)
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Decides if a timeDate object has monthly time stamps
    
    # Arguments:
    #   x - an object of class timeDate
    
    # FUNCTION:
    
    # Monthly ?
    .isMonthly = FALSE
    charvec = as.character(x)
    s = substr(charvec, 1, 7)
    test = length(s)/length(unique(sort(s))) 
    if(test == 1) .isMonthly = TRUE
    
    # Return Value:
    .isMonthly
}


# ------------------------------------------------------------------------------


.isMonthly.timeSeries <- 
function(x) 
{
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Decides if a timeSeries object has monthly time stamps
    
    # FUNCTION:
    
    # Return Value:
    .isMonthly.timeDate(time(x))
}


################################################################################


.isQuarterly <- 
function(x) 
{
    # A function implemented by Diethelm Wuertz
    
    # FUNCTION:
    
    # Return Value:
    UseMethod(".isQuarterly")
}


# ------------------------------------------------------------------------------


.isQuarterly.default <- 
function(x) 
{
    # A function implemented by Diethelm Wuertz
    
    # FUNCTION:
    
    # Return Value:
    NA
}


# ------------------------------------------------------------------------------


.isQuarterly.timeDate <-
function(x)
{
    # A function implemented by Diethelm Wuertz
    
    # Descriptions:
    #   Decides if a timeDate object has quarterly time stamps
    
    # Arguments:
    #   x - an object of class timeDate
    
    # FUNCTION:
    
    # Quarterly ?
    .isQuarterly = FALSE
    charvec = as.character(x)
    s = substr(charvec, 1, 7)
    S = substr(as.character(timeLastDayInQuarter(x)), 1, 7)
    test = length(s)/length(unique(sort(S))) 
    if(test == 1) .isQuarterly = TRUE
    
    # Return Value:
    .isQuarterly
}


# ------------------------------------------------------------------------------


.isQuarterly.timeSeries <- 
function(x) 
{
    # A function implemented by Diethelm Wuertz
    
    # FUNCTION:
    
    # Return Value:
    .isQuarterly(time(x))
}


################################################################################


.frequency.timeDate <-
function(x)
{
    # A function implemented by Diethelm Wuertz
    
    # Descriptions:
    #   Returns the frequency of a timeDate object
    
    # Arguments:
    #   x - an object of class timeDate
    
    # FUNCTION:
    
    # Frequency:
    frequency = 1
    if(.isQuarterly(x)) frequency = 4
    if(.isMonthly(x)) frequency = 12
    
    # Return Value:
    frequency
}


# ------------------------------------------------------------------------------


.frequency.timeSeries <-
function(x)
{
    # A function implemented by Diethelm Wuertz
    
    # Descriptions:
    #   Returns the frequency of a timeSeries object
    
    # Arguments:
    #   x - an object of class timeSeries
    
    # FUNCTION:
    
    # Frequency:
    frequency = 1
    if(.isQuarterly(x)) frequency = 4
    if(.isMonthly(x)) frequency = 12
    
    # Return Value:
    frequency
}


################################################################################

