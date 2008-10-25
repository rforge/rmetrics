
# Rmetrics is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# Rmetrics is distributed in the hope that it will be useful,
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
#   1999 - 2007, Diethelm Wuertz, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################

   
test.timeLastDayInMonth <- 
    function()
{ 
    # What date has the last day in a month for a given date ?
    charvec = "2006-04-16"
    
    timeLastDayInMonth(charvec, format = "%Y-%m-%d", 
        zone = myFinCenter, FinCenter = myFinCenter)
    
    timeLastDayInMonth(charvec)
    
    timeLastDayInMonth(charvec, FinCenter = "Zurich")
    
    # Return Value:
    return()  
}

    
# ------------------------------------------------------------------------------

   
test.timeFirstDayInMonth <- 
    function()
{ 
    # What date has the first day in a month for a given date ?
    charvec = "2006-04-16"
    timeFirstDayInMonth(charvec)
     
    # Return Value:
    return()  
}


# ------------------------------------------------------------------------------

   
test.timeLastDayInQuarter <- 
    function()
{ 
    # What date has the last day in a quarter for a given date ?
    charvec = "2006-04-16"
    timeLastDayInQuarter(charvec)
    
    # Return Value:
    return()  
}


# ------------------------------------------------------------------------------


test.timeFirstDayInQuarter <- 
    function()
{
    # What date has the first day in a quarter for a given date ?
    charvec = "2006-04-16"
    timeFirstDayInQuarter(charvec)
    
    # Return Value:
    return()  
}


# ------------------------------------------------------------------------------


test.timeNdayOnOrAfter <- 
    function()
{
    # What date has the first Monday on or after March 15, 1986 ?
    timeNdayOnOrAfter("1986-03-15", 1)
    
    # Return Value:
    return()  
}

    
# ------------------------------------------------------------------------------


test.timeNdayOnOrBefore <- 
    function()
{
    # What date has Friday on or before April 22, 1977 ?
    timeNdayOnOrBefore("1986-03-15", 5)
    
    # Return Value:
    return()  
}


# ------------------------------------------------------------------------------


test.timeNthNdayInMonth <- 
    function()
{
    # What date is the second Monday in April 2004 ?
    timeNthNdayInMonth("2004-04-01", 1, 2)
    
    # Return Value:
    return()  
}


# ------------------------------------------------------------------------------


test.timeLastNdayInMonth <- 
    function()
{
    # What date has the last Tuesday in May, 1996 ?
    timeLastNdayInMonth("1996-05-01", 2)
    
    # Return Value:
    return()  
}


################################################################################
    
