
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
#   1999 - 2006, Diethelm Wuertz, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# S3 METHODS:            TEST AND REPRESENTATION OF OBJECTS:
#  isWeekday              Tests if a date is a weekday or not
#  isWeekend              Tests if a date falls on a weekend or not
#  isBizday               Tests if a date is a business day or not
#  isHoliday              Tests if a date is a non-business day or not
#  getDayOfWeek           Returns the day of the week to a 'timeDate' object
#  getDayOfYear           Returns the day of the year to a 'timeDate' object
# S3 MEHOD:              SUBSETTING TIMEDATE OBJECTS:
#  [.timeDate             Extracts or replaces subsets from 'timeDate' objects
#  cut.timeDate           Extracts a piece from a 'timeDate' object
#  start.timeDate         Extracts the first entry of a 'timeDate' object
#  end.timeDate           Extracts the last entry of a 'timeDate' object
#  blockStart             Creates start dates for equally sized blocks
#  blockEnd               Creates end dates for equally sized blocks
################################################################################


test.helpFile = 
function()
{
    # Help File:
    helpFile = function() { 
        example(TimeDateSubsets); return() }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
} 


# ------------------------------------------------------------------------------
      
 
test.subsets = 
function()
{   
    #  isWeekday              Tests if a date is a weekday or not
    #  isWeekend              Tests if a date falls on a weekend or not
    #  isBizday               Tests if a date is a business day or not
    #  isHoliday              Tests if a date is a non-business day or not
    #  getDayOfWeek           Returns the day of the week to a 'timeDate' object
    #  getDayOfYear           Returns the day of the year to a 'timeDate' object
    
    #  [.timeDate             Extracts or replaces subsets from 'timeDate' objects
    #  cut.timeDate           Extracts a piece from a 'timeDate' object
    #  start.timeDate         Extracts the first entry of a 'timeDate' object
    #  end.timeDate           Extracts the last entry of a 'timeDate' object
    #  blockStart             Creates start dates for equally sized blocks
    #  blockEnd               Creates end dates for equally sized blocks
    
    
    ## Easter -
    myFinCenter
    Easter(2006)
    target = timeSequence(from = Easter(currentYear)-7*24*3600, length.out = 8) 
    target
    charvec = c("2006-04-09", paste("2006-04-1", 0:6, sep = ""))
    current = timeDate(charvec)
    current
    checkIdentical(target, current)
    
    ## Weekdays and Weekend Days: 
    tS = timeSequence(from = Easter(currentYear)-7*24*3600, length.out = 8)
    isWeekday(tS)
    isWeekend(tS)
    getDayOfWeek(tS)
    
    ## Business Days and Holidays:
    holiday.NYSE()
    getDayOfWeek(tS)
    Easter(2006)
    isBizday(tS, holiday.NYSE())
    isHoliday(tS, holiday.NYSE())
    
    ## [ - Subsetting:
    tS[c(1, 6:8)]   
    tS[isBizday(tS)]
    tS[isHoliday(tS)]
    
    ## cut - 
    GF = GoodFriday(2006)
    GF
    EM = EasterMonday(2006)
    EM
    target = cut(tS, from = GF, to = EM)
    target
    charvec = paste("2006-04-1", 4:6, sep = "")
    current = timeDate(charvec)
    current
    checkIdentical(target, current)
    
    ## start - 
    ts = timeCalendar()
    target = start(tS)
    target
    # current = sort(tS)[1]
    # current
    # checkIdentical(target, current)
    
    ## end - 
    target = end(tS)
    target
    # current = rev(sort(tS))[1]
    # current
    # checkIdentical(target@Data, current@Data)
    
    ## head | tail -
    # not yet available
    
    ## order | sample | uniq -
    # not yet available
   
    # Return Value:
    return()
}
 

# ------------------------------------------------------------------------------


if (FALSE) {
    testResult <- runTestFile("C:/Rmetrics/trunk/fCalendar/test/runit023B.R")
    printTextProtocol(testResult)
}


################################################################################
   
    