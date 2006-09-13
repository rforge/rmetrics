
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


test.subsetTests = 
function()
{   
    #  isWeekday              Tests if a date is a weekday or not
    #  isWeekend              Tests if a date falls on a weekend or not
    #  isBizday               Tests if a date is a business day or not
    #  isHoliday              Tests if a date is a non-business day or not
    #  getDayOfWeek           Returns the day of the week to a 'timeDate' object
    #  getDayOfYear           Returns the day of the year to a 'timeDate' object
    
    # Easter() Function:
    myFinCenter = "Zurich"
    target = timeSequence(from = Easter(2006)-7*24*3600, length.out = 8) 
    print(target)
    charvec = c(
        "2006-04-09", "2006-04-10", "2006-04-11", "2006-04-12", "2006-04-13", 
        "2006-04-14", "2006-04-15", "2006-04-16")
    current = timeDate(charvec)
    print(current)
    checkIdentical(target, current)
    
    # Weekdays:
    tS = timeSequence(from = Easter(2006)-7*24*3600, length.out = 8)
    WD = isWeekday(tS)
    current = c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE) 
    checkIdentical(as.logical(WD), current)
    
    # Weekends:
    tS = timeSequence(from = Easter(2006)-7*24*3600, length.out = 8)
    WE = isWeekend(tS)
    current = !c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE) 
    checkIdentical(as.logical(WE), current)
    
    # Day of Week:
    tS = timeSequence(from = Easter(2006)-7*24*3600, length.out = 8)
    DOW = getDayOfWeek(tS)
    current = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
    checkIdentical(as.character(DOW), current)
    
    # NYSE Business Days - Dates:
    NYSE = holidayNYSE(2006)
    charvec = c(
        "2006-01-02", "2006-01-16", "2006-02-20", "2006-04-14", "2006-05-29", 
        "2006-07-04", "2006-09-04", "2006-11-23", "2006-12-25")
    checkIdentical(format(NYSE), charvec)
    
    # NYSE Business Days - Day-of-Week:
    DOW = getDayOfWeek(NYSE)
    current = c("Mon", "Mon", "Mon", "Fri", "Mon", "Tue", "Mon", "Thu", "Mon")
    checkIdentical(as.character(DOW), current)
    
    # Holidays:
    TD = Easter(2006)
    checkIdentical(format(TD), "2006-04-16")

    # Bizdays:
    tS = timeSequence(from = Easter(2006)-7*24*3600, length.out = 8)
    target = isBizday(tS, holidayNYSE(2006))
    current = c(FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)
    names(current) = names(target)
    checkIdentical(target, current)
    
    # Holidays:
    tS = timeSequence(from = Easter(2006)-7*24*3600, length.out = 8)
    target = isHoliday(tS, holidayNYSE(2006))
    current = !c(FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)
    names(current) = names(target)
    checkIdentical(target, current)
    
    # Return Value:
    return()
}
 

# ------------------------------------------------------------------------------


test.subsetExtracts = 
function()
{   
    #  [.timeDate             Extracts or replaces subsets from 'timeDate' objects
    #  cut.timeDate           Extracts a piece from a 'timeDate' object
    #  start.timeDate         Extracts the first entry of a 'timeDate' object
    #  end.timeDate           Extracts the last entry of a 'timeDate' object
    #  blockStart             Creates start dates for equally sized blocks
    #  blockEnd               Creates end dates for equally sized blocks
    
    # [ - Subsetting:
    tS[c(1, 6:8)]   
    tS[isBizday(tS)]
    tS[isHoliday(tS)]
    
    # cut - 
    GF = GoodFriday(2006)
    print(GF)
    EM = EasterMonday(2006)
    print(EM)
    target = cut(tS, from = GF, to = EM)
    print(target)
    charvec = paste("2006-04-1", 4:6, sep = "")
    current = timeDate(charvec)
    print(current)
    checkIdentical(
        target, 
        current)
    
    # start - 
    tS = timeCalendar()
    target = start(tS)
    print(target)
    checkIdentical(
        format(target), 
        current = format(timeDate("2006-01-01")))
    
    # end -
    tS = timeCalendar() 
    target = end(tS)
    print(target)
    checkIdentical(
        format(target), 
        current = format(timeDate("2006-12-01")))
    
    # head | tail -
    tS = timeCalendar()
    head(tS)
    tail(tS)
    
    # order | sample | uniq -
    tS = timeCalendar()
    # order.timeDate - not yet available
    sample(tS)
    unique(tS)
    # unique(sort(c(tS, tS)))
   
    # Return Value:
    return()
}
 

# ------------------------------------------------------------------------------


if (FALSE) {
    require(RUnit)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fCalendar/test/runit3B.R")
    printTextProtocol(testResult)
}


################################################################################
   
    