
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
#   1999 - 2004, Diethelm Wuertz, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:           HOLIDAY CALENDAR FUNCTIONS:
#  easter              Returns date of easter or related feasts as '.sdate'
#  .easter.sunday       Easter Algorithm
#  holiday             Returns a holiday date of G7 and CH as '.sdate'
# FUNCTION:           TIME DATE HOLIDAY CALENDARS:
#  holiday.NYSE        Returns 'timeDate' object for full-day NYSE holidays
# FUNCTION:           DESCRIPTION:
#  .on.or.after        Computes date in month that is a nday ON OR AFTER date
#  .on.or.before       Computes date in month that is a nday ON OR BEFORE date
#  .nth.of.nday        Computes nth ocurrance of a nday in year/month
#  .last.of.nday       Computes the last nday in year/month
# FUNCTION:           DESCRIPTION:
#  .sjulian            Computes Julian day numbers from ISO-8601 dates
#  .sdate              Computes ISO-8601 dates from Julian day numbers
#  .sday.of.week       Computes day of the week for ISO-8601 dates 
#  .sleap.year         Returns TRUE/FALSE if dates belong to leap years or not
#  .print.sdate        Print method for objects of class ".sdate"
# FUNCTION:           DESCRIPTION:
#  fjulian             Transform formatted dates to julian day numbers
# FUNCTION:           DESCRIPTION:
#  .julian             Implements SPlus like 'julian'
#  month.day.year      Implements SPlus like 'month.day.year'
#  leap.year           Implements SPlus like 'leap.year'
#  day.of.week         Implements SPlus like 'day.of.week'
################################################################################


test.helpFile = 
function()
{
    # Help File:
    helpFile = function() { 
        example(HolidayCalendars); return() }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
} 
 

# ------------------------------------------------------------------------------


test.holidayCalendars = 
function()
{

    # easter -
    # easter(year = currentYear, shift = 0)
    # Dates for Easter and Good Friday from 2000 until 2010:
    easter()
    easter = easter(2000:2010)
    easter
    checkTrue(inherits(easter, "timeDate"))
    
    goodfriday = easter(2000:2010, -2) 
    goodfriday
    checkIdentical(easter, goodfriday + 2*24*3600)
    
    HD = holiday(2000:2010, Easter)     
    HD
    checkTrue(inherits(HD, "timeDate"))      
    HD = holiday(2000:2010, GoodFriday)  
    HD
    checkTrue(inherits(HD, "timeDate"))
   
    # holidays -   
    Easter(2000:2010)                    
    GoodFriday(2000:2010)
  
    # holiday.NYSE -
    HD = holiday.NYSE(currentYear)
    HD
    checkTrue(inherits(HD, "timeDate"))
   
    # fjulian -
    fdates = c("8/11/73", "08-11-73", "August 11 1973", "Aug11/73")
    FJ = fjulian(fdates) 
    FJ
    checkIdentical(class(FJ), "numeric")
   
    # fjulian -
    fdates = c("11/8/73", "11-08-73", "11 August 1973", "11Aug73")
    FJ = fjulian(fdates, order = 'dmy')
    FJ 
    
    # .julian - 
    # day.of.week -
    # The number of days from January 1, 1990 to each of:
    # January 15, 1990, February 15, 1991, March 15, 1992, etc.
    .julian(1:12, rep(15,12), 1990+(0:11), origin = c(1, 1, 1990))
    # November 12, 98, was a Wednesday.
    day.of.week(m = 11, d = 12, y = 98)
   
    # Return Value:
    return()
    
}


# ------------------------------------------------------------------------------
   

if (FALSE) {
    testResult <- runTestFile("C:/Rmetrics/trunk/fCalendar/test/runit025B.R")
    printTextProtocol(testResult)
}


################################################################################
   
    