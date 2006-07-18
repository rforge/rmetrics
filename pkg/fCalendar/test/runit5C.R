
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
# Holiday Database:
# Copyright 1997, Diethelm Wuertz
#   www.rmetrics.org
# Required "Holiday" Functions:
#   "easter", ".on.or.after", ".nth.of.nday", ".last.of.nday", 
# The functions return an object of class ".sdate"
#   ISO-8601 formatted integers, i.e. CCYYMMDD
################################################################################


test.helpFile = 
function()
{
    # Help File:
    helpFile = function() { 
        example(HolidayDates); return() }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.holidayList =
function()
{
   currentYear          
   easter()            
   easter(2000:2009)
   class(easter())    

   Holiday = levels(.holidayList()$HOLIDAYS)
   for ( i in 1:115) {
       print(Holiday[i])
       print(holiday(Holiday = Holiday[i]))
   }
   
   # Return Value:
   return()
}   


# ------------------------------------------------------------------------------


test.holidayDates =
function()
{

    # Holiday List:
    List = .holidayList()
    List
    
    # Holidays:
    Holiday = levels(List$HOLIDAYS)
    for ( i in 1:115) {
        print(Holiday[i])
        print(eval(parse(text = paste(Holiday[i], "(2006)", sep = ""))))
    }
    
    # Easter Date:
    X = Easter(2001:2010)
    X
    checkTrue(inherits(X, "timeDate"))
     
    # NYSE Holiday Calendar:  
    NYSE = holiday.NYSE(2000:2005)
    NYSE
    checkTrue(inherits(X, "timeDate"))
    
    # Julian:
    J = julian(NYSE)
    J
    class(J)
    unclass(J)
    
    # Month/Day/Year:
    MDY = month.day.year(as.numeric(J))
    cbind(MDY$year, MDY$month, MDY$day, as.character(NYSE))
    
    #Return value:
    return()
} 
 

# ------------------------------------------------------------------------------


if (FALSE) {
    testResult <- runTestFile("C:/Rmetrics/trunk/fCalendar/test/runit025C.R")
    printTextProtocol(testResult)
}


################################################################################
   
    