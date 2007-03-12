
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
# FUNCTION:           HOLIDAY CALENDAR FUNCTIONS:
#  holiday             Returns a holiday date of G7 and CH 
#  holidayNYSE         Returns 'timeDate' object for full-day NYSE holidays
#  holidayZURICH       Returns 'timeDate' object for ZURICH holidays
################################################################################


test.aaa = 
function()
{
    # Help File:
    helpFile = function() { 
        example(HolidayCalendars, ask = FALSE)
        return() 
    }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
} 
 

# ------------------------------------------------------------------------------


test.holiday = 
function()
{
    # easter -
    # easter(year = currentYear, shift = 0)
    # Dates for Easter and Good Friday from 2000 until 2010:
    .easter()
    Easter = .easter(2000:2010)
    Easter
    checkTrue(inherits(Easter, "timeDate"))
    
    GoodFriday = .easter(2000:2010, -2) 
    GoodFriday
    checkIdentical(
        target = Easter, 
        current = GoodFriday + 2*24*3600)
    
    HD = holiday(2000:2010, "Easter")     
    HD
    checkTrue(inherits(HD, "timeDate")) 
         
    HD = holiday(2000:2010, "GoodFriday")  
    HD
    checkTrue(inherits(HD, "timeDate"))
   
    # holidays -   
    Easter(2000:2010)                    
    GoodFriday(2000:2010)
  
    # Return Value:
    return()    
} 
 

# ------------------------------------------------------------------------------


test.holidayNYSE = 
function()
{       
    # Holiday NYSE -
    HD = holidayNYSE(currentYear)
    print(HD)
    checkTrue(inherits(HD, "timeDate"))
   
    # Return Value:
    return()   
}


# ------------------------------------------------------------------------------


test.holidayZURICH = 
function()
{       
    # Holiday Zurich -                              # CHECK
    # HD = holidayZURICH(currentYear)
    # print(HD)
    # checkTrue(inherits(HD, "timeDate"))
   
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------
   

if (FALSE) {
    require(RUnit)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fCalendar/test/runit5B.R",
        rngKind = "Marsaglia-Multicarry", rngNormalKind = "Inversion")
    printTextProtocol(testResult)
}


################################################################################
   
    