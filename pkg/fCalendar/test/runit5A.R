
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
# FUNCTION:             DESCRIPTION:
#  ...                   Holiday Functions
# FUNCTION:             DESCRIPTION:
#  .holidayList          Prints all public and ecclestical holidays
#  .easter               Returns date of easter or related feasts 
#  .easterSunday         Easter Algorithm 
# FUNCTION:             DESCRIPTION:
#  .on.or.after          Computes date in month that is a nday ON OR AFTER date
#  .on.or.before         Computes date in month that is a nday ON OR BEFORE date
#  .nth.of.nday          Computes nth ocurrance of a nday in year/month
#  .last.of.nday         Computes the last nday in year/month
# FUNCTION:             DESCRIPTION:
#  .sdate                Computes ISO-8601 dates from Julian day numbers
#  .sjulian              Computes Julian day numbers from ISO-8601 dates
#  .sday.of.week         Computes day of the week for ISO-8601 dates 
#  .sleap.year           Returns TRUE/FALSE if dates belong to leap years or not
################################################################################


test.aaa = 
function()
{
    # Help File:
    helpFile = function() { 
        example(CalendarData, ask = FALSE)
        return() 
    }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
} 
 

# ------------------------------------------------------------------------------


test.holidays =
function()
{
    # Holidays:
    holidays = as.vector(.holidayList()[,1])
    for (holiday in holidays) {
        Holiday = match.fun(holiday)
        cat(as.character(Holiday(currentYear)), holiday, "\n")
    }
    
    # Return Value:
    return()  
}


# ------------------------------------------------------------------------------


test.easter =
function()
{
    # Easter:
    .easter()
    .easterSunday(2007)
    
    # Return Value:
    return()  
}


# ------------------------------------------------------------------------------


test.holidayList =
function()
{
    # Holiday List:
    .holidayList()
    
    # Return Value:
    return()  
}


# ------------------------------------------------------------------------------



if (FALSE) {
    require(RUnit)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fCalendar/test/runit5A.R",
        rngKind = "Marsaglia-Multicarry", rngNormalKind = "Inversion")
    printTextProtocol(testResult)
}


################################################################################
   
