
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
# FUNCTION:              SPECIAL TIMEDATE OPERATIONS:
#  timeLastDayInMonth     Computes the last day in a given month and year
#  timeFirstDayInMonth    Computes the first day in a given month and year
#  timeLastDayInQuarter   Computes the last day in a given quarter and year
#  timeFirstDayInQuarter  Computes the first day in a given quarter and year
#  timeNdayOnOrAfter      Computes date in month that is a n-day ON OR AFTER  
#  timeNdayOnOrBefore     Computes date in month that is a n-day ON OR BEFORE  
#  timeNthNdayInMonth     Computes n-th ocurrance of a n-day in year/month
#  timeLastNdayInMonth    Computes the last n-day in year/month
################################################################################


test.helpFile = 
function()
{
    # Help File:
    helpFile = function() { 
        example(TimeDateSpecDates); return() }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
} 


# ------------------------------------------------------------------------------


if (FALSE) {
    require(RUnit)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fCalendar/test/runit3D.R")
    printTextProtocol(testResult)
}


################################################################################
   
    