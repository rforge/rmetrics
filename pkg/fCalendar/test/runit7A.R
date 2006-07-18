
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
# MA 02111-1307 USA

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
# FUNCTION:             DESCRIPTION:
#  myFinCenter           My financial Center
#  currentYear           Date of the Current Year
#  myUnits               Date Units
#  xmpCalendar           Sets prompt
#  xmpfCalendar          Popups the example menu
# FUNCTION:             DESCRIPTION:
#  modify                Modify a 'timeSeries' object
#  modify.default        S3 Default Method
#  atoms                 Extract atoms from 'timeSeries' object
#  atoms.default         S3 Default Method
################################################################################


test.helpFile = 
function()
{
    # Help File:
    helpFile = function() { 
        example(CalendarTools); return() }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
}
 

# ------------------------------------------------------------------------------


if (FALSE) {
    testResult <- runTestFile("C:/Rmetrics/trunk/fCalendar/test/runit027A.R")
    printTextProtocol(testResult)
}


################################################################################
   
    
