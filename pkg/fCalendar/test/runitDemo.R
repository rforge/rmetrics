
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
# You should have received A copy of the GNU Library General 
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
# DEMO:                         DESCRIPTION
#  xmpDWChapter021.R    
#  xmpDWChapter022.R
#  xmpDWChapter023.R
#  xmpDWChapter024.R
#  xmpDWChapter025.R
################################################################################


test.demoFile21 = 
function()
{
    # Help File:
    demoFile = paste(.Library, 
        "\\fCalendar\\Demo\\xmpDWChapter021.R", sep = "")
    source(demoFile)

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.demoFile22 = 
function()
{
    # Help File:
    demoFile = paste(.Library, 
        "\\fCalendar\\Demo\\xmpDWChapter022.R", sep = "")
    source(demoFile)

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.demoFile23 = 
function()
{
    # Help File:
    demoFile = paste(.Library, 
        "\\fCalendar\\Demo\\xmpDWChapter023.R", sep = "")
    source(demoFile)

    # Return Value:
    return()    
}

# ------------------------------------------------------------------------------


test.demoFile24 = 
function()
{
    # Help File:
    demoFile = paste(.Library, 
        "\\fCalendar\\Demo\\xmpDWChapter024.R", sep = "")
    source(demoFile)

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.demoFile25 = 
function()
{
    # Help File:
    demoFile = paste(.Library, 
        "\\fCalendar\\Demo\\xmpDWChapter025.R", sep = "")
    source(demoFile)

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


test.demoZWChapter02 = 
function()
{
    # Help File:
    demoFile = paste(.Library, 
        "\\fCalendar\\Demo\\xmpZWChapter02.R", sep = "")
    source(demoFile)

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------


if (FALSE) {
    require(RUnit)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fCalendar/test/runitDemo.R")
    printTextProtocol(testResult)
}


# ------------------------------------------------------------------------------

