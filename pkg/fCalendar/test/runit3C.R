
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
# S3 MEHOD:              MATHEMATICAL OPERATIONS:
#  Ops.timeDate           Group 'Ops' generic functions for 'timeDate' objects
#  +.timeDate             Performs arithmetic + operation on 'timeDate' objects
#  -.timeDate             Performs arithmetic - operation on 'timeDate' objects
#  diff.timeDate          Returns suitably lagged and iterated differences
#  difftimeDate           Returns a difference of two 'timeDate' objects
#  round.timeDate         Rounds objects of class 'timeDate'
#  trunc.timeDate         Truncates objects of class 'timeDate' 
# S3 MEHOD:              CONCATENATION, ORDERING AND SORTING:
#  c.timeDate             Concatenates 'timeDate' objects
#  rep.timeDate           Replicates a 'timeDate' object
#  sort.timeDate          Sorts a 'timeDate' object
#  sample.timeDate        Resamples a 'timeDate' object
#  unique.timeDate        NMakes a 'timeDate' object unique
#  rev.timeDate           Reverts  a 'timeDate' object
################################################################################


test.helpFile = 
function()
{
    # Help File:
    helpFile = function() { 
        example(TimeDateMathOps); return() }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
} 
 

# ------------------------------------------------------------------------------


test.Ops =
function()
{
    myFinCenter <<- "NewYork"
    NY = timeCalendar(h=10) 
    
    myFinCenter <<- "Zurich"
    ZH = timeCalendar(h=16)
    
    NY - ZH 
    
    TEST = (NY > ZH)
    checkTrue(!TEST[1])
    checkTrue(TEST[4])
    
    TEST = (NY - 24*3600 == ZH)
    
    # Return Value:
    return()  
}


# ------------------------------------------------------------------------------


test.methods =
function()
{
    myFinCenter <<- "NewYork"
    NY = timeCalendar(h=10) 
    
    myFinCenter <<- "Zurich"
    ZH = timeCalendar(h=16)
    
    c(NY, ZH)
    c(ZH, NY)
    
    rep(NY[1:3], times = 3)
    rep(NY[1:3], each = 3)
}


# ------------------------------------------------------------------------------


if (FALSE) {
    require(RUnit)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fCalendar/test/runit3C.R")
    printTextProtocol(testResult)
}


################################################################################
   
    