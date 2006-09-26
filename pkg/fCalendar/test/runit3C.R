
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


test.mathOps =
function()
{
    #  Ops.timeDate           Group 'Ops' generic functions for 'timeDate' objects
    #  +.timeDate             Performs arithmetic + operation on 'timeDate' objects
    #  -.timeDate             Performs arithmetic - operation on 'timeDate' objects
    #  diff.timeDate          Returns suitably lagged and iterated differences
    #  difftimeDate           Returns a difference of two 'timeDate' objects
    #  round.timeDate         Rounds objects of class 'timeDate'
    #  trunc.timeDate         Truncates objects of class 'timeDate' 

    # New York:
    myFinCenter = "NewYork"
    NY = timeCalendar(h = 10) 
    
    # Back to Zurich:
    myFinCenter = "Zurich"
    ZH = timeCalendar(h = 16)
    
    # Group Ops:
    TEST = (NY > ZH)
    checkTrue(!TEST[1])
    checkTrue(TEST[4])
    TEST = (NY - 24*3600 == ZH)
    
    # + Operation:
    
    # - Operation:
    NY - ZH 
    
    # diff.timeDate() Function:
    
    
    # difftimeDate() Function:
    
    
    # round() Function:
    set.seed(4711)
    myFinCenter = "GMT"
    TC = timeCalendar(
        m = 1:12,
        d = rep(1, 12),
        h = runif(12, 0, 23), 
        min = runif(12, 0, 59), 
        s = runif(12, 0, 59))
    print(TC)
   
    
        currentYear = 2006
        m = 1:12
        d = rep(1, 12)
        h = runif(12, 0, 23)
        min = runif(12, 0, 59)
        s = runif(12, 0, 59)
        
        
        
         
    
    # trunc() Function:
    set.seed(4711)
    TC = timeCalendar(
        h = runif(12, 0, 23), 
        min = runif(12, 0, 59), 
        s = runif(12, 0, 59))
    print(TC)
    

    # Return Value:
    return()  
}


# ------------------------------------------------------------------------------


test.Ordering =
function()
{
    #  c.timeDate             Concatenates 'timeDate' objects
    #  rep.timeDate           Replicates a 'timeDate' object
    #  sort.timeDate          Sorts a 'timeDate' object
    #  sample.timeDate        Resamples a 'timeDate' object
    #  unique.timeDate        NMakes a 'timeDate' object unique
    #  rev.timeDate           Reverts  a 'timeDate' object

    # NewYork:
    myFinCenter = "NewYork"
    NY = timeCalendar(h = 10) 
    current = c(
        "2006-01-01 10:00:00", "2006-02-01 10:00:00", "2006-03-01 10:00:00",
        "2006-04-01 10:00:00", "2006-05-01 10:00:00", "2006-06-01 10:00:00",
        "2006-07-01 10:00:00", "2006-08-01 10:00:00", "2006-09-01 10:00:00",
        "2006-10-01 10:00:00", "2006-11-01 10:00:00", "2006-12-01 10:00:00")
    checkIdentical(format(NY), current)
    
    # Back to Zurich:
    myFinCenter = "Zurich"
    ZH = timeCalendar(h = 16)
    current = c(
        "2006-01-01 16:00:00", "2006-02-01 16:00:00", "2006-03-01 16:00:00",
        "2006-04-01 16:00:00", "2006-05-01 16:00:00", "2006-06-01 16:00:00",
        "2006-07-01 16:00:00", "2006-08-01 16:00:00", "2006-09-01 16:00:00",
        "2006-10-01 16:00:00", "2006-11-01 16:00:00", "2006-12-01 16:00:00")
    checkIdentical(format(ZH), current)
    
    # NY-ZH Concatenate:
    NYC = c(NY, ZH)[13]
    print(NYC)
    current = "2006-01-01 10:00:00"
    checkIdentical(format(NYC), current)
    
    # ZH-NY Concatenate:
    ZRH = c(ZH, NY)[13]
    print(ZRH)
    current = "2006-01-01 16:00:00"
    checkIdentical(format(ZRH), current)
    
    # Replicate:
    DIFF = rep(NY[1:3], times = 3)-rep(NY[1:3], each = 3)
    target = as.numeric(DIFF[c(1, 5, 9)])
    checkIdentical(target, c(0, 0, 0))
    
    # Sort | Sample:
    TC = timeCalendar()
    print(TC)
    set.dseed = 4711
    SAMPLE = sample(TC)
    print(SAMPLE)
    checkIdentical(TC, sort(SAMPLE))
    
    # Revert:
    TS = timeSequence()
    print(TS)
    REV = rev(TS)
    print(head(REV))
    checkIdentical(TS, rev(REV))
}


# ------------------------------------------------------------------------------


if (FALSE) {
    require(RUnit)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fCalendar/test/runit3C.R")
    printTextProtocol(testResult)
}


################################################################################
   
    