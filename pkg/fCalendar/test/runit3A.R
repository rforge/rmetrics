
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
# FUNCTION:              FINANCIAL CENTERS:
#  myFinCenter            Sets my financial center
#  rulesFinCenter         Returns DST rules for a financial center
#  listFinCenter          Lists all supported financial centers
# FUNCTION:              GENERATION OF TIMEDATE OBJECTS:
#  'timeDate'             S4 Class representation for timeDate objects
#  timeDate               Creates a 'timeDate' object from given dates
#  .whichFormat            Returns format string called by timeDate
#  .midnightStandard       Corrects for midnight standard called by timeDate
#  .formatFinCenter        Internal called by timeDate
#  timeCalendar           Creates a 'timeDate' object from calendar atoms
#  timeSequence           Creates a regularly spaced 'timeDate' object
#   seq.timeDate           A synonyme function for timeSequence
#  Sys.timeDate           Returns system time as an object of class 'timeDate' 
#  is.timeDate            Tests if the object is of class 'timeDate' 
# METHODS:               REPRESENTATION OF TIMEDATE OBJECTS:
#  print.timeDate         Prints 'timeDate' object
#  plot.timeDate          Plots 'timeDate' object
#  points.timeDate        Adds points to a 'timeDate' plot
#  lines.timeDate         Adds lines to a 'timeDate' plot
#  summary.timeDate       Summarizes details of a 'timeDate' object
#  format.timeDate        Formats 'timeDate' as ISO conform character string
################################################################################


test.helpFile = 
function()
{
    # Help File:
    helpFile = function() { 
        example(TimeDateClass); return() }
    checkIdentical(
        target = class(try(helpFile())),
        current = "NULL")

    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------
 

test.finCenter =
function()
{
}


# ------------------------------------------------------------------------------


test.midnightStandard = 
function()
{   
    # Midnight Standard:
    MS = .midnightStandard("20010101",       
        .whichFormat("20010101"))
    print(MS)
    checkIdentical(MS, "20010101")
    MS = .midnightStandard("200101010000",   
        .whichFormat("200101010000"))
    print(MS)
    checkIdentical(MS, "200101010000")
    MS = .midnightStandard("20010101000000", 
        .whichFormat("20010101000000"))
    print(MS)
    checkIdentical(MS, "2001-01-01 00:00:00")
    MS = .midnightStandard("200101011600",   
        .whichFormat("200101011600"))
    print(MS)
    checkIdentical(MS, "200101011600")
    MS = .midnightStandard("20010101160000", 
        .whichFormat("20010101160000"))
    print(MS)
    checkIdentical(MS, "2001-01-01 16:00:00")
    
    # Midnight Standard:
    MS = .midnightStandard("2001-01-01",          
        .whichFormat("2001-01-01"))
    print(MS)
    checkIdentical(MS, "2001-01-01")
    MS = .midnightStandard("2001-01-01 00:00",    
        .whichFormat("2001-01-01 00:00"))
    print(MS)
    checkIdentical(MS, "2001-01-01 00:00")
    MS = .midnightStandard("2001-01-01 00:00:00", 
        .whichFormat("2001-01-01 00:00:00"))
    print(MS)
    checkIdentical(MS, "2001-01-01 00:00:00")
    MS = .midnightStandard("2001-01-01 16:00",    
        .whichFormat("2001-01-01 16:00"))
    print(MS)
    checkIdentical(MS, "2001-01-01 16:00")
    MS = .midnightStandard("2001-01-01 16:00:00", 
        .whichFormat("2001-01-01 16:00:00"))
    print(MS)
    checkIdentical(MS, "2001-01-01 16:00:00")
    
    # Midnight Standard:
    .midnightStandard("2006-01-01 24:00:00", format = "%Y-%m-%d %H:%M:%S")
    print(MS)
    checkIdentical(MS, "2001-01-01 16:00:00")
    .midnightStandard("2006-01-01", format = "%Y-%m-%d")
    print(MS)
    checkIdentical(MS, "2001-01-01 16:00:00")
    .midnightStandard("20060101240000", format = "%Y%m%d%H%M%S")
    print(MS)
    checkIdentical(MS, "2001-01-01 16:00:00")
    .midnightStandard("20060101", format = "%Y%m%d")
    print(MS)
    checkIdentical(MS, "2001-01-01 16:00:00")
    .midnightStandard("1/1/2006", format = "%m/%d/%Y")
    print(MS)
    checkIdentical(MS, "2001-01-01 16:00:00")
    
    # Return Value:
    return()    
}


# ------------------------------------------------------------------------------
 
  
test.timeDate = 
function()
{
    # FORMAT:
    
    WF = .whichFormat("20010101")
    print(WF)
    checkIdentical(WF, "%Y%m%d")
    WF = .whichFormat("200101010000")
    print(WF)
    checkIdentical(WF, "%Y%m%d%H%M")
    WF = .whichFormat("20010101000000")
    print(WF)
    checkIdentical(WF, "%Y%m%d%H%M%S")
    WF = .whichFormat("200101011600")
    print(WF)
    checkIdentical(WF, "%Y%m%d%H%M")
    WF = .whichFormat("20010101160000")
    print(WF)
    checkIdentical(WF, "%Y%m%d%H%M%S")
    
    WF = .whichFormat("2001-01-01")
    print(WF)
    checkIdentical(WF, "%Y-%m-%d")
    WF = .whichFormat("2001-01-01 00:00")
    print(WF)
    checkIdentical(WF, "%Y-%m-%d %H:%M")
    WF = .whichFormat("2001-01-01 00:00:00")
    print(WF)
    checkIdentical(WF, "%Y-%m-%d %H:%M:%S")
    WF = .whichFormat("2001-01-01 16:00")
    print(WF)
    checkIdentical(WF, "%Y-%m-%d %H:%M")
    WF = .whichFormat("2001-01-01 16:00:00")
    print(WF)
    checkIdentical(WF, "%Y-%m-%d %H:%M:%S")
    
    WF = .whichFormat("01/01/2001")
    print(WF)
    checkIdentical(WF, "%m/%d/%Y")
    WF = .whichFormat("01-Jan-2001")
    print(WF)
    checkIdentical(WF, "%d-%b-%Y")
    
    # CHARACTER:
    
    myFinCenter <<- "GMT"
    print(myFinCenter)
    
    charvec = paste("2006-01-", c(10, 20, 30), sep = "")
    print(charvec)
    TD = timeDate(charvec)
    print(TD)
    CHARVEC = as.character(TD)
    attr(CHARVEC, "control")<-NULL
    checkIdentical(charvec, CHARVEC)
    
    charvec = paste(charvec, "00")
    print(charvec)
    TD = timeDate(charvec)
    print(TD)
    
    charvec = paste(charvec, ":00", sep = "")
    print(charvec)
    TD = timeDate(charvec)
    print(TD)
    
    charvec = paste(charvec, ":00", sep = "")
    print(charvec)
    TD = timeDate(charvec)
    print(TD)

    # YYYYMMDDhhmmss
    timeDate("20010101")
    timeDate("200101010000")
    timeDate("20010101000000")     
    timeDate("200101011600")
    timeDate("20010101160000")
    
    TD = timeDate("2001-01-01")
    TD
    TD@format
    TD = timeDate("2001-01-01 00:00")
    TD
    TD@format
    TD = timeDate("2001-01-01 00:00:00")    
    TD
    TD@format 
    TD = timeDate("2001-01-01 16:00")
    TD
    TD@format
    TD = timeDate("2001-01-01 16:00:00") 
    TD
    TD@format
         
    timeDate(c("2001-01-01 00:00",    "2001-01-01 16:00"   ))
    timeDate(c("2001-01-01 16:00:00", "2001-01-01 16:00:00"))
    
    
    timeDate("20010101", 
        zone = "GMT", FinCenter = "Zurich")
    timeDate("200101010000", 
        zone = "GMT", FinCenter = "Zurich")
    timeDate("20010101000000", 
        zone = "GMT", FinCenter = "Zurich")     
    timeDate("200101011600", 
        zone = "GMT", FinCenter = "Zurich")
    timeDate("20010101160000", 
        zone = "GMT", FinCenter = "Zurich")     
    timeDate(c("2001-01-01 00:00", "2001-01-01 16:00"), 
        zone = "GMT", FinCenter = "Zurich")
    timeDate(c("2001-01-01 16:00:00", "2001-01-01 16:00:00"), 
        zone = "GMT", FinCenter = "Zurich")

       
    # Format: "%d-%b-%Y"
    Months = c("Mar", "Jun", "Sep", "Dec")
    charvec = paste("01-", Months, "-2006", sep = "")
    charvec
    X = timeDate(charvec, format = "%Y-%b-%d")
    X
    X@format
    X = timeDate(charvec)
    
    # Format: "%m/%d/%Y"
    X = timeDate("12/15/2006", format = "%m/%d/%Y")
    X
    X@format
    timeDate("12/15/2006")
    
    
    # POSIX:
    X = ISOdate(year=2006, month=1:12, day=1, hour = 0)
    X
    class(X)
    timeDate(X)
    timeDate(X, zone = "GMT", FinCenter = "GMT")
    timeDate(X, zone = "NewYork", FinCenter = "NewYork")
    timeDate(X, zone = "GMT", FinCenter = "NewYork")
    as.timeDate(X)
    as.timeDate(X, zone = "NewYork", FinCenter = "NewYork") 
    as.timeDate(X, zone = "GMT", FinCenter = "NewYork")
    
    
    # DATE:
    X = as.Date(ISOdate(year=2006, month=1:12, day=1, hour = 0))
    X
    class(X)
    timeDate(X)
    timeDate(X, zone = "GMT", FinCenter = "GMT")
    timeDate(X, zone = "NewYork", FinCenter = "NewYork")
    timeDate(X, zone = "GMT", FinCenter = "NewYork")
    as.timeDate(X)
    as.timeDate(X, zone = "NewYork", FinCenter = "NewYork") 
    as.timeDate(X, zone = "GMT", FinCenter = "NewYork")

    
    # Return Value:
    return()    
}

 
# ------------------------------------------------------------------------------


test.timeCalendar = 
function()
{
    # timeCalendar -
    # Fix Financial Center:
    myFinCenter <<- "Zurich"
    target = myFinCenter
    target
    current = "Zurich"
    current
    checkIdentical(target, current)
    
    # timeCalendar -
    # Check CurrentYear:
    target = as.character(currentYear)
    target
    current = substr(as.character(Sys.Date()), 1, 4)
    current
    checkIdentical(target, current)
    
    # timeCalendar -
    # Generate timDate from timeCalendar, compare with timeSequence:
    myFinCenter <<- "GMT"
    target = timeCalendar(y = 2006, m = 1:12, d = 1)
    target
    current = timeSequence(from = "2006-01-01", by = "month", length.out = 12)  
    current
    checkIdentical(target, current)
    
    # timeCalendar -
    # Generate timDate from timeCalendar, compare with timeSequence:
    myFinCenter <<- "Zurich"
    target = timeCalendar(y = 2006, m = 1:12, d = 1)
    target
    current = timeSequence(from = "2006-01-01", by = "month", length.out = 12)  
    current
    checkIdentical(target, current)
              
    # timeCalendar -
    # Date/Time:
    timeCalendar(h = 16)                 
    timeCalendar(h = 16, zone = "GMT")
    timeCalendar(h = 16, zone = "GMT", FinCenter = "NewYork")
    
    # Return Value:
    return()
}
   
    
# ------------------------------------------------------------------------------

    
test.timeSequence = 
function()
{
    # timeCalendar -
    # Generate timDate Objects from Sequences:
    myFinCenter
    target = timeSequence(from = "2006-01-01", to = "2006-01-31", by = "day")
    target
    current = timeCalendar(y = 2006, m = 1, d = 1:31)
    current
    checkIdentical(target, current)
     
    from = "2006-01-01 00:00:00"
    to   = "2006-01-01 23:59:59"
    timeSequence(from, to, by = "hour")
    
    from = "2006-01-01 16:00:00"
    to   = "2006-01-01 16:14:59"
    timeSequence(from, to, by = "min")
    
    from = "2006-01-01 16:00:00"
    to   = "2006-01-01 16:00:14"
    timeSequence(from, to, by = "s")
    
    from = "2006-01-01 16:00:00"
    timeSequence(from, length.out = 15, by = "year")
    timeSequence(from, length.out = 15, by = "quarter")
    timeSequence(from, length.out = 15, by = "week")
    timeSequence(from, length.out = 15, by = "month")
    timeSequence(from, length.out = 15, by = "day")
    timeSequence(from, length.out = 15, by = "hour")
    timeSequence(from, length.out = 15, by = "min")
    timeSequence(from, length.out = 15, by = "sec")
    
    from = "2006-01-01"
    timeSequence(from, length.out = 15, by = "year")
    timeSequence(from, length.out = 15, by = "quarter")
    timeSequence(from, length.out = 15, by = "week")
    timeSequence(from, length.out = 15, by = "month")
    timeSequence(from, length.out = 15, by = "day")
    timeSequence(from, length.out = 15, by = "hour")
    timeSequence(from, length.out = 15, by = "min")
    timeSequence(from, length.out = 15, by = "sec")
    
    # Return Value:
    return()
}   
  

# ------------------------------------------------------------------------------


test.sysTimeDate = 
function()
{
    Sys.timeDate()
    Sys.timeDate("GMT")
    Sys.timeDate("NewYork")
    class(Sys.timeDate())
    
    charvec = Sys.Date()
    charvec
    class(charvec)
    timeDate(charvec)
    
    # Return Value:
    return()
}


# ------------------------------------------------------------------------------
      
 
test.displayMethods = 
function()
{   
    # print.timeDate     Prints 'timeDate' Object
    # summary.timeDate   Summarizes details of a 'timeDate' object  
    # format.timeDate    Formats 'timeDate' as ISO conform character string
 
    myFinCenter <<- "NewYork"
    
    DT = timeCalendar()
    print(DT)
    summary(DT)
    format(DT)
    
    DT = timeSequence("2006-01-01", length.out = 10)
    print(DT)
    y = rnorm(10)
    plot(DT, y)
    points(DT, y, col = "red")
    lines(DT, y, col = "blue")
    summary(DT)
    format(DT)
    
    DT = Sys.timeDate()
    print(DT)
    summary(DT)
    format(DT)
    
    # Return Value:
    return()
} 
 

# ------------------------------------------------------------------------------


if (FALSE) {
    require(RUnit)
    testResult <- runTestFile("C:/Rmetrics/SVN/trunk/fCalendar/test/runit3A.R")
    printTextProtocol(testResult)
}


################################################################################

