
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# A copy of the GNU General Public License is available via WWW at
# http://www.gnu.org/copyleft/gpl.html.  You can also obtain it by
# writing to the Free Software Foundation, Inc., 59 Temple Place,
# Suite 330, Boston, MA  02111-1307  USA. 

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
# FUNCTION:
################################################################################


# ******************************************************************************
# Time Date Class


.fCalendar.TimeDateClass.1 = 
function()
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # * Example timeDate: x = 2005-[1:12]-01
    .fun <<- function(object2x) {
        object <<- timeCalendar()
        if (as.logical(object2x))
            x <<- .saveAs(
                data = object, 
                infoName = "timeCalendar()",
                console = NULL ) 
        object }
    .objectMenu(
        params = list( 
            object2x = TRUE),
        infoName = "timeCalendar()",
        tkoutput = TRUE,
        console = NULL,
        title = "timeCalendar()" )      
}


.fCalendar.TimeDateClass.2 = 
function()
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # * Example timeDate: x = 12 Random Dates in 2005
    .fun <<- function(object2x) {
        y = rep(2005, 12)
        d = trunc(runif(12, 1, 29))
        m = trunc(runif(12, 1, 13))
        object <<- timeCalendar(y = y, m = m, d = d, FinCenter = "GMT")
        if (as.logical(object2x))
            x <<- .saveAs(
                data = object, 
                infoName = "random timeCalendar()",
                console = NULL ) 
        object }
    .objectMenu(
        params = list( 
            object2x = TRUE),
        infoName = "random timeCalendar()",
        tkoutput = TRUE,
        console = NULL,
        title = "12 Random Dates in 2005" )      
}


.fCalendar.TimeDateClass.3 = 
function()
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # * Example timeDate: x = 12 Random Times in 2005 
    .fun <<- function(object2x) {
        y = rep(2005, 12)
        d = trunc(runif(12, 1, 29))
        m = trunc(runif(12, 1, 13))
        h = trunc(runif(12, 0, 24))
        min = trunc(runif(12, 0, 60))
        s = trunc(runif(12, 0, 60))
        object <<- timeCalendar(y = y, m = m, d = d, h = h, 
            min = min, s = s, FinCenter = "GMT")
        if (as.logical(object2x))
            x <<- .saveAs(
                data = object, 
                infoName = "random timeCalendar()",
                console = NULL ) 
        object }
    .objectMenu(
        params = list( 
            object2x = TRUE),
        infoName = "random timeCalendar()",
        tkoutput = TRUE,
        console = NULL,
        title = "12 Random Times in 2005" )      
}


.fCalendar.TimeDateClass.4 = 
function()
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Print timeDate Class Representation:
    .getClass("timeDate")
}


.fCalendar.TimeDateClass.5 = 
function()
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Current Date and Time:
    .getTime()
}


.fCalendar.TimeDateClass.6 = 
function()
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # List of Financial Centers
    .getFinCenters()
}


.fCalendar.TimeDateClass.7 = 
function()
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # DST Rules for a Financial Center:
    .fun <<- function(FinCenter){
        .FUN <<- match.fun(FinCenter)
        .FUN() }
    .objectMenu(
        params = list(
            FinCenter = "Zurich"),
        infoName = "DST Rules",
        tkoutput = TRUE,
        console = NULL,
        title = "Daylight Saving Time Rules" )
}


.fCalendar.TimeDateClass.8 = 
function()
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Create timeDate Sequence:
    .fun <<- function(from, to, by, length.out, format, FinCenter, 
        object2x) {
        object <<- timeSequence(from = from, to = to, by = by, 
            length.out = eval(parse(text = length.out)), format = format, 
            FinCenter = FinCenter) 
        if (object2x) 
            x <<- .saveAs(
                data = object,
                infoName = "Copied from object to x") 
        object }
    .objectMenu(
        params = list(
            from = "2005-01-01", 
            to = "2005-12-31", 
            by = "day", 
            length.out = "NULL", 
            format = "%Y-%m-%d", 
            FinCenter = "GMT",
            object2x = FALSE),
        infoName = "timeDate Sequence",
        tkoutput = FALSE,
        console = "print(object[1:5])" )
}


.fCalendar.TimeDateClass.9 = 
function()
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Create timeDate Calendar:
    .fun <<- function(y, m, d, h, min, s, FinCenter, object2x) {
        object <<- timeCalendar(
            y = eval(parse(text = y)),
            m = eval(parse(text = m)), 
            d = eval(parse(text = d)),
            h = eval(parse(text = h)), 
            min = eval(parse(text = min)),
            s = eval(parse(text = s)), 
            FinCenter = FinCenter) 
        if (object2x) 
            x <<- .saveAs(
                data = object,
                infoName = "Copied from object to x") 
        object }
    .objectMenu(
        params = list(
            y = "2005", 
            m = "1:12", 
            d = "1", 
            h = "NULL", 
            min = "NULL", 
            s = "NULL",
            FinCenter = "GMT",
            object2x = FALSE),
        infoName = "timeDate Calendar",
        tkoutput = FALSE,
        console = "print(object[1:5])" )
}


.fCalendar.TimeDateClass.10 = 
function()
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Last Day in Month for a Given Date:
    .fun <<- function(series, format, FinCenter, object2x) {
        x = eval(parse(text = series))
        object <<- timeLastDayInMonth(charvec = as.character(x), 
            format = format, FinCenter = FinCenter) 
        if (object2x) 
            x <<- .saveAs(
                data = object,
                infoName = "Copied from object to x") 
        object }
    .objectMenu(
        params = list(
            series = "x",
            format = "%Y-%m-%d",
            FinCenter = "GMT",
            object2x = FALSE),
        infoName = "Last Day in Month",
        tkoutput = FALSE,
        console = "print(object[1:5])" ) 
}


.fCalendar.TimeDateClass.11 = 
function()
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # The N-Day On-Or-After a given Date:
    .fun <<- function(series, nday, format, FinCenter, object2x) {
        x = eval(parse(text = series))
        object <<- timeNdayOnOrAfter(charvec = as.character(x), 
            nday = nday, format = format, FinCenter = FinCenter) 
        if (object2x) 
            x <<- .saveAs(
                data = object,
                infoName = "Copied from object to x") 
        object } 
    .objectMenu(
        params = list(
            series = "x",
            nday = 1,
            format = "%Y-%m-%d",
            FinCenter = "GMT",
            object2x = FALSE),
        infoName = "The N-Day On-Or-After",
        tkoutput = FALSE,
        console = "print(object[1:5])" )      
}


.fCalendar.TimeDateClass.12 = 
function()
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # The N-Day On-Or-Before a given Date:
    .fun <<- function(series, nday, format, FinCenter, object2x) {
        x = eval(parse(text = series))
        object <<- timeNdayOnOrBefore(charvec = as.character(x), nday = nday, 
            format = format, FinCenter = FinCenter) 
        if (object2x) 
            x <<- .saveAs(
                data = object,
                infoName = "Copied from object to x") 
        object }
    .objectMenu(
        params = list(
            series = "x",
            nday = 1,
            format = "%Y-%m-%d",
            FinCenter = "GMT",
            object2x = FALSE),
        infoName = "The N-Day On-Or-Before",
        tkoutput = FALSE,
        console = "print(object[1:5])" )     
}


.fCalendar.TimeDateClass.13 = 
function()
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # The n-th ocurrance of a n-day in year/month
    .fun <<- function(series, nday, nth, format, FinCenter, object2x) {
        x = eval(parse(text = series))
        object <<- timeNthNdayInMonth(charvec = as.character(x), 
            nday = nday, nt = nth, format = format, FinCenter = FinCenter) 
        if (object2x) 
            x <<- .saveAs(
                data = object,
                infoName = "Copied from object to x") 
        object }
    .objectMenu(
        params = list(
            series = "x",
            nday = 1,
            nth = 1, 
            format = "%Y-%m-%d",
            FinCenter = "GMT",
            object2x = FALSE),
        infoName = "The n-th Ocurrance of a n-Day",
        tkoutput = FALSE,
        console = "print(object[1:5])" )     
}


.fCalendar.TimeDateClass.14 = 
function()
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # The last n-day in year/month 
    .fun <<- function(series, nday, format, FinCenter, object2x) {
        x = eval(parse(text = series))
        object <<- timeLastNdayInMonth(charvec = as.character(x), 
            nday = nday, format = format, FinCenter = FinCenter) 
        if (object2x) 
            x <<- .saveAs(
                data = object,
                infoName = "Copied from object to x") 
        object }
    .objectMenu(
        params = list(
            series = "x",
            nday = 1,
            format = "%Y-%m-%d",
            FinCenter = "GMT",
            object2x = FALSE),
        infoName = "The Last N-Day",
        tkoutput = FALSE,
        console = "print(object[1:5])" )     
}


.fCalendar.TimeDateClass.15 = 
function()
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Print Summary of a 'timeDate' object
    ans = capture.output(summary(x))[2:7]
    .tkTitle("Summary of a timeDate Object")
    .tkOutput(ans)
}


.fCalendar.TimeDateClass.16 = 
function()
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # ... Save object to x:
    object <<- .infoObject(
        data = x, 
        infoName = "Object copied to x",
        console = "print(head(data))" )
}                                                



# ******************************************************************************
# Time Date Methods


.fCalendar.TimeDateMethods.1 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # * Example timeDate: x = 2005-[1:12]-01 
    x <<- .saveAs(
        data = timeCalendar(), 
        infoName = "timeDate Example",
        console = "print(data)")
}


.fCalendar.TimeDateMethods.2 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # * Example timeDate: x = 12 Random Dates in 2005 
    y = rep(2005, 12)
    d = trunc(runif(12, 1, 29))
    m = trunc(runif(12, 1, 13))
    x <<- .saveAs(
        data = timeCalendar(y = y, m = m , d = d, FinCenter = "GMT"), 
        infoName = "Random Date Vector Example",
        console = "print(data)")
}


.fCalendar.TimeDateMethods.3 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # * Example timeDate: x = 12 Random Times in 2005 
    y = rep(2005, 12)
    d = trunc(runif(12, 1, 29))
    m = trunc(runif(12, 1, 13))
    h = trunc(runif(12, 0, 24))
    min = trunc(runif(12, 0, 60))
    s = trunc(runif(12, 0, 60))
    x <<- .saveAs(
        data = timeCalendar(y = y, m = m, d = d, h = h, 
        min = min, s = s, FinCenter = "GMT"), 
        infoName = "Random Date/Time Vector Example",
        console = "print(data)")
}


.fCalendar.TimeDateMethods.4 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Print timeDate Class Representation:
    .getClass("timeDate") 
}


.fCalendar.TimeDateMethods.5 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Extract the first object of a timeDate Vector
    .fun <<- function(series, object2x) {
        x = eval(parse(text = series))
        object <<- start(modify(x = x, "sort"))
        if (object2x) 
            x <<- .saveAs(
                data = object,
                infoName = "Last timeDate Value")
        .tkReport("First Value of timeDate Object")
        object }
    .objectMenu(
        params = list(
            series = "x",
            object2x = FALSE),
        infoName = "First timeDate Value")
}


.fCalendar.TimeDateMethods.6 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Extract the last object of a timeDate Vector:
    .fun <<- function(series, object2x) {
        x = eval(parse(text = series))
        object <<- end(modify(x = x, "sort"))
        if (object2x) 
            x <<- .saveAs(
                data = object,
                infoName = "Last timeDate Value")
        .tkReport("Last timeDate Valuet")
        object  }
    .objectMenu(
        params = list(
            series = "x",
            object2x = FALSE),
        infoName = "Last timeDate Value",
        tkoutput = FALSE )
}


.fCalendar.TimeDateMethods.7 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # +/- a time Span from a timeDate Vector
    .fun <<- function(series, PlusMinus, days, hours, minutes, 
        seconds, object2x) {
        x = eval(parse(text = series))
        ans = days*24*60*60 + hours*60*60 + minutes*60 + seconds
        if (PlusMinus == "+") object <<- x + ans
        if (PlusMinus == "-") object <<- x - ans
        if (object2x) 
            x <<- .saveAs(
                data = object,
                infoName = "+/- Time Span")
        stop("PlusMinus must be the + or - sign.") 
        object}
    .objectMenu(
        params = list(
            series = "x",
            PlusMinus = "+",
            days = 1,
            hours = 0,
            minutes = 0,
            seconds = 0,
            object2x = FALSE),
        infoName = "+/- Time Span",
        tkoutput = FALSE,
        console = "print(object[1:5])" ) 
}


.fCalendar.TimeDateMethods.8 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Sort a timeDate Vector by Time and Date:
    .fun <<- function(series, object2x) {
        x = eval(parse(text = series))
        object <<- modify(x = x, method = "sort") 
        if (object2x) 
            x <<- .saveAs(
                data = object,
                infoName = "Last timeDate Value")
        object}  
    .objectMenu(
        params = list(
            series = "x",
            object2x = FALSE),
        infoName = "+/- Time Span",
        tkoutput = FALSE,
        console = "print(object[1:5])" ) 
}


.fCalendar.TimeDateMethods.9 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Round a timeDate Vector to a Given Unit:
    .fun <<- function(series, units, object2x) {
        x = eval(parse(text = series))  
        object <<- modify(x = x, method = "round", units = units) 
        if (object2x) 
            x <<- .saveAs(
                data = object,
                infoName = "Rounded timeDate")
        object }
    .objectMenu(
        params = list(
            series = "x",
            units = "days",
            object2x = FALSE),
        infoName = "Rounded timeDate",
        tkoutput = FALSE,
        console = "print(object[1:5])" )
}


.fCalendar.TimeDateMethods.10 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Truncate a timeDate Vector to a Given Unit:
    .fun <<- function(series, units, object2x) { 
        x = eval(parse(text = series))
        object <<- modify(x, method = "trunc", units = units)
        if (object2x) 
            x <<- .saveAs(
                data = object,
                infoName = "Truncated timeDate")
        object }
    .objectMenu(
        params = list(
            series = "x",
            units = "days",
            object2x = FALSE),
        infoName = "Truncated timeDate",
        console = "print(object[1:5])" )
}


.fCalendar.TimeDateMethods.11 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Return a timeDate Vector in Reverse Order:
    .fun <<- function(series, object2x) { 
        x = eval(parse(text = series))
        object <<- rev(x = x)
        if (object2x) 
            x <<- .saveAs(
                data = object,
                infoName = "Last timeDate Value")
        object }
    .objectMenu(
        params = list(
            series = "x",
            object2x = FALSE),
        infoName = "Reverted timeDate",
        console = "print(object[1:5])" )
}


.fCalendar.TimeDateMethods.12 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Convert timeDate to a character vector:
    .fun <<- function(series, object2x) { 
        x = eval(parse(text = series))
        object <<- as.character(x = x) 
        if (object2x) 
            x <<- .saveAs(
                data = object,
                infoName = "Last timeDate Value")
        object }
    .objectMenu(
        params = list(
            series = "x",
            object2x = FALSE),
        infoName = "timeDate as.character",
        console = "print(head(object))" )
}


.fCalendar.TimeDateMethods.13 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Convert timeDate to a data frame:
    .fun <<- function(series, object2x) { 
        x = eval(parse(text = series))
        object <<- as.data.frame(x = x)
        if (object2x) 
            x <<- .saveAs(
                data = object,
                infoName = "timeDate as.data.frame")
        object }
    .objectMenu(
        params = list(
            series = "x",
            object2x = FALSE),
        infoName = "timeDate as.data.frame",
        console = "print(head(object))" )
}


.fCalendar.TimeDateMethods.14 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Convert timeDate to a POSIXct Object:
    .fun <<- function(series, object2) { 
        x = eval(parse(text = series))
        object <<- as.POSIXct.timeDate(x = x)
        if (object2x) 
            x <<- .saveAs(
                data = object,
                infoName = "timeDate as.POSIXct")
        object }
    .objectMenu(
        params = list(
            series = "x",
            object2x = FALSE),
        infoName = "timeDate as.POSIXct",
        console = "print(head(object))" )
}


.fCalendar.TimeDateMethods.15 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Julian Day Counts from timeDate Vector:
    .fun <<- function(series, object2x) { 
        x = eval(parse(text = series))
        object <<- julian(x = x)
        if (object2x) 
            x <<- .saveAs(
                data = object,
                infoName = "timeDate as Julian")
        object }
    .objectMenu(
        params = list(
            series = "x",
            object2x = FALSE),
        infoName = "timeDate as Julian",
        console = "print(object[1:5])" )
}


.fCalendar.TimeDateMethods.16 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Extract data.frame of timeDate Atoms:
    .fun <<- function(series, object2x) { 
        x = eval(parse(text = series))
        object <<- atoms(x = x)
        if (object2x) 
            x <<- .saveAs(
                data = object,
                infoName = "timeDate as Atoms")
        object }
    .objectMenu(
        params = list(
            series = "x",
            object2x = FALSE),
        infoName = "timeDate as Atoms",
        console = "print(object)" )
}


.fCalendar.TimeDateMethods.17 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # ... Copy object to x:
    object <<- .infoObject(
        data = x, 
        infoName = "Object copied to x")
}


# ******************************************************************************
# Daylight Saving Time Rules


.fCalendar.E3Cmd.1 = 
function()  
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # List of Financial Centers:
    object <<- .infoObject(
        data = listFinCenter(), 
        infoName = "List of Financial Centers")
    .tkReport("List of Financial Ceneters")
}
    
    
.fCalendar.E3Cmd.2 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # DST Rules for a Financial Center:
    .fun <<- function(FinCenter) {
        .FUN <<- match.fun(FinCenter)
        .FUN() }
    .objectMenu(
        params = list(
            FinCenter = "Zurich"),
        infoName = "DST Rules",
        tkoutput = TRUE,
        title = "Daylight Saving Time Rules",
        description = date() )
}


# ******************************************************************************
# Time Series Class


.fCalendar.TimeSeriesClass.1 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Example timeSeries: x = MSFT|SP500 Values
    .msftsp500DataSet()
}


.fCalendar.TimeSeriesClass.2 = 
function()
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:

    # Print timeDate Class Representation:
    .getClass("timeSeries")  
}


.fCalendar.TimeSeriesClass.3 = 
function()
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:

    # is.timeSeries ?
    .fun <<- function(series) { 
        x = eval(parse(text = series))
        is.timeSeries(object = x) }
    .objectMenu(
        params = list(
            series = "x"),
        infoName = "is.timeSeries ?",
        console = "print(object)" )
}


.fCalendar.TimeSeriesClass.4 = 
function()
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:

    # Show Head of Series:
    .fun <<- function(series) { 
        x = eval(parse(text = series))
        head(x = x) }
    .objectMenu(
        params = list(
            series = "x"),
        infoName = "Head of Series",
        console = "print(object)" )
}


.fCalendar.TimeSeriesClass.5 = 
function()
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:

    # Show Tail of Series:
    .fun <<- function(series) { 
        x = eval(parse(text = series))
        tail(x = x) }
    .objectMenu(
        params = list(
            series = "x"),
        infoName = "Tail of Series",
        console = "print(object)" )
}


.fCalendar.TimeSeriesClass.6 = 
function()
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:

    # NYI - Apply a Function to Series:
    ans = "\nNot yet implemented!\n\n"
    print(ans)
}


.fCalendar.TimeSeriesClass.7 = 
function()
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:

    # Align Daily Series:
    .fun <<- function(series, method, include.weekends, object2x) { 
        x = eval(parse(text = series))
        ans = alignDailySeries(x = x, method = method, 
            include.weekends = include.weekends) 
        ans@FinCenter = x@FinCenter 
        object <<- ans
        if (object2x) 
            x <<- .saveAs(
                data = object,
                infoName = "Aligned Daily timeSeries")
        object }
    .objectMenu(
        params = list(
            series = "x",
            method = "before",
            include.weekends = TRUE,
            object2x = FALSE),
        infoName = "Aligned Daily timeSeries",
        console = "print(head(object))" )
}


.fCalendar.TimeSeriesClass.8 = 
function()
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:

    # Cut Series:
    .fun <<- function(series, from, to, object2x) {
        x = eval(parse(text = series))
        object <<- cutSeries(x = x, from = from, to = to) 
        if (object2x) 
            x <<- .saveAs(
                data = object,
                infoName = "Cutted timeSeries")
        object }
    .objectMenu(
        params = list(
            series = "x",
            from = "2000-01-01",
            to = "2000-12-31"),
        infoName = "Cutted timeSeries",
        console = "print(head(object))" )
}


.fCalendar.TimeSeriesClass.9 = 
function()
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:

    # Difference Series:
    .fun <<- function(series, lag, diff, trim, pa, object2x) {
        x = eval(parse(text = series))
        object <<- diffSeries(x = x, lag = lag, diff = diff, 
            trim = trim, pad = pad) 
        if (object2x) 
            x <<- .saveAs(
                data = object,
                infoName = "Differenced timeSeries")
        object }
    .objectMenu(
        params = list(
            series = "x",
            lag = 1, 
            diff = 1, 
            trim = FALSE, 
            pad = NA,
            object2x = FALSE),
        infoName = "Differenced timeSeries",
        console = "print(head(object))" )
}


.fCalendar.TimeSeriesClass.10 = 
function()
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:

    # Lag Series:
    .fun <<- function(series, k, trim, object2x) {
        x = eval(parse(text = series))
        k = as.integer(k)
        object <<- lagSeries(x = x, k = k, trim  = trim, colNames = NULL) 
        if (object2x) 
            x <<- .saveAs(
                data = object,
                infoName = "Lagged timeSeries")
        object }
    .objectMenu(
        params = list(
            series = "x",
            k = 1, 
            trim = FALSE,
            object2x = FALSE),
        infoName = "Lagged timeSeries",
        console = "print(head(object))" )
}


.fCalendar.TimeSeriesClass.11 = 
function()
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:

    # Merge Series:
    ans = "\nNot yet implemented!\n\n"
    print(ans)
}


.fCalendar.TimeSeriesClass.12 = 
function()
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:

    # Get Return Series:
    .fun <<- function(series, type, percentage, trim, digits, object2x) { 
        x = eval(parse(text = series))
        object <<- returnSeries(x = x, type = type, 
            percentage = percentage, trim = trim, digits = digits) 
        if (object2x) 
            x <<- .saveAs(
                data = object,
                infoName = "Return timeSeries")
        object }
    .objectMenu(
        params = list(
            series = "x",
            type = "continuous",
            percentage = FALSE, 
            trim = TRUE,
            digits = 4,
            object2x = FALSE),
        infoName = "Return timeSeries",
        console = "print(head(object))" )
}


.fCalendar.TimeSeriesClass.13 = 
function()
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:

    # Revert Series:
    .fun <<- function(series, object2x) { 
        x = eval(parse(text = series))
        object <<- revSeries(x = x) 
        if (object2x) 
            x <<- .saveAs(
                data = object,
                infoName = "Reverted timeSeries")
        object }
    .objectMenu(
        params = list(
            series = "x",
            object2x = FALSE),
        infoName = "Reverted timeSeries",
        console = "print(head(object))" )
}


.fCalendar.TimeSeriesClass.14 = 
function()
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:

    # ... copy object to x:
    x <<- .infoObject(
        data = object, 
        infoName = "saved object",
        console = "print(head(object))" )
}


# ******************************************************************************
# Holiday Calendars


.fCalendar.HolidayCalendars.1 = function() .fCalendar.E5Cmd(1)
.fCalendar.HolidayCalendars.2 = function() .fCalendar.E5Cmd(2)
.fCalendar.HolidayCalendars.3 = function() .fCalendar.E5Cmd(3)
.fCalendar.HolidayCalendars.4 = function() .fCalendar.E5Cmd(4)


.fCalendar.HolidayCalendars.1 = 
function()
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:

    # Date of Easter & Related Dates:
    .fun <<- function(year, shift) {
        year = eval(parse(text = year)) 
        easter(year = year, shift = shift) }
    .objectMenu(
        params = list(
            year = as.character(currentYear),
            shift = 0 ),
        infoName = "Date of Easter",
        console = "print(object)" )
}


.fCalendar.HolidayCalendars.2 = 
function()
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:

    # List of Supported Holidays:  
    object <<- .infoObject(
        data = .holidayList(), 
        infoName = "Holiday List")
    .tkReport("Holiday List")
}


.fCalendar.HolidayCalendars.3 = 
function()
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:

    # Return Holiday Date:
    .fun <<- function(year, Holiday) {
        year = eval(parse(text = year))
        object <<- holiday(year, Holiday)
        .tkReport(paste("Holiday Dates", "-", Holiday))
        object }
    .objectMenu(
        params = list(
            year = as.character(currentYear),
            Holiday = "LaborDay" ),
        infoName = "Holiday Dates")
}


.fCalendar.HolidayCalendars.4 = 
function()
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:

    # NYSE Holiday Calendar:
    .fun <<- function(fromYear, toYear) {
        years = seq(fromYear, toYear, by = sign(toYear-fromYear))
        object <<- holiday.NYSE(years) 
        .tkReport("NYSE Holiday Dates")
        object }
    .objectMenu(
        params = list(
            fromYear = 2005,
            toYear = 2005),
        infoName = "NYSE Holiday Dates" )
}


# ******************************************************************************
# High Frequency Data Tools


.fCalendar.HighFrequencyData.1  = function() .fCalendar.F1Cmd(1)
.fCalendar.HighFrequencyData.2  = function() .fCalendar.F1Cmd(2)
.fCalendar.HighFrequencyData.3  = function() .fCalendar.F1Cmd(3)
.fCalendar.HighFrequencyData.4  = function() .fCalendar.F1Cmd(4)
.fCalendar.HighFrequencyData.5  = function() .fCalendar.F1Cmd(5)
.fCalendar.HighFrequencyData.6  = function() .fCalendar.F1Cmd(6)
.fCalendar.HighFrequencyData.7  = function() .fCalendar.F1Cmd(7)
.fCalendar.HighFrequencyData.8  = function() .fCalendar.F1Cmd(8)
.fCalendar.HighFrequencyData.9  = function() .fCalendar.F1Cmd(9)
.fCalendar.HighFrequencyData.10 = function() .fCalendar.F1Cmd(10)
.fCalendar.HighFrequencyData.11 = function() .fCalendar.F1Cmd(11)
.fCalendar.HighFrequencyData.12 = function() .fCalendar.F1Cmd(12)
.fCalendar.HighFrequencyData.13 = function() .fCalendar.F1Cmd(13)
.fCalendar.HighFrequencyData.14 = function() .fCalendar.F1Cmd(14)
.fCalendar.HighFrequencyData.15 = function() .fCalendar.F1Cmd(15)


.fCalendar.HighFrequencyData.1 = 
function()
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:

    # * Example data.frame: x = FX Reuters USDTHB:
    data(usdthb)
    x <<- .saveAs(
        data = usdthb, 
        infoName = "FX Reuters USDTHB",
        console = "print(tail(data))" )
}


.fCalendar.HighFrequencyData.2 = 
function()
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:

    # FX Parser for Reuters Data:
    .fun <<- function(series, include) {
        x = eval(parse(text = series))
        object <<- fxdata.contributors(x = x, include = include)
        print(object)
        cat("\n")
        x <<- .saveAs(
            data = fxdata.parser(x = x, parser.table = object),
            infoName = "parsed Data") 
        y = head(x)
        y[, 1] = as.character(y[, 1])
        print(y)
        cat("...\n")
        object }
    .objectMenu(
        params = list(
            series = "x",
            include = 5),
        infoName = "FX Data Parser")            
}


.fCalendar.HighFrequencyData.3 = 
function()
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:

    # FX Filter for Reuters Data:
    .fun <<- function(parameter) {
        par(mfrow = c(2, 1))
        NumberOfRecords = length(x[, 1])
        yMin = min ( c( x[, 4], -100*log(x[1, 4]/x[, 4]) ) )
        yMax = max ( c( x[, 4], -100*log(x[1, 4]/x[, 4]) ) )
        plot(x[, 4], type = "l", 
            xlab = "Reuters FX Data", 
            ylab = "100*log(Bid[n]/Bid[1])      Bid",
            ylim = c(-20,30), main = "Unfiltered Data")
        lines(x = c(1, NumberOfRecords), y = rep(x[1, 4], 2), col = 4)
        lines(-100*log(x[1, 4]/x[, 4]))
        lines(x = c(1, NumberOfRecords), y = c(0, 0), col = 4)
        # Filter the data:
        x.filt = fxdata.filter(x = x, parameter = parameter)
        # Quick And Dirty Time Scaling
        Records = length(x.filt$accepted[, 4])
        scale = NumberOfRecords/Records
        # Plot filtered data:
        yMin = min ( c(x.filt$accepted[, 4], 
            -100*log(x.filt$accepted[1, 4]/x.filt$accepted[, 4])))
        yMax = max ( c(x.filt$accepted[, 4], 
            -100*log(x.filt$accepted[1, 4]/x.filt$accepted[, 4])))
        plot(x = (1:Records)*scale, y = x.filt$accepted[, 4], type = "l", 
            xlab = "Reuters FX Data", 
            ylab = "100*log(Bid[n]/Bid[1])      Bid", 
            ylim = c(yMin, yMax), main = "Filtered Data")
        y = rep(x.filt$accepted[1, 4], 2)
        lines(x = c(1, NumberOfRecords), y = y, col = 4)
        y = -100*log(x.filt$accepted[1, 4]/x.filt$accepted[, 4])
        lines(x = (1:Records)*scale, y = y)
        lines(x = c(1, NumberOfRecords), y = c(0, 0), col = 4)
        object <<- x.filt
        x.filt }
    .objectMenu(
        params = list(
            parameter = "strong"),
        infoName = "Filtered FX Data",
        tkoutput = TRUE )  
}


.fCalendar.HighFrequencyData.4 = 
function()
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:

    # FX Format on Variable Minutes Scale:
    .fun <<- function(series, digits) {
        x = eval(parse(text = series))
        fxdata.varmin(x = x, digits = digits) }
    .objectMenu(
        params = list(
            series = "x",
            digits = 4),
        infoName = "Variable Minutes formatted FX Data",
        console = "print(head(object))" )  
}


.fCalendar.HighFrequencyData.5 = 
function()
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:

    # * Example data.frame: x = FX Reuters AUDUSD:
    data(audusd)
    x <<- .saveAs(
        data = audusd, 
        infoName = "FX Reuters AUDUSD",
        console = "print(tail(data))" )
}


.fCalendar.HighFrequencyData.6 = 
function()
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:

    # Extract BID Prices from Reuters FX Data:
    .fun <<- function(ReutersData, object2x) {
        x = eval(parse(text = ReutersData))
        object <<- list(t = x[, "XDATE"], x = x[, "BID"])
        if (object2x) 
            x <<- .saveAs(
                data = object, 
                infoName = "List of FX Prices")
        object }
    .objectMenu(
        params = list(
            ReutersData = "x",
            object2x = TRUE ),
        infoName = "List of FX Prices",
        console = "print(tail(data.frame(object)))" )  
}


.fCalendar.HighFrequencyData.7 = 
function()
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:

    # Compute log Prices from Prices:
    .fun <<-  function(Prices, object2x) {
        x = eval(parse(text = Prices))
        object  <<- xts.log(xts = x) 
        if (object2x) 
            x <<- .saveAs(
                data = object, 
                infoName = "List of Log FX Prices")
        object }
    .objectMenu(
        params = list(
            Prices = "x",
            object2x = FALSE),
        infoName = "List of Log FX Prices",
        console = "print(tail(data.frame(object)))" )   
}


.fCalendar.HighFrequencyData.8 = 
function()
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:

    # Compute log Returns from log Prices:
    .fun <<-  function(logPrices, object2x) {
        x = eval(parse(text = logPrices))
        object  <<- xts.diff(xts.log(xts = x))
        if (object2x) 
            x <<- .saveAs(
                data = object, 
                infoName = "List of Log FX Returns")
        object }
    .objectMenu(
        params = list(
            logPrices = "x",
            object2x = TRUE),
        infoName = "List of Log FX Returns",
        console = "print(tail(data.frame(object)))" )
}


.fCalendar.HighFrequencyData.9 = 
function()
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:

    # Cut Series - Prices|logPrices|logReturns
    .fun <<-  function(series, object2x) {
        x = eval(parse(text = series))
        object  <<- xts.cut(xts = x, from.date = from, to.date = to)
        if (object2x) 
            x <<- .saveAs(
                data = object, 
                infoName = "List of Cutted Series")
        object }
    .objectMenu(
        params = list(
            series = "x",
            from = 19971021,
            to = 19971021,
            object2x = FALSE),
        infoName = "List of Cutted Series",
        console = "print(tail(data.frame(object)))" )
}


.fCalendar.HighFrequencyData.10 = 
function()
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:

    # Interpolate - Prices|logPrices|logReturns
    .fun <<-  function(series, delta, method, object2x) {
        x = eval(parse(text = series))
        object  <<- xts.interp(xts = x, delta = delta, method = method)
        if (object2x) 
            x <<- .saveAs(
                data = object, 
                infoName = "List of Cutted Series")
        object }
    .objectMenu(
        params = list(
            series = "x",
            delta = 1,
            method = "constant",
            object2x = FALSE),
        infoName = "List of Cutted Series",
        console = "print(tail(data.frame(object)))" )
}


.fCalendar.HighFrequencyData.11 = 
function()
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:

    # Convert List to timeSeries Object",
}


.fCalendar.HighFrequencyData.12 = 
function()
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:

    data(fdax97m)
    x <<- .saveAs(
        data = as.timeSeries(fdax97m, format = "%Y%m%d%H%M"),
        infoName = "FDAX Index 1997")
}


.fCalendar.HighFrequencyData.13 = 
function()
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:

    # De-seasonalize in Upsilon Time:
    .fun <<-  function(series, from, to, delta, alpha = alpha, doplot) {
        prices = eval(parse(text = series))
        # Settings:
        options(object.size = 5e8)
        par(mfrow = c(2, 2), cex = 0.7)
        if (doplot) {
            plot(prices, col = "steelblue", 
                main = "Values in Physical Time")
            plot(returnSeries(prices), col = "steelblue",
                main = "Returns in Physical Time")
        }
        z = x@positions
        positions = paste(substring(z, 1, 4), substring(z, 6, 7), 
            substring(z, 9, 10), substring(z, 12, 13), 
            substring(z, 15, 16), sep = "")
        prices = list(t = as.numeric(positions), x = as.vector(x@Data))
        MeanTimeInterval = delta
        # Load Example Data File:
        prices = xts.cut(prices, from.date = from, to.date = to)          
        # Create Hourly Upsilon Time Map:
        tmap = xts.map(prices, mean.deltat = MeanTimeInterval, 
            alpha = alpha)      
        # Extract Data Records According to Time Map:
        upsilon.prices = xts.upsilon(prices, weekly.map = tmap$ymap, 
            doplot = doplot, main = "Prices in Upsilon Time")
        if (doplot) {
            plot(x = tmap$xmap, y = tmap$ymap, type = "l", 
                main = "Time Mapping") 
        }
        series = timeSeries(
            data = as.vector(upsilon.prices$x), 
            charvec = as.character(upsilon.prices$t),
            format = "%Y%m%d%H%M",
            FinCenter = "GMT")
        list(data = series, tmap = tmap)
        }
    .objectMenu(
        params = list(
            series = "x",
            from = 19970106,
            to = 19971228,
            delta = 60,
            alpha = 1.05,
            doplot = TRUE),
        infoName = "Series & Time Map")     
}


.fCalendar.HighFrequencyData.14 = 
function()
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:

    .fun <<- function(series, kParameter, doplot) {
        prices = eval(parse(text = series))
        # Settings:
        options(object.size = 5.0e8)
        par(mfrow = c(2, 2), cex = 0.7)
        if (doplot) {
            plot(prices, col = "steelblue",
                main = "Values in Physical Time")
            plot(returnSeries(prices), col = "steelblue",
                main = "Returns in Physical Time")
        }
        # Prices:
        z = x@positions
        nPhysical = length(z)
        positions = paste(substring(z, 1, 4), substring(z, 6, 7), 
            substring(z, 9, 10), substring(z, 12, 13), 
            substring(z, 15, 16), sep = "")
        prices = list(t = as.numeric(positions), x = as.vector(x@Data))
        logprices = xts.log(prices)  
        returns = xts.diff(logprices)   
        # Devolatilize Time Series With dv-Series Algorithm:
        AverageVolatility = 10*var(returns$x)
        dvseries = xts.dvs(prices, k = kParameter, 
            volatility = AverageVolatility,
            main = "De-Volatilized Prices", doplot = doplot) 
        # Quantile =- Quantile Plot
        if (doplot) {
            qqgaussPlot(diff(dvseries$x), main = "QQplot: dv-series") 
        }   
        nBusiness = length(dvseries$x)
        print(c(nBusiness = nBusiness, nPhysical = nPhysical, 
            ratio = nBusiness/nPhysical))
        series = timeSeries(
            data = as.vector(dvseries$x), 
            charvec = as.character(dvseries$t),
            format = "%Y%m%d%H%M",
            FinCenter = "GMT")
        list(data = series, params = c(kParameter = kParameter,
            AverageVolatility = AverageVolatility))
    }
    .objectMenu(
        params = list(
            series = "x",
            kParameter = 8,
            doplot = TRUE),
        infoName = "Series & Parameters")        
}


.fCalendar.HighFrequencyData.15 = 
function()
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:

    # Plot Daily/Weekly Volatility Charts:
    .fun <<- function(series, from, to, period, deltat, doplot) {
        prices = eval(parse(text = series))
        # Settings:
        options(object.size = 5.0e8)
        par(mfrow = c(2, 2), cex = 0.7)
        if (doplot) {
            plot(prices, main = "Values in Physical Time")
            plot(returnSeries(prices), main = "Returns in Physical Time")
        }
        # Prices:
        z = x@positions
        positions = paste(substring(z, 1, 4), substring(z, 6, 7), 
            substring(z, 9, 10), substring(z, 12, 13), 
            substring(z, 15, 16), sep = "")
        xts = list(t = as.numeric(positions), x = as.vector(x@Data))
        xts = xts.cut(xts, from.date = from, to.date = to)
        # Create Daily and Weekly Histograms:
        result = xts.dwh (xts, period = "both", dolog = TRUE, 
            dodiff = TRUE, deltat = delta, doplot = doplot) 
        }           
    .objectMenu(
        params = list(
            series = "x",
            from = 19970106,
            to = 19971228,
            period = "both",
            delta = 30,
            doplot = TRUE),
        infoName = "Series & Time Map")     
}


################################################################################

