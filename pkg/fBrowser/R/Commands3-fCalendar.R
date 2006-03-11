
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
# fCalendar Commands


# ******************************************************************************
# Time Date Class


.fCalendar.TimeDateClass.dates = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # * Example timeDate: x = 12 Random Dates in Current Year
    myFunction = function(FinCenter, object2x) {
        y = rep(currentYear, 12)
        d = trunc(runif(12, 1, 29))
        m = trunc(runif(12, 1, 13))
        object <<- timeCalendar(y = y, m = m, d = d, FinCenter = FinCenter)
        object }
    tkExecute(
        fun = myFunction,
        params = list( 
            FinCenter = "GMT",
            object2x = TRUE),
        infoName = "12 Random Dates in Current Year",
        tkoutput = TRUE,
        console = NULL,
        title = "12 Random Dates in Current Year",
        description = NULL )          
}


.fCalendar.TimeDateClass.times = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # * Example timeDate: x = 12 Random Times in Current Year 
    myFunction = function(FinCenter, object2x) {
        y = rep(2005, 12)
        d = trunc(runif(12, 1, 29))
        m = trunc(runif(12, 1, 13))
        h = trunc(runif(12, 0, 24))
        min = trunc(runif(12, 0, 60))
        s = trunc(runif(12, 0, 60))
        object <<- timeCalendar(y = y, m = m, d = d, h = h, 
            min = min, s = s, FinCenter = FinCenter)
        object }
    tkExecute(
        fun = myFunction,
        params = list( 
            FinCenter = "GMT",
            object2x = TRUE),
        infoName = "12 Random Times in Current Year",
        tkoutput = TRUE,
        console = NULL,
        title = "12 Random Times in Current Year",
        description = NULL )          
}


.fCalendar.TimeDateClass.4 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Print timeDate Class Representation:
    tkGetClass("timeDate")
}


.fCalendar.TimeDateClass.5 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Create timeDate Sequence:
    myFunction = function(from, to, by, length.out, format, FinCenter, 
        object2x, report) {
        object <<- timeSequence(from = from, to = to, by = by, 
            length.out = eval(parse(text = length.out)), format = format, 
            FinCenter = FinCenter) 
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            from = "2005-01-01", 
            to = "2005-12-31", 
            by = "day", 
            length.out = "NULL", 
            format = "%Y-%m-%d", 
            FinCenter = "GMT",
            object2x = FALSE,
            report = TRUE),
        infoName = "timeDate Sequence",
        tkoutput = FALSE,
        console = "print(object[1:5])",
        title = "timeDate Sequence",
        description = NULL )          
}


.fCalendar.TimeDateClass.6 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Create timeDate Calendar:
    myFunction = function(y, m, d, h, min, s, FinCenter, object2x, report) {
        object <<- timeCalendar(
            y = eval(parse(text = y)),
            m = eval(parse(text = m)), 
            d = eval(parse(text = d)),
            h = eval(parse(text = h)), 
            min = eval(parse(text = min)),
            s = eval(parse(text = s)), 
            FinCenter = FinCenter) 
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            y = "2005", 
            m = "1:12", 
            d = "1", 
            h = "NULL", 
            min = "NULL", 
            s = "NULL",
            FinCenter = "GMT",
            object2x = FALSE,
            report = TRUE),
        infoName = "timeDate Calendar",
        tkoutput = FALSE,
        console = "print(object[1:5])",
        title = "timeDate Calendar",
        description = NULL )          
}


.fCalendar.TimeDateClass.7 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Current Date and Time:
    tkGetTime()
}


.fCalendar.TimeDateClass.8 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Last Day in Month for a Given Date:
    myFunction = function(series, format, FinCenter, 
        object2x, report) {
        x = eval(parse(text = series))
        object <<- timeLastDayInMonth(charvec = as.character(x), 
            format = format, FinCenter = FinCenter) 
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            format = "%Y-%m-%d",
            FinCenter = "GMT",
            object2x = FALSE,
            report = TRUE),
        infoName = "Last Day in Month",
        tkoutput = TRUE,
        console = "print(object[1:5])",
        title = "Last Day in Month",
        description = NULL )          
}


.fCalendar.TimeDateClass.9 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # The N-Day On-Or-After a given Date:
    myFunction = function(series, nday, format, FinCenter, 
        object2x, report) {
        x = eval(parse(text = series))
        object <<- timeNdayOnOrAfter(charvec = as.character(x), 
            nday = nday, format = format, FinCenter = FinCenter) 
        object } 
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            nday = 1,
            format = "%Y-%m-%d",
            FinCenter = "GMT",
            object2x = FALSE,
            report = TRUE),
        infoName = "The N-Day On-Or-After",
        tkoutput = TRUE,
        console = "print(object[1:5])",
        title = "The N-Day On-Or-After",
        description = NULL )               
}


.fCalendar.TimeDateClass.10 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # The N-Day On-Or-Before a given Date:
    myFunction = function(series, nday, format, FinCenter, 
        object2x, report) {
        x = eval(parse(text = series))
        object <<- timeNdayOnOrBefore(charvec = as.character(x), nday = nday, 
            format = format, FinCenter = FinCenter) 
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            nday = 1,
            format = "%Y-%m-%d",
            FinCenter = "GMT",
            object2x = FALSE,
            report = TRUE),
        infoName = "The N-Day On-Or-Before",
        tkoutput = TRUE,
        console = "print(object[1:5])",
        title = "The N-Day On-Or-Before",
        description = NULL )              
}


.fCalendar.TimeDateClass.11 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # The n-th ocurrance of a n-day in year/month
    myFunction = function(series, nday, nth, format, FinCenter, 
        object2x, report = TRUE) {
        x = eval(parse(text = series))
        object <<- timeNthNdayInMonth(charvec = as.character(x), 
            nday = nday, nt = nth, format = format, FinCenter = FinCenter)  
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            nday = 1,
            nth = 1, 
            format = "%Y-%m-%d",
            FinCenter = "GMT",
            object2x = FALSE,
            report = TRUE),
        infoName = "The n-th Ocurrance of a n-Day",
        tkoutput = TRUE,
        console = "print(object[1:5])",
        title = "The n-th Ocurrance of a n-Day",
        description = NULL )              
}


.fCalendar.TimeDateClass.12 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # The last n-day in year/month 
    myFunction = function(series, nday, format, FinCenter, 
        object2x, report) {
        x = eval(parse(text = series))
        object <<- timeLastNdayInMonth(charvec = as.character(x), 
            nday = nday, format = format, FinCenter = FinCenter)  
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            nday = 1,
            format = "%Y-%m-%d",
            FinCenter = "GMT",
            object2x = FALSE,
            report = TRUE),
        infoName = "The Last N-Day",
        tkoutput = TRUE,
        console = "print(object[1:5])",
        title = "The Last N-Day",
        description = NULL )              
}


.fCalendar.TimeDateClass.13 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # List of Financial Centers
    tkGetFinCenters()
}


.fCalendar.TimeDateClass.14 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # DST Rules for a Financial Center:
    myFunction = function(FinCenter){
        FUN = match.fun(FinCenter)
        object <<- FUN()
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            FinCenter = "Zurich"),
        infoName = "DST Rules",
        tkoutput = TRUE,
        console = NULL,
        title = "Daylight Saving Time Rules",
        description = NULL )
}


.fCalendar.TimeDateClass.15 = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Print Summary of a 'timeDate' object
    tkSummary(x, title = "timeDate Summary")
}                                               



# ******************************************************************************
# Time Date Methods


.fCalendar.TimeDateMethods.dates = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # * Example timeDate: x = 12 Random Dates in Current Year
    myFunction = function(FinCenter, object2x) {
        y = rep(currentYear, 12)
        d = trunc(runif(12, 1, 29))
        m = trunc(runif(12, 1, 13))
        object <<- timeCalendar(y = y, m = m, d = d, FinCenter = FinCenter)
        object }
    tkExecute(
        fun = myFunction,
        params = list( 
            FinCenter = "GMT",
            object2x = TRUE),
        infoName = "12 Random Dates in Current Year",
        tkoutput = TRUE,
        console = NULL,
        title = "12 Random Dates in Current Year",
        description = NULL )          
}


.fCalendar.TimeDateMethods.times = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # * Example timeDate: x = 12 Random Times in Current Year 
    myFunction = function(FinCenter, object2x) {
        y = rep(2005, 12)
        d = trunc(runif(12, 1, 29))
        m = trunc(runif(12, 1, 13))
        h = trunc(runif(12, 0, 24))
        min = trunc(runif(12, 0, 60))
        s = trunc(runif(12, 0, 60))
        object <<- timeCalendar(y = y, m = m, d = d, h = h, 
            min = min, s = s, FinCenter = FinCenter)
        object }
    tkExecute(
        fun = myFunction,
        params = list( 
            FinCenter = "GMT",
            object2x = TRUE),
        infoName = "12 Random Times in Current Year",
        tkoutput = TRUE,
        console = NULL,
        title = "12 Random Times in Current Year",
        description = NULL )          
}


.fCalendar.TimeDateMethods.4 = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Print timeDate Class Representation:
    tkGetClass("timeDate") 
}


.fCalendar.TimeDateMethods.5 = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Extract the first object of a timeDate Vector
    myFunction = function(series, object2x) {
        x = eval(parse(text = series))
        object <<- start(modify(x = x, "sort"))
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            object2x = FALSE),
        infoName = "First timeDate Value",
        tkoutput = TRUE,
        console = NULL,
        title = "First timeDate Value",
        description = NULL )
}


.fCalendar.TimeDateMethods.6 = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Extract the last object of a timeDate Vector:
    myFunction = function(series, object2x) {
        x = eval(parse(text = series))
        object <<- end(modify(x = x, "sort"))
        object  }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            object2x = FALSE),
        infoName = "Last timeDate Value",
        tkoutput = TRUE,
        console = NULL,
        title = "Last timeDate Value",
        description = NULL )
}


.fCalendar.TimeDateMethods.7 = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # +/- a time Span from a timeDate Vector
    myFunction = function(series, PlusMinus, days, hours, minutes, 
        seconds, object2x, report) {
        x = eval(parse(text = series))
        ans = days*24*60*60 + hours*60*60 + minutes*60 + seconds
        if (PlusMinus == "+") object <<- x + ans
        if (PlusMinus == "-") object <<- x - ans
        object}
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            PlusMinus = "+",
            days = 1,
            hours = 0,
            minutes = 0,
            seconds = 0,
            object2x = FALSE,
            report = TRUE),
        infoName = "+/- Time Span a timeDate Object",
        tkoutput = FALSE,
        console = "print(object[1:5])",
        title = "+/- Time Span a timeDate Object",
        description = NULL )          
}


.fCalendar.TimeDateMethods.8 = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Sort a timeDate Vector by Time and Date:
    myFunction = function(series, object2x, report) {
        x = eval(parse(text = series))
        object <<- modify(x = x, method = "sort") 
        object}  
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            object2x = FALSE,
            report = TRUE),
        infoName = "Sort a timeDate Object",
        tkoutput = FALSE,
        console = "print(object[1:5])",
        title = "Sort a timeDate Vector",
        description = NULL )          
}


.fCalendar.TimeDateMethods.9 = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Round a timeDate Vector to a Given Unit:
    myFunction = function(series, units, object2x, report) {
        x = eval(parse(text = series))  
        object <<- modify(x = x, method = "round", units = units) 
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            units = "days",
            object2x = FALSE,
            report = TRUE),
        infoName = "Round a timeDate Object",
        tkoutput = FALSE,
        console = "print(object[1:5])",
        title = "Round a timeDate Object",
        description = NULL )
}


.fCalendar.TimeDateMethods.10 = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Truncate a timeDate Vector to a Given Unit:
    myFunction = function(series, units, object2x, report) { 
        x = eval(parse(text = series))
        object <<- modify(x, method = "trunc", units = units)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            units = "days",
            object2x = FALSE,
            report = TRUE),
        infoName = "Truncate a timeDate Object",
        console = "print(object[1:5])",
        title = "Truncate a timeDate Object",
        description = NULL )
}


.fCalendar.TimeDateMethods.11 = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Return a timeDate Vector in Reverse Order:
    myFunction = function(series, object2x, report) { 
        x = eval(parse(text = series))
        object <<- rev(x = x)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            object2x = FALSE,
            report = TRUE),
        infoName = "Revert a timeDate Object",
        tkoutput = TRUE,
        console = "print(object[1:5])",
        title = "Revert a timeDate Object",
        description = NULL )
}


.fCalendar.TimeDateMethods.12 = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Convert timeDate to a character vector:
    myFunction = function(series, object2x, report) { 
        x = eval(parse(text = series))
        object <<- as.character(x = x) 
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            object2x = FALSE,
            report = TRUE),
        infoName = "timeDate Object to Character",
        tkoutput = TRUE,
        console = "print(head(object))",
        title = "timeDate Object to Character",
        description = NULL )
}


.fCalendar.TimeDateMethods.13 = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Convert timeDate to a data frame:
    myFunction = function(series, object2x, report) { 
        x = eval(parse(text = series))
        object <<- as.data.frame(x = x)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            object2x = FALSE, 
            report = TRUE),
        infoName = "timeDate Object to Data Frame",
        tkoutput = TRUE,
        console = "print(head(object))",
        title = "timeDate Object to Data Frame",
        description = NULL )
}


.fCalendar.TimeDateMethods.14 = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Convert timeDate to a POSIXct Object:
    myFunction = function(series, object2, report) { 
        x = eval(parse(text = series))
        object <<- as.POSIXct(x = x)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            object2x = FALSE,
            report = TRUE),
        infoName = "timeDate Object to POSIXct",
        tkoutput = TRUE,
        console = "print(head(object))",
        title = "timeDate Object to POSIXct",
        description = NULL )
}


.fCalendar.TimeDateMethods.15 = 
function() 
{   # A function implemented by Diethelm Wuertz

    # Julian Day Counts from timeDate Vector:
    myFunction = function(series, object2x, report) { 
        x = eval(parse(text = series))
        object <<- julian(x = x)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            object2x = FALSE,
            report = TRUE),
        infoName = "timeDate Object to Julian Counts",
        tkoutput = TRUE,
        console = "print(object[1:5])",
        title = "timeDate Object to Julian Counts",
        description = NULL )
}


.fCalendar.TimeDateMethods.16 = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Extract data.frame of timeDate Atoms:
    myFunction = function(series, object2x, report) { 
        x = eval(parse(text = series))
        object <<- atoms(x = x)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            object2x = FALSE,
            report = TRUE),
        infoName = "Extract Atoms from timeDate Object",
        tkoutput = TRUE,
        console = "print(object)",
        title = "Extract Atoms from timeDate Object",
        description = NULL )
}


# ******************************************************************************
# Time Series Class


.fCalendar.TimeSeriesClass.msftsp500Monthly = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Example timeSeries: x = MSFT|SP500 Values
    tkGetData(Data = "msftsp500Monthly", infoName = "Return Series with NA")  
}


.fCalendar.TimeSeriesClass.getClass = 
function()
{   # A function implemented by Diethelm Wuertz

    # Print timeDate Class Representation:
    tkGgetClass("timeSeries")  
}


.fCalendar.TimeSeriesClass.timeSeries = 
function()
{   # A function implemented by Diethelm Wuertz

    # Create timeSeries Object:
    myFunction = function(data, charvec, units, format, zone, FinCenter,
        pbject2x, record) { 
        data = eval(parse(text = data))
        charvec = as.character(eval(parse(text = charvec)))
        units = as.character(eval(parse(text = units)))
        object <<- timeSeries(data = data, charvec = charvec, 
            units = units, format = format, zone = zone , 
            FinCenter = FinCenter)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            data = "matrix(rnorm(24), ncol=2)",
            charvec = "timeCalendar(y=2006, m=1:12)",
            units = "LETTERS[1:2]",
            format = "%Y-%m-%d",
            zone = "GMT",
            FinCenter = "GMT",
            object2x = TRUE,
            report = TRUE),
        infoName = "Create a timSeries Object",
        tkoutput = TRUE,
        console = "print(object)",
        title = "Create a timSeries Object",
        description = NULL )
}


.fCalendar.TimeSeriesClass.applySeries = 
function()
{   # A function implemented by Diethelm Wuertz

    # Apply a Function to Series:
    myFunction = function(series, from, to, FUN, object2x, report) {
        x = eval(parse(text = series))
        FUN = match.fun(FUN)
        object <<- applySeries(x = x, from = from, to = to, FUN = FUN,
            units = NULL) 
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            from = "2000-01-01",
            to = "2000-12-31",
            FUN = "colAvgs",
            object2x = FALSE,
            report = TRUE),
        infoName = "Apply FUN to timeSeries Object",
        tkoutput = TRUE,
        console = "print(head(object))",
        title = "Apply FUN to timeSeries Object",
        description = NULL )
}


.fCalendar.TimeSeriesClass.alignSeries = 
function()
{   # A function implemented by Diethelm Wuertz

    # Align Daily Series:
    myFunction = function(series, method, include.weekends, object2x) { 
        x = eval(parse(text = series))
        ans = alignDailySeries(x = x, method = method, 
            include.weekends = include.weekends) 
        ans@FinCenter = x@FinCenter 
        object <<- ans
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            method = "before",
            include.weekends = TRUE,
            object2x = FALSE),
        infoName = "Aligned Daily timeSeries",
        tkoutput = TRUE,
        console = "print(head(object))",
        title = NULL,
        description = NULL )
}


.fCalendar.TimeSeriesClass.cutSeries = 
function()
{   # A function implemented by Diethelm Wuertz

    # Cut Series:
    myFunction = function(series, from, to, object2x) {
        x = eval(parse(text = series))
        object <<- cutSeries(x = x, from = from, to = to) 
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            from = "2000-01-01",
            to = "2000-12-31"),
        infoName = "Cutted timeSeries",
        tkoutput = TRUE,
        console = "print(head(object))",
        title = NULL,
        description = NULL )
}


.fCalendar.TimeSeriesClass.diffSeries = 
function()
{   # A function implemented by Diethelm Wuertz

    # Difference Series:
    myFunction = function(series, lag, diff, trim, pa, object2x) {
        x = eval(parse(text = series))
        object <<- diffSeries(x = x, lag = lag, diff = diff, 
            trim = trim, pad = pad) 
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            lag = 1, 
            diff = 1, 
            trim = FALSE, 
            pad = NA,
            object2x = FALSE),
        infoName = "Differenced timeSeries",
        tkoutput = TRUE,
        console = "print(head(object))",
        title = NULL,
        description = NULL )
}


.fCalendar.TimeSeriesClass.lagSeries = 
function()
{   # A function implemented by Diethelm Wuertz

    # Lag Series:
    myFunction = function(series, k, trim, object2x) {
        x = eval(parse(text = series))
        k = as.integer(k)
        object <<- lagSeries(x = x, k = k, trim  = trim, colNames = NULL) 
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            k = 1, 
            trim = FALSE,
            object2x = FALSE),
        infoName = "Lagged timeSeries",
        tkoutput = TRUE,
        console = "print(head(object))",
        title = NULL,
        description = NULL )
}


.fCalendar.TimeSeriesClass.mergeSeries = 
function()
{   # A function implemented by Diethelm Wuertz

    # Merge timeSeries Objects:
    myFunction = function(series, matrix, object2x, report) { 
        x = eval(parse(text = series))
        data = eval(parse(text = matrix))
        object <<- mergeSeries(x = x, y = data, units = NULL)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            matrix = "object@Data",
            object2x = FALSE,
            report = TRUE),
        infoName = "Merge Series with Matrix",
        tkoutput = FALSE,
        console = "print(head(object))",
        title = "Merge Series with Matrix",
        description = NULL )
}


.fCalendar.TimeSeriesClass.returnSeries = 
function()
{   # A function implemented by Diethelm Wuertz

    # Get Return Series:
    myFunction = function(series, type, percentage, trim, digits, 
        object2x, report) { 
        x = eval(parse(text = series))
        object <<- returnSeries(x = x, type = type, 
            percentage = percentage, trim = trim, digits = digits) 
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            type = "continuous",
            percentage = FALSE, 
            trim = TRUE,
            digits = 4,
            object2x = FALSE,
            report = TRUE),
        infoName = "Compute Return Series",
        tkoutput = FALSE,
        console = "print(head(object))",
        title = "Compute Return Series",
        description = NULL )
}


.fCalendar.TimeSeriesClass.revSeries = 
function()
{   # A function implemented by Diethelm Wuertz

    # Revert Series:
    myFunction = function(series, object2x, report) { 
        x = eval(parse(text = series))
        object <<- revSeries(x = x) 
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            object2x = FALSE,
            report = TRUE),
        infoName = "Revert timeSeries Object",
        tkoutput = FALSE,
        console = "print(head(object))",
        title = "Revert timeSeries Object",
        description = NULL )
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

    # Date of Easter & Related Dates:
    myFunction = function(year, shift) {
        year = eval(parse(text = year)) 
        easter(year = year, shift = shift) }
    tkExecute(
        fun = myFunction,
        params = list(
            year = as.character(currentYear),
            shift = 0 ),
        infoName = "Date of Easter",
        tkoutput = TRUE,
        console = "print(object)",
        title = NULL,
        description = NULL )
}


.fCalendar.HolidayCalendars.2 = 
function()
{   # A function implemented by Diethelm Wuertz

    # List of Supported Holidays:  
    object <<- .infoObject(
        data = .holidayList(), 
        infoName = "Holiday List")
    .tkReport("Holiday List")
}


.fCalendar.HolidayCalendars.3 = 
function()
{   # A function implemented by Diethelm Wuertz

    # Return Holiday Date:
    myFunction = function(year, Holiday) {
        year = eval(parse(text = year))
        object <<- holiday(year, Holiday)
        .tkReport(paste("Holiday Dates", "-", Holiday))
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            year = as.character(currentYear),
            Holiday = "LaborDay" ),
        infoName = "Holiday Dates",
        tkoutput = TRUE,
        console = NULL,
        title = NULL,
        description = NULL )
}


.fCalendar.HolidayCalendars.4 = 
function()
{   # A function implemented by Diethelm Wuertz

    # NYSE Holiday Calendar:
    myFunction = function(fromYear, toYear) {
        years = seq(fromYear, toYear, by = sign(toYear-fromYear))
        object <<- holiday.NYSE(years) 
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            fromYear = 2005,
            toYear = 2005),
        infoName = "NYSE Holiday Dates",
        tkoutput = TRUE,
        console = NULL,
        title = NULL,
        description = NULL )
}


# ******************************************************************************
# High Frequency Data Tools


.fCalendar.HighFrequencyData.1 = 
function()
{   # A function implemented by Diethelm Wuertz

    # * Example data.frame: x = FX Reuters USDTHB:
    data(usdthb)
    x <<- tkSaveAs(
        data = usdthb, 
        infoName = "FX Reuters USDTHB",
        tkoutput = TRUE,
        console = "print(tail(data))",
        title = NULL,
        description = NULL )
}


.fCalendar.HighFrequencyData.2 = 
function()
{   # A function implemented by Diethelm Wuertz

    # FX Parser for Reuters Data:
    myFunction = function(series, include) {
        x = eval(parse(text = series))
        object <<- fxdata.contributors(x = x, include = include)
        print(object)
        cat("\n")
        x <<- tkSaveAs(
            data = fxdata.parser(x = x, parser.table = object),
            infoName = "parsed Data") 
        y = head(x)
        y[, 1] = as.character(y[, 1])
        print(y)
        cat("...\n")
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            include = 5),
        infoName = "FX Data Parser")            
}


.fCalendar.HighFrequencyData.3 = 
function()
{   # A function implemented by Diethelm Wuertz

    # FX Filter for Reuters Data:
    myFunction = function(parameter) {
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
    tkExecute(
        fun = myFunction,
        params = list(
            parameter = "strong"),
        infoName = "Filtered FX Data",
        tkoutput = TRUE,
        console = NULL,
        title = NULL,
        description = NULL )   
}


.fCalendar.HighFrequencyData.4 = 
function()
{   # A function implemented by Diethelm Wuertz

    # FX Format on Variable Minutes Scale:
    myFunction = function(series, digits) {
        x = eval(parse(text = series))
        fxdata.varmin(x = x, digits = digits) }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            digits = 4),
        infoName = "Variable Minutes formatted FX Data",
        tkoutput = TRUE,
        console = "print(head(object))",
        title = NULL,
        description = NULL )  
}


.fCalendar.HighFrequencyData.5 = 
function()
{   # A function implemented by Diethelm Wuertz

    # * Example data.frame: x = FX Reuters AUDUSD:
    data(audusd)
    x <<- tkSaveAs(
        data = audusd, 
        infoName = "FX Reuters AUDUSD",
        tkoutput = TRUE,
        console = "print(tail(data))",
        title = NULL,
        description = NULL )
}


.fCalendar.HighFrequencyData.6 = 
function()
{   # A function implemented by Diethelm Wuertz

    # Extract BID Prices from Reuters FX Data:
    myFunction = function(ReutersData, object2x) {
        x = eval(parse(text = ReutersData))
        object <<- list(t = x[, "XDATE"], x = x[, "BID"])
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            ReutersData = "x",
            object2x = TRUE ),
        infoName = "List of FX Prices",
        tkoutput = TRUE,
        console = "print(tail(data.frame(object)))",
        title = NULL,
        description = NULL )  
}


.fCalendar.HighFrequencyData.7 = 
function()
{   # A function implemented by Diethelm Wuertz

    # Compute log Prices from Prices:
    myFunction =  function(Prices, object2x) {
        x = eval(parse(text = Prices))
        object  <<- xts.log(xts = x) 
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            Prices = "x",
            object2x = FALSE),
        infoName = "List of Log FX Prices",
        tkoutput = TRUE,
        console = "print(tail(data.frame(object)))",
        title = NULL,
        description = NULL )   
}


.fCalendar.HighFrequencyData.8 = 
function()
{   # A function implemented by Diethelm Wuertz

    # Compute log Returns from log Prices:
    myFunction =  function(logPrices, object2x) {
        x = eval(parse(text = logPrices))
        object  <<- xts.diff(xts.log(xts = x))
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            logPrices = "x",
            object2x = TRUE),
        infoName = "List of Log FX Returns",
        tkoutput = TRUE,
        console = "print(tail(data.frame(object)))",
        title = NULL,
        description = NULL )
}


.fCalendar.HighFrequencyData.9 = 
function()
{   # A function implemented by Diethelm Wuertz

    # Cut Series - Prices|logPrices|logReturns
    myFunction =  function(series, object2x) {
        x = eval(parse(text = series))
        object  <<- xts.cut(xts = x, from.date = from, to.date = to)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            from = 19971021,
            to = 19971021,
            object2x = FALSE),
        infoName = "List of Cutted Series",
        tkoutput = TRUE,
        console = "print(tail(data.frame(object)))",
        title = NULL,
        description = NULL )
}


.fCalendar.HighFrequencyData.10 = 
function()
{   # A function implemented by Diethelm Wuertz

    # Interpolate - Prices|logPrices|logReturns
    myFunction =  function(series, delta, method, object2x) {
        x = eval(parse(text = series))
        object  <<- xts.interp(xts = x, delta = delta, method = method)
        object }
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            delta = 1,
            method = "constant",
            object2x = FALSE),
        infoName = "List of Cutted Series",
        tkoutput = TRUE,
        console = "print(tail(data.frame(object)))",
        title = NULL,
        description = NULL )
}


.fCalendar.HighFrequencyData.11 = 
function()
{   # A function implemented by Diethelm Wuertz

    # Convert List to timeSeries Object",
}


.fCalendar.HighFrequencyData.12 = 
function()
{   # A function implemented by Diethelm Wuertz

    # :
    data(fdax97m)
    x <<- tkSaveAs(
        data = as.timeSeries(fdax97m, format = "%Y%m%d%H%M"),
        infoName = "FDAX Index 1997")
}


.fCalendar.HighFrequencyData.13 = 
function()
{   # A function implemented by Diethelm Wuertz

    # De-seasonalize in Upsilon Time:
    myFunction =  function(series, from, to, delta, alpha = alpha, doplot) {
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
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            from = 19970106,
            to = 19971228,
            delta = 60,
            alpha = 1.05,
            doplot = TRUE),
        infoName = "Series & Time Map",
        tkoutput = TRUE,
        console = NULL,
        title = NULL,
        description = NULL )  
}


.fCalendar.HighFrequencyData.14 = 
function()
{   # A function implemented by Diethelm Wuertz

    # :
    myFunction = function(series, kParameter, doplot) {
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
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            kParameter = 8,
            doplot = TRUE),
        infoName = "Series & Parameters",
        tkoutput = TRUE,
        console = NULL,
        title = NULL,
        description = NULL )       
}


.fCalendar.HighFrequencyData.15 = 
function()
{   # A function implemented by Diethelm Wuertz

    # Plot Daily/Weekly Volatility Charts:
    myFunction = function(series, from, to, period, deltat, doplot) {
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
    tkExecute(
        fun = myFunction,
        params = list(
            series = "x",
            from = 19970106,
            to = 19971228,
            period = "both",
            delta = 30,
            doplot = TRUE),
        infoName = "Series & Time Map",
        tkoutput = TRUE,
        console = NULL,
        title = NULL,
        description = NULL )     
}


################################################################################

