
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
# Create a timeDate Object


.fCalendar.TimeDateClass.dates = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # * Example timeDate: x = 12 Random Dates in Current Year
    helpTopic <<- "timeCalendar"
    myFunction = function(FinCenter, object2x, report) {
        y = rep(currentYear, 12)
        d = trunc(runif(12, 1, 29))
        m = trunc(runif(12, 1, 13))
        object <<- timeCalendar(y = y, m = m, d = d, FinCenter = FinCenter)
        if (report) tkTitle("12 Random Dates")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list( 
            FinCenter = "GMT",
            object2x = TRUE,
            report = TRUE),
        subject = "12 Random Dates" )          
}


# ------------------------------------------------------------------------------


.fCalendar.TimeDateClass.times = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # * Example timeDate: x = 12 Random Times in Current Year 
    helpTopic <<- "timeCalendar"
    myFunction = function(FinCenter, object2x, report) {
        y = rep(2005, 12)
        d = trunc(runif(12, 1, 29))
        m = trunc(runif(12, 1, 13))
        h = trunc(runif(12, 0, 24))
        min = trunc(runif(12, 0, 60))
        s = trunc(runif(12, 0, 60))
        object <<- timeCalendar(y = y, m = m, d = d, h = h, 
            min = min, s = s, FinCenter = FinCenter)
        if (report) tkTitle("12 Random Times")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list( 
            FinCenter = "GMT",
            object2x = TRUE,
            report = TRUE),
        subject = "12 Random Times" )          
}


# ------------------------------------------------------------------------------


.fCalendar.TimeDateClass.timeSequence = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Create a timeDate Sequence:
    helpTopic <<- "timeSequence"
    myFunction = function(from, to, by, length.out, format, FinCenter, 
        object2x, report) {
        object <<- timeSequence(from = from, to = to, by = by, 
            length.out = eval(parse(text = length.out)), format = format, 
            FinCenter = FinCenter) 
        if (report) tkTitle("timeDate Sequence")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            from = "2005-01-01", 
            to = "2005-12-31", 
            by = "day", 
            length.out = "NULL", 
            format = "%Y-%m-%d", 
            FinCenter = "GMT",
            object2x = FALSE,
            report = TRUE),
        subject = "timeDate Sequence" )          
}


# ------------------------------------------------------------------------------


.fCalendar.TimeDateClass.timeCalendar = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Create a timeDate Calendar:
    helpTopic <<- "timeCalendar"
    myFunction = function(y, m, d, h, min, s, FinCenter, object2x, report) {
        object <<- timeCalendar(
            y = eval(parse(text = y)),
            m = eval(parse(text = m)), 
            d = eval(parse(text = d)),
            h = eval(parse(text = h)), 
            min = eval(parse(text = min)),
            s = eval(parse(text = s)), 
            FinCenter = FinCenter) 
        if (report) tkTitle("timeDate Calendar")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            y = "2005", 
            m = "1:12", 
            d = "1", 
            h = "NULL", 
            min = "NULL", 
            s = "NULL",
            FinCenter = "GMT",
            object2x = FALSE,
            report = TRUE),
        subject = "timeDate Calendar" )          
}


# ------------------------------------------------------------------------------


.fCalendar.TimeDateClass.getTime = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # The Current Date and Time:
    helpTopic <<- "Sys.timeDate"
    .tkGetTime()
}


# ------------------------------------------------------------------------------


.fCalendar.TimeDateClass.timeDayInPeriod = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # Last Day in Month for a Given Date:
    helpTopic <<- "timeFirstDayInMonth"
    myFunction = function(series, which, period, format, FinCenter, 
        object2x, report) {
        x = tkEval(series)
        which = tkSplit(which)
        period = tkSplit(period)
        if (which == "first") {
            if (period == "month") {
                object <<- timeFirstDayInMonth(charvec = as.character(x), 
                    format = format, FinCenter = FinCenter) 
                if (report) tkTitle("First Days in Month")
            } else if (period == "quarter") {
                object <<- timeFirstDayInQuarter(charvec = as.character(x), 
                    format = format, FinCenter = FinCenter) 
                if (report) tkTitle("First Days in Quarter")
            }      
        } else if (which == "last") {
            if (period == "month") {
                object <<- timeLastDayInMonth(charvec = as.character(x), 
                    format = format, FinCenter = FinCenter) 
                if (report) tkTitle("Last Days in Month")
            } else if (period == "quarter") {
                object <<- timeLastDayInQuarter(charvec = as.character(x), 
                    format = format, FinCenter = FinCenter) 
                if (report) tkTitle("Last Days in Quarter")
            }  
        }    
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            which = "last & first",
            period = "month & quarter",
            format = "%Y-%m-%d",
            FinCenter = "GMT",
            object2x = FALSE,
            report = TRUE),
        subject = "1st/Last Days in Period" )          
}


# ------------------------------------------------------------------------------


.fCalendar.TimeDateClass.timeNdayOnOrAfter = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # The N-Day On-Or-After a given Date:
    helpTopic <<- "timeNdayOnOrAfter"
    myFunction = function(series, nday, format, FinCenter, 
        object2x, report) {
        x = tkEval(series)
        object <<- timeNdayOnOrAfter(charvec = as.character(x), 
            nday = nday, format = format, FinCenter = FinCenter) 
        if (report) tkTitle("The N-Day On-Or-After")
        object } 
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            nday = 1,
            format = "%Y-%m-%d",
            FinCenter = "GMT",
            object2x = FALSE,
            report = TRUE),
        subject = "The N-Day On-Or-After" )               
}


# ------------------------------------------------------------------------------


.fCalendar.TimeDateClass.timeNdayOnOrBefore = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # The N-Day On-Or-Before a given Date:
    helpTopic <<- "timeNdayOnOrBefore"
    myFunction = function(series, nday, format, FinCenter, 
        object2x, report) {
        x = tkEval(series)
        object <<- timeNdayOnOrBefore(charvec = as.character(x), nday = nday, 
            format = format, FinCenter = FinCenter) 
        if (report) tkTitle("The N-Day On-Or-Before")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            nday = 1,
            format = "%Y-%m-%d",
            FinCenter = "GMT",
            object2x = FALSE,
            report = TRUE),
        subject = "The N-Day On-Or-Before" )              
}


# ------------------------------------------------------------------------------


.fCalendar.TimeDateClass.timeNthNdayInMonth = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # The n-th ocurrance of a n-day in year/month
    helpTopic <<- "timeNthNdayInMonth"
    myFunction = function(series, nday, nth, format, FinCenter, 
        object2x, report) {
        x = tkEval(series)
        object <<- timeNthNdayInMonth(charvec = as.character(x), 
            nday = nday, nt = nth, format = format, FinCenter = FinCenter) 
        if (report) tkTitle("The n-th Ocurrance of a n-Day") 
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            nday = 1,
            nth = 1, 
            format = "%Y-%m-%d",
            FinCenter = "GMT",
            object2x = FALSE,
            report = TRUE),
        subject = "The n-th Ocurrance of a n-Day" )              
}


# ------------------------------------------------------------------------------


.fCalendar.TimeDateClass.timeLastNdayInMonth = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # The last n-day in year/month 
    helpTopic <<- "timeLastNdayInMonth"
    myFunction = function(series, nday, format, FinCenter, 
        object2x, report) {
        x = tkEval(series)
        object <<- timeLastNdayInMonth(charvec = as.character(x), 
            nday = nday, format = format, FinCenter = FinCenter)  
        if (report) tkTitle("The Last N-Day")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            nday = 1,
            format = "%Y-%m-%d",
            FinCenter = "GMT",
            object2x = FALSE,
            report = TRUE),
        subject = "The Last N-Day" )              
}


# ------------------------------------------------------------------------------


.fCalendar.TimeDateClass.getFinCenters = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # List of Financial Centers
    helpTopic <<- "timeDate"
    .tkGetFinCenters()
}


# ------------------------------------------------------------------------------


.fCalendar.TimeDateClass.FinCenter = 
function()
{   # A function implemented by Diethelm Wuertz
    
    # DST Rules for a Financial Center:
    helpTopic <<- "timeDate"
    myFunction = function(FinCenter, object2x, report){
        FUN = match.fun(FinCenter)
        object <<- FUN()
        if (report) tkTitle("DST Rules")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            FinCenter = "Zurich",
            object2x = FALSE,
            report = TRUE),
        subject = "DST Rules" )
}                                              


################################################################################
# Manipulate a timeDate Object


.fCalendar.TimeDateMethods.start = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Extract the first object of a timeDate Vector
    helpTopic <<- "timeDate"
    myFunction = function(series, object2x, report) {
        x = tkEval(series)
        object <<- start(modify(x = x, "sort"))
        if (report) tkTitle("Earliest Date")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            object2x = FALSE,
            report = TRUE ),
        subject = "First Value" )
}


# ------------------------------------------------------------------------------


.fCalendar.TimeDateMethods.end = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Extract the last object of a timeDate Vector:
    helpTopic <<- "timeDate"
    myFunction = function(series, object2x, report) {
        x = tkEval(series)
        object <<- end(modify(x = x, "sort"))
        if (report) tkTitle("Latest Date")
        object  }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            object2x = FALSE,
            report = TRUE),
        subject = "Last Value" )
}


# ------------------------------------------------------------------------------


.fCalendar.TimeDateMethods.plusminus = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # +/- a time Span from a timeDate Vector
    helpTopic <<- "timeDate"
    myFunction = function(series, PlusMinus, days, hours, minutes, 
        seconds, object2x, report) {
        x = tkEval(series)
        ans = days*24*60*60 + hours*60*60 + minutes*60 + seconds
        if (PlusMinus == "+") object <<- x + ans
        if (PlusMinus == "-") object <<- x - ans
        if (report) tkTitle("Addition / Subtraction")
        object}
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            PlusMinus = "+",
            days = 1,
            hours = 0,
            minutes = 0,
            seconds = 0,
            object2x = FALSE,
            report = TRUE),
        subject = "+/- Time Span Object" )          
}


# ------------------------------------------------------------------------------


.fCalendar.TimeDateMethods.sort = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Sort a timeDate Vector by Time and Date:
    helpTopic <<- "timeDate"
    myFunction = function(series, object2x, report) {
        x = tkEval(series)
        object <<- modify(x = x, method = "sort") 
        if (report) tkTitle("Sort Dates")
        object}  
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            object2x = FALSE,
            report = TRUE),
        subject = "Sort Object" )          
}


# ------------------------------------------------------------------------------


.fCalendar.TimeDateMethods.round = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Round a timeDate Vector to a Given Unit:
    helpTopic <<- "timeDate"
    myFunction = function(series, units, object2x, report) {
        x = tkEval(series)  
        object <<- modify(x = x, method = "round", units = units) 
        if (report) tkTitle("Round Dates")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            units = "days",
            object2x = FALSE,
            report = TRUE),
        subject = "Round Object" )
}


# ------------------------------------------------------------------------------


.fCalendar.TimeDateMethods.trunc = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Truncate a timeDate Vector to a Given Unit:
    helpTopic <<- "timeDate"
    myFunction = function(series, units, object2x, report) { 
        x = tkEval(series)
        object <<- modify(x, method = "trunc", units = units)
        if (report) tkTitle("Truncate Dates")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            units = "days",
            object2x = FALSE,
            report = TRUE),
        subject = "Truncate Object" )
}


# ------------------------------------------------------------------------------


.fCalendar.TimeDateMethods.rev = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Return a timeDate Vector in Reverse Order:
    helpTopic <<- "timeDate"
    myFunction = function(series, object2x, report) { 
        x = tkEval(series)
        object <<- rev(x = x)
        if (report) tkTitle("Revert Dates")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            object2x = FALSE,
            report = TRUE),
        subject = "Revert Object" )
}


# ------------------------------------------------------------------------------


.fCalendar.TimeDateMethods.asCharacter = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Convert timeDate to a character vector:
    helpTopic <<- "timeDate"
    myFunction = function(series, object2x, report) { 
        x = tkEval(series)
        object <<- as.character(x = x) 
        if (report) tkTitle("timeDate Converted to Charater")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            object2x = FALSE,
            report = TRUE),
        subject = "to Character" )
}


# ------------------------------------------------------------------------------


.fCalendar.TimeDateMethods.asDataFrame = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Convert timeDate to a data frame:
    helpTopic <<- "timeDate"
    myFunction = function(series, object2x, report) { 
        x = tkEval(series)
        object <<- as.data.frame(x = x)
        if (report) tkTitle("timeDate Converted to Data Frame")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            object2x = FALSE, 
            report = TRUE),
        subject = "to Data Frame" )
}


# ------------------------------------------------------------------------------


.fCalendar.TimeDateMethods.asPOSIXct = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Convert timeDate to a POSIXct Object:
    helpTopic <<- "timeDate"
    myFunction = function(series, object2, report) { 
        x = tkEval(series)
        object <<- as.POSIXct(x = x)
        if (report) tkTitle("timeDate Converted to POSIXct")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            object2x = FALSE,
            report = TRUE),
        subject = "to POSIXct" )
}


# ------------------------------------------------------------------------------


.fCalendar.TimeDateMethods.julian = 
function() 
{   # A function implemented by Diethelm Wuertz

    # Julian Day Counts from timeDate Vector:
    helpTopic <<- "timeDate"
    myFunction = function(series, unclass, object2x, report) { 
        x = tkEval(series)
        object <<- julian(x = x)
        if (unclass) object <<- unclass(object)
        if (report) tkTitle("Julian Counts from timeDate Object")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            unclass = FALSE,
            object2x = FALSE,
            report = TRUE),
        subject = "Julian Counts" )
}


# ------------------------------------------------------------------------------


.fCalendar.TimeDateMethods.atoms = 
function() 
{   # A function implemented by Diethelm Wuertz
    
    # Extract data.frame of timeDate Atoms:
    helpTopic <<- "atoms"
    myFunction = function(series, object2x, report) { 
        x = tkEval(series)
        object <<- atoms(x = x)
        if (report) tkTitle("Extract Atoms from timeDate Object")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            object2x = FALSE,
            report = TRUE),
        subject = "Extract Atoms" )
}


################################################################################
# Compose and Modify a timeSeries Object


.fCalendar.TimeSeriesClass.timeSeries = 
function()
{   # A function implemented by Diethelm Wuertz

    # Create timeSeries Object:
    helpTopic <<- "timeSeries"
    myFunction = function(data, charvec, units, format, zone, FinCenter,
        pbject2x, record) { 
        data = eval(parse(text = data))
        charvec = as.character(eval(parse(text = charvec)))
        units = as.character(eval(parse(text = units)))
        object <<- timeSeries(data = data, charvec = charvec, 
            units = units, format = format, zone = zone , 
            FinCenter = FinCenter)
        if (report) tkTitle("timeSeries Object")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            data = "matrix(rnorm(24),ncol=2)",
            charvec = "timeCalendar(y=2006,m=1:12)",
            units = "LETTERS[1:2]",
            format = "%Y-%m-%d",
            zone = "GMT",
            FinCenter = "GMT",
            object2x = TRUE,
            report = TRUE),
        subject = "Create a timSeries Object" )
}


# ------------------------------------------------------------------------------


.fCalendar.TimeSeriesClass.applySeries = 
function()
{   # A function implemented by Diethelm Wuertz

    # Apply a Function to Series:
    helpTopic <<- "applySeries"
    myFunction = function(series, from, to, by, FUN, object2x, report) {
        x = tkEval(series)
        by = tkSplit(by)
        if (from == "NULL") from = NULL
        if (to == "NULL") to = NULL
        fun = match.fun(FUN)
        object <<- applySeries(x = x, from = from, to = to, by = by, 
            FUN = fun, units = NULL) 
        if (report) {
	        tkTitle("Apply Function to timeSeries Object")
	        tkOutput(paste("Function:\n", FUN, "\n"))
        }
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            from = "NULL",
            to = "NULL",
            by = "monthly & quarterly",
            FUN = "colAvgs",
            object2x = FALSE,
            report = TRUE),
        subject = "Apply FUN to timeSeries" )
}


# ------------------------------------------------------------------------------


.fCalendar.TimeSeriesClass.alignDailySeries = 
function()
{   # A function implemented by Diethelm Wuertz

    # Align Daily Series:
    helpTopic <<- "alignDailySeries"
    myFunction = function(series, method, include.weekends, 
        object2x, report) { 
        x = tkEval(series)
        method = tkSplit(method)
        ans = alignDailySeries(x = x, method = method, 
            include.weekends = include.weekends) 
        # ans@FinCenter = x@FinCenter 
        object <<- ans
        if (report) tkTitle("Align Daily Series")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            method = "fillNA & before & after & interp",
            include.weekends = TRUE,
            object2x = FALSE,
            report = TRUE ),
        subject = "Align Daily Series")
}


# ------------------------------------------------------------------------------


.fCalendar.TimeSeriesClass.cutSeries = 
function()
{   # A function implemented by Diethelm Wuertz

    # Cut Series:
    helpTopic <<- "cutSeries"   
    myFunction = function(series, from, to, object2x, report) {
        x = tkEval(series)
        object <<- cutSeries(x = x, from = from, to = to) 
        if (report) tkTitle("Cut a timeSeries")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            from = "2005-03-01",
            to = "2005-04-31",
            object2x = TRUE,
            report = TRUE),
        subject = "Cut a timeSeries" )
}


# ------------------------------------------------------------------------------


.fCalendar.TimeSeriesClass.diffSeries = 
function()
{   # A function implemented by Diethelm Wuertz

    # Difference Series:
    helpTopic <<- "diffSeries"
    myFunction = function(series, lag, diff, trim, pa, object2x, report) {
        x = tkEval(series)
        object <<- diffSeries(x = x, lag = lag, diff = diff, 
            trim = trim, pad = pad) 
        if (report) tkTitle("Difference a timeSeries")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            lag = 1, 
            diff = 1, 
            trim = FALSE, 
            pad = NA,
            object2x = FALSE, 
            report = TRUE ),
        subject = "Difference a timeSeries" )
}


# ------------------------------------------------------------------------------


.fCalendar.TimeSeriesClass.lagSeries = 
function()
{   # A function implemented by Diethelm Wuertz

    # Lag Series:
    helpTopic <<- "lagSeries"
    myFunction = function(series, k, trim, object2x, report) {
        x = tkEval(series)
        object <<- lagSeries(x = x, k = k, trim = trim, units = NULL) 
        if (report) tkTitle("Lag a timeSeries")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            k = 1, 
            trim = FALSE,
            object2x = FALSE,
            report = TRUE),
        subject = "Lag a timeSeries" )
}


# ------------------------------------------------------------------------------


.fCalendar.TimeSeriesClass.mergeSeries = 
function()
{   # A function implemented by Diethelm Wuertz

    # Merge timeSeries Objects:
    helpTopic <<- "mergeSeries"
    myFunction = function(series, matrix, object2x, report) { 
        x = tkEval(series)
        data = eval(parse(text = matrix))
        object <<- mergeSeries(x = x, y = data, units = NULL)
        if (report) tkTitle("Merge Series with Matrix")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            matrix = "object@Data",
            object2x = FALSE,
            report = TRUE),
        subject = "Merge Series with Matrix" )
}


# ------------------------------------------------------------------------------


.fCalendar.TimeSeriesClass.returnSeries = 
function()
{   # A function implemented by Diethelm Wuertz

    # Get Return Series:
    helpTopic <<- "returnSeries"
    myFunction = function(series, type, percentage, trim, digits, 
        object2x, report) { 
        x = tkEval(series)
        type = tkSplit(type)
        object <<- returnSeries(x = x, type = type, percentage = 
            percentage, trim = trim, digits = digits, units = NULL) 
        if (report) tkTitle("Compute Return Series")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            type = "continuous & discrete",
            percentage = FALSE, 
            trim = FALSE,
            digits = 4,
            object2x = FALSE,
            report = TRUE),
        subject =  "Return Series" )
}


# ------------------------------------------------------------------------------


.fCalendar.TimeSeriesClass.revSeries = 
function()
{   # A function implemented by Diethelm Wuertz

    # Revert Series:
    helpTopic <<- "revSeries"
    myFunction = function(series, object2x, report) { 
        x = tkEval(series)
        object <<- revSeries(x = x) 
        if (report) tkTitle("Revert timeSeries")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            object2x = FALSE,
            report = TRUE),
        subject = "Revert timeSeries" )
}


################################################################################
# Holiday Calendars


.fCalendar.Holidays.easterDate = 
function()
{   # A function implemented by Diethelm Wuertz

    # Date of Easter & Related Dates:
    helpTopic <<- "easter"
    myFunction = function(year, shift, object2x, report) {
        year = eval(parse(text = year)) 
        object <<- timeDate(easter(year = year, shift = shift))
        if (report) tkTitle("Holiday Date(s)")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            year = as.character(currentYear),
            shift = 0,
            object2x = FALSE,
            report = TRUE),
        subject = "Holiday Date(s)" )
}


# ------------------------------------------------------------------------------


.fCalendar.Holidays.holidayList = 
function()
{   # A function implemented by Diethelm Wuertz

    # List of Supported Holidays:  
    helpTopic <<- "easter"
    myFunction = function(object2x, report) {
        object <<- .holidayList()
        if (report) tkTitle("Holiday List")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            object2x = FALSE,
            report = TRUE),
        subject = "Holiday List" )
}


# ------------------------------------------------------------------------------


.fCalendar.Holidays.holidayDate = 
function()
{   # A function implemented by Diethelm Wuertz

    # Return Holiday Date:
    helpTopic <<- "holiday"
    myFunction = function(year, Holiday, object2x, report) {
        year = eval(parse(text = year))
        object <<- holiday(year, Holiday)
        if (report) tkTitle("Holiday Date(s)")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            year = as.character(currentYear),
            Holiday = "Easter",
            object2x = FALSE,
            report = TRUE ),
        subject = "Holiday Date(s)" )
}


# ------------------------------------------------------------------------------


.fCalendar.Holidays.nyseHolidays = 
function()
{   # A function implemented by Diethelm Wuertz

    # NYSE Holiday Calendar:
    helpTopic <<- "holiday"
    myFunction = function(fromYear, toYear, object2x, report) {
        years = seq(fromYear, toYear, by = sign(toYear-fromYear))
        object <<- holiday.NYSE(years) 
        if (report) tkTitle("NYSE Holiday Dates")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            fromYear = currentYear,
            toYear = currentYear,
            object2x = FALSE,
            report = TRUE),
        subject = "NYSE Holiday Dates" )
}


################################################################################
# High Frequency Data Tools -- OLD DON't USE NOW !!


.fCalendar.HighFrequencyData.2 = 
function()
{   # A function implemented by Diethelm Wuertz

    # FX Parser for Reuters Data:
    helpTopic <<- ""
    myFunction = function(series, include) {
        x = tkEval(series)
        object <<- fxdata.contributors(x = x, include = include)
        print(object)
        cat("\n")
        x <<- tkSaveAsX(data = fxdata.parser(x = x, parser.table = object),
            subject = "parsed Data") 
        y = head(x)
        y[, 1] = as.character(y[, 1])
        print(y)
        cat("...\n")
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            include = 5),
        subject = "FX Data Parser")            
}


# ------------------------------------------------------------------------------


.fCalendar.HighFrequencyData.3 = 
function()
{   # A function implemented by Diethelm Wuertz

    # FX Filter for Reuters Data:
    helpTopic <<- ""
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
        prototypes = list(
            parameter = "strong"),
        subject = "Filtered FX Data" )   
}


# ------------------------------------------------------------------------------


.fCalendar.HighFrequencyData.4 = 
function()
{   # A function implemented by Diethelm Wuertz

    # FX Format on Variable Minutes Scale:
    helpTopic <<- ""
    myFunction = function(series, digits) {
        x = tkEval(series)
        fxdata.varmin(x = x, digits = digits) }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            digits = 4),
        subject = "Variable Minutes formatted FX Data" )  
}


# ------------------------------------------------------------------------------


.fCalendar.HighFrequencyData.5 = 
function()
{   # A function implemented by Diethelm Wuertz

    # * Example data.frame: x = FX Reuters AUDUSD:
    helpTopic <<- ""
    data(audusd)
    x <<- tkSaveAsX(data = audusd, subject = "FX Reuters AUDUSD")
}


# ------------------------------------------------------------------------------


.fCalendar.HighFrequencyData.6 = 
function()
{   # A function implemented by Diethelm Wuertz

    # Extract BID Prices from Reuters FX Data:
    helpTopic <<- ""
    myFunction = function(ReutersData, object2x) {
        x = eval(parse(text = ReutersData))
        object <<- list(t = x[, "XDATE"], x = x[, "BID"])
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            ReutersData = "x",
            object2x = TRUE ),
        subject = "List of FX Prices" )  
}


# ------------------------------------------------------------------------------


.fCalendar.HighFrequencyData.7 = 
function()
{   # A function implemented by Diethelm Wuertz

    # Compute log Prices from Prices:
    helpTopic <<- ""
    myFunction =  function(Prices, object2x) {
        x = eval(parse(text = Prices))
        object  <<- xts.log(xts = x) 
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            Prices = "x",
            object2x = FALSE),
        subject = "List of Log FX Prices" )   
}


# ------------------------------------------------------------------------------


.fCalendar.HighFrequencyData.8 = 
function()
{   # A function implemented by Diethelm Wuertz

    # Compute log Returns from log Prices:
    helpTopic <<- ""
    myFunction =  function(logPrices, object2x) {
        x = eval(parse(text = logPrices))
        object  <<- xts.diff(xts.log(xts = x))
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            logPrices = "x",
            object2x = TRUE),
        subject = "List of Log FX Returns" )
}


# ------------------------------------------------------------------------------


.fCalendar.HighFrequencyData.9 = 
function()
{   # A function implemented by Diethelm Wuertz

    # Cut Series - Prices|logPrices|logReturns
    helpTopic <<- ""
    myFunction =  function(series, object2x) {
        x = tkEval(series)
        object  <<- xts.cut(xts = x, from.date = from, to.date = to)
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            from = 19971021,
            to = 19971021,
            object2x = FALSE),
        subject = "List of Cutted Series" )
}


# ------------------------------------------------------------------------------


.fCalendar.HighFrequencyData.10 = 
function()
{   # A function implemented by Diethelm Wuertz

    # Interpolate - Prices|logPrices|logReturns
    helpTopic <<- ""
    myFunction =  function(series, delta, method, object2x) {
        x = tkEval(series)
        object  <<- xts.interp(xts = x, delta = delta, method = method)
        object }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            delta = 1,
            method = "constant",
            object2x = FALSE),
        subject = "List of Cutted Series" )
}


# ------------------------------------------------------------------------------


.fCalendar.HighFrequencyData.11 = 
function()
{   # A function implemented by Diethelm Wuertz

    # Convert List to timeSeries Object",
    helpTopic <<- ""
    
}


# ------------------------------------------------------------------------------


.fCalendar.HighFrequencyData.12 = 
function()
{   # A function implemented by Diethelm Wuertz

    # :
    helpTopic <<- ""
    data(fdax97m)
    x <<- tkSaveAsX(data = as.timeSeries(fdax97m, format = "%Y%m%d%H%M"),
        subject = "FDAX Index 1997")
}


# ------------------------------------------------------------------------------


.fCalendar.HighFrequencyData.13 = 
function()
{   # A function implemented by Diethelm Wuertz

    # De-seasonalize in Upsilon Time:
    helpTopic <<- ""
    myFunction = function(series, from, to, delta, alpha = alpha, doplot) {
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
        prototypes = list(
            series = "x",
            from = 19970106,
            to = 19971228,
            delta = 60,
            alpha = 1.05,
            doplot = TRUE),
        subject = "Series & Time Map" )  
}


# ------------------------------------------------------------------------------


.fCalendar.HighFrequencyData.14 = 
function()
{   # A function implemented by Diethelm Wuertz

    # :
    helpTopic <<- ""
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
        list(data = series, prototypes = c(kParameter = kParameter,
            AverageVolatility = AverageVolatility))
    }
    tkExecute(
        fun = myFunction,
        prototypes = list(
            series = "x",
            kParameter = 8,
            doplot = TRUE),
        subject = "Series & Parameters" )       
}


# ------------------------------------------------------------------------------


.fCalendar.HighFrequencyData.15 = 
function()
{   # A function implemented by Diethelm Wuertz

    # Plot Daily/Weekly Volatility Charts:
    helpTopic <<- ""
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
        prototypes = list(
            series = "x",
            from = 19970106,
            to = 19971228,
            period = "both",
            delta = 30,
            doplot = TRUE),
        subject = "Series & Time Map" )     
}


################################################################################

