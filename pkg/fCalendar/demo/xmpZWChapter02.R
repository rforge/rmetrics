
#
# Example:
#   A Compendium for R and Rmetrics users to the book 
#     "Modeling Financial Time Series with S-Plus" 
#     written by E. Zivot and J. Wang
#   ISBN 0-387-95549-6
#
# Details:
#   Examples from Chapter 2
#
# Notes:
#   This is not a COPY of the S-Plus "example.ssc" files accompanying the
#     book of Zivot and Wang. It is worth to note that this file contents a 
#     new implementation of the examples tailored to Rmetrics based on R.
#   Diethelm Wuertz
#     www.rmetrics.org
#     www.itp.phys.ethz.ch
#     www.finance.ch
#
# Author:
#   (C) 2002-2004, Diethelm Wuertz, GPL
#

    
# ------------------------------------------------------------------------------
# Chapter 2 - Time Series Specification, Maipulation and Visualization

    # IMPORTANT TO KNOW !!!
    #   To play through the examples in Chapter 2 of the book of E. Zivot
    #   and J. Wang, we need the data used in the example. All the datasets
    #   are public available.
    #   The data were downloaded from the Internet or obtained from other
    #   sources and stored in MS Excel CSV spreadsheet files. The first row 
    #   (the header) holds the "units" to be loaded to the "@units" slot 
    #   of the "timeSeries" object, and the first column holds the date/time 
    #   character vectors from which the "@positions" slot will be created. 
    #   The element of the upper left cell in the spreadsheet files holds 
    #   the unquoted POSIX format specification.    
    #   Note, that the information stored in these files, format, data
    #   and its precision, differ from that used together with the S-Plus 
    #   software in the book of Zivot and Wang.
    #   Internet download functions are available in the Rmetrics
    #   package "fCalendar". You can use them to update the data files.
    # Now we show how to work out the same examples using "R" and Rmetrics".
    #   I is worth to briefly mention the basic concepts of the 'timeDate'  
    #   and 'timeSeries' classes implemented in Rmetrics:
    #   1. a consequent usage of the ISO-8601 format specifiacation standard,
    #   2. all times and dates are handled internally as POSIXt GMT objects
    #   3. using a 'FinCenter' where the time/Date and other objects are used
    ###
 
    
# ------------------------------------------------------------------------------
# Section 2.2 - The Specification of 'timeSeries' Objects


    # Chapter 2.2 deals with the Specification of 'timeSeries' Objects.
    #   Rmetrics works internally with POSIX objects free of a time zone
    #   attribute. Time zone and daylight saving time rules are taken
    #   from the Ical database, this allows the definition of Financial
    #   Centers.
    # When using the book of E. Zivot and J. Wang be specific about time 
    #   the zone specification used. All examples in the book were created 
    #   on a windows computer with "Pacific" coast as the default time 
    #   zone. 
    # These examples were created in Zurich, und use the "Rmetrics" 
    #   functions. The 'timeDate' classes can be found in the file 
    #   "timeDateClasses.R", the methods in the file "timeDateClasses.R", 
    #    and the 'timeSeries' classes in the file "timeSeriesClasses.R".
    #   Note, the data slot for 'timeDate' and 'timeSeries' objects is 
    #   named here "@Data" and not "@.Data" what is used by S-Plus!
    ###
     
    
    # The function 'readSeries' allows you to read data from a
    # spreadsheet file and transforms it ditrectly to a 'timeSeries'
    # object.
    args(readSeries)
    ###
    
    
    # The file "singleIndex.dat.csv" contains the monthly closing 
    # prices for Microsoft Corporation and the S&P 500 index.
    # Download from Rmetrics Data Base:
    URL = "http://www.itp.phys.ethz.ch/econophysics/R/data/textbooks/"
    SRC = "ZivotWang/data/singleIndex.dat.csv"
    DATA = paste(URL, SRC, sep = "") 
    download.file(DATA, destfile = "singleIndex.dat.csv")
    singleIndex.dat = readSeries("singleIndex.dat.csv")
    print(singleIndex.dat[1,])
    print(end(singleIndex.dat))
    ###
    
    
    # Investigate monthly data for MSFT and SP500:
    # p. 16
    class(singleIndex.dat)
    # Slots:
    slotNames(singleIndex.dat)
    singleIndex.dat@Data[1:5,]
    singleIndex.dat@units
    singleIndex.dat@positions[1:5]
    singleIndex.dat@FinCenter
    singleIndex.dat@title
    singleIndex.dat@documentation
    # Functions:
    # Note: for positions() use: seriesPositions() to access
    #   the positions slot - the same notation as seriesData()
    print(seriesPositions(singleIndex.dat)[1:5])
    class(seriesPositions(singleIndex.dat))
    print(start(singleIndex.dat))
    print(end(singleIndex.dat))
    class(end(singleIndex.dat))
    print(seriesData(singleIndex.dat)[1:5,])
    class(seriesData(singleIndex.dat))
    ###


# ------------------------------------------------------------------------------
# Section 2.2.1 - Basic Manipulations:
    

    # Extract Microsoft series from a bivariate 'timeSeries' object:
    # p. 18
    msft.p = singleIndex.dat[, "MSFT"]
    msft.p = singleIndex.dat[, 1]
    msft.p@title = "Monthly closing price on Microsoft"
    msft.p@documentation = c(
        "Monthly closing price adjusted for stock",
        "splits and dividends.")
    msft.p@units = "US dollar price"
    class(msft.p)
    ###
    
    
    # Extract a subsample from a 'timeSeries' object:
    # p. 18
    smpl = (
        seriesPositions(singleIndex.dat) >= timeDate("3/01/1992") &
        seriesPositions(singleIndex.dat) <= timeDate("1/31/1993") )
    print(singleIndex.dat[smpl, ])
    class(singleIndex.dat)
    # Alternatively use in R cutSeries():
    tS = cut(singleIndex.dat, timeDate("3/01/1992"), timeDate("1/31/1993"))
    print(tS)
    # Or with ISO dates ...
    tS = cut(singleIndex.dat, timeDate("1992-03-01"), timeDate("1993-01-31"))
    print(tS)
    # Note, American and ISO format specifications are automatically recognized
    ###
    
    
    # Extract data matrix from 'timeSeries' object:
    # p. 19
    singleIndex.ts = seriesData(singleIndex.dat)
    # Useful when functions have no methods for 'timeSeries' objects ...
    colMeans(singleIndex.ts)
    ###


# ------------------------------------------------------------------------------
# Section 2.2.2 - Create 'timeDate' Objects

    
    # "Rmetrics" uses the information from the Ical data files. Time  
    #   zones and financial centers are defined by strings of the form 
    #   "Continent/City". You can add easily others like "America/Eastern" 
    #   which maps on "America/NewYork" or like "America/Pacific" which
    #   maps on "America/LosAngeles"   
    # Eric Zivot lives in Seattle, so for the "zone" and "Fincenter"
    #   use "America/Pacific" or "America/LosAngeles"  
    
    
    # Create "timeDate" objects
    # p. 19
    tdLA = timeDate(
        charvec = "1/1/2002", 
        format = "%m/%d/%Y", 
        zone = "America/Pacific",
        FinCenter = "America/LosAngeles")
    print(tdLA)
    # Now for "GMT":
    tdGMT = timeDate(
        charvec = "1/1/2002", 
        format = "%m/%d/%Y", 
        zone = "GMT",
        FinCenter = "GMT")
    print(tdGMT)
    ###
    
    
    # "Rmetrics uses no default "time.in.format". The internal default
    #   is "GMT" and the FinCenter can be selected as one from the Ical
    #   database. The local FinCenter is stored in the variable 
    #   "myFinCenter". Examples for common data formats are not provided.
    ###
    
    
    # Print 'timeDate' Class and Slot Names:
    # p. 20
    td = tdGMT
    class(td)
    print(td)
    slotNames(td)
    ###
    
    
    # Get Help:
    # p. 20
    # > ?class.timeDate
    # in Rmetrics use:
    # ... please uncomment the next two lines
    # help(timeDate)
    # help(timeSeries)
    ###
    
    
    # Inspect the Data Slot and compare its properties with S-Plus:
    # p. 20
    unclass(td@Data)
    class(td@Data)
    # ... this is a 'POSIXlt' object
    # to get Julians [in seconds], use
    unclass(as.POSIXct(td))
    # ... or
    args(julian.timeDate)
    julian(td)
    julian(td, myUnits = "days")
    # Yields: Time difference of 11688.33 days
    # From where comes 1/3 day?
    # Julians have their origin 1960-01-01 00:00:00 GMT!
    # Confirm it:
    julian(tdGMT, myUnits = "days")
    # Test the Origin:
    tdORIGIN = timeDate(
        charvec = "1970-01-01", 
        format = "%Y-%m-%d",
        FinCenter = "GMT")
    tdORIGIN
    julian(tdORIGIN, myUnits = "days")
    ###
    
    
    # Print the Format Slot:
    # p. 20
    td
    td@format
    ### format error 
    
    
    ### TIME ZONE ISSUES ###
    
    
    # Print the FinCenter Slot:
    # p. 21
    # > td@time.zone 
    # ... use:
    tdLA@FinCenter
    tdGMT@FinCenter
    # The local FinCenter is:
    myFinCenter
    ###
    
    
    # Format date/time in Pacific Time Zone:
    # tdPACIFIC = timeDate("Mar 02, 1963 08:00 PM",
    #    format = "%b %d, %Y %H:%M %p",
    #     zone = "America/Pacific",
    #     FinCenter = "America/Pacific" )
    # ... makes some problems - please  check source code
    # Workaround:
    tdPACIFIC = timeDate(
        strptime("Mar 02, 1963 08:00 PM", format = "%b %d, %Y %H:%M %p"),
        zone = "America/Pacific",
        FinCenter = "America/Pacific" )
    tdPACIFIC
    ###
    
    # Convert date/time to Eastern Time Zone:
    # p. 21
    # To use the DST rules for Eastern Time use either FinCenter 
    # "America/Eastern" or "America/NewYork", they are the same. 
    tdPST = timeDate(
        "2002-01-01", 
        format = "%Y-%m-%d",
        zone = "America/Pacific", 
        FinCenter = "America/Pacific" )
    tD = timeDate(
        tdPST, 
        zone = "America/Pacific", 
        FinCenter = "America/NewYork" )
    print(tD)
    tD = timeDate(
        tdPST, 
        zone = "America/Pacific", 
        FinCenter = "America/Eastern" )
    print(tD)
    ###
    
    
    # Format date/time as recorded in "GMT" and used as "GMT"
    # Rmetrics
    tdGMT = timeDate(
        "1/1/2002", 
        format = "%m/%d/%Y",
        zone = "GMT",
        FinCenter = "GMT")
    print(tdGMT)
    ###
    
    
    # Format date/time as recorded in "GMT" and used in "America/Pacific"
    # Rmetrics
    tdPST = timeDate(
        tdGMT, 
        FinCenter = "America/Pacific")
    print(tdPST)
    ###
    
    
    # Format date/time as recorded in "PST" and used in "PST"
    # Rmetrics
    tdPST = timeDate(
        "1/1/2002", 
        format = "%m/%d/%Y",
        zone = "America/Pacific",
        FinCenter = "America/Pacific")
    print(tdPST)
    ###
    
    
    # Format date/time as recorded in "PST" and used in "GMT"
    # Rmetrics
    tdGMT = timeDate(
        tdPST, 
        zone = "America/Pacific", 
        FinCenter = "GMT")
    print(tdGMT)
    ###
    
    
    # Use FinCenter "Europe/Zurich" at home in Zurich:
    # Rmetrics
    print(myFinCenter)
    tdZUR = timeDate(
        tdPST, 
        zone = "America/Pacific", 
        FinCenter = "Europe/Zurich")
    print(tdZUR)
    ###
    

    ### MATHEMATICAL OPERATIONS ON 'timeDate' OBJECTS ###
    

    # Create 'timeDate' objects from Scratch:
    # p. 23
    # 1st of January 2002 ... 
    td1 = timeDate(
        "2002-01-01", 
        format = "%Y-%m-%d",
        zone = "GMT",
        FinCenter = "GMT")
    # and 1st of February 2002 ...
    td2 = timeDate(
        "2002-02-01", 
        format = "%Y-%m-%d",
        zone = "GMT",
        FinCenter = "GMT")
    print(td1)
    print(td2)
    # Or shorthand ...
    tD = timeDate(c("2002-01-01", "2002-02-01"), FinCenter = "GMT") 
    print(tD)
    ###
    
    
    # Convert 'timeDate' objects to Julian Counts:
    # p. 23
    # > as.numeric(td1)
    # ... use julian, that's more definite!
    print(td1)
    tJ = julian(td1, myUnits = "days")
    print(tJ)
    # Add one day -  we use seconds!
    # ... these are 24 hours each with 3600 seconds
    tD = td1 + 24*3600
    print(tD)
    # Add half a day      
    tD = td1 + 12*3600
    print(tD)
    # One day earlier:
    tD = td1 - 24*3600
    print(tD)
    # > 2 * td1
    # > td1 + td2
    # ... not supported, multiplicating and adding dates 
    # makes to our opinion no real sense.
    ###
    

    # Compute Differences - 'timeSpan' Objects:
    # p. 23
    td.diff = td2 - td1
    print(td.diff)
    class(td.diff)
    # > slotNames(td.diff)
    # ... it's a S3 method, therefore we have no slot names.
    ###
    
    
# ------------------------------------------------------------------------------
# Section 2.2.3 - Creating Common 'timeDate' Sequences


    ### ANNUAL SEQUENCES ###
    

    # Create annual sequence using 'timeCalendar' function:
    # p. 24
    args(timeCalendar)
    td = timeCalendar(
        y = 1900:1910,
        m = NULL)
    class(td)[1]
    print(td)
    tJ = julian(td, myUnits = "days")
    print(tJ)
    ###

    
    # Create annual sequence using 'timeSequence' function:
    # p. 24
    args(timeSequence)
    td = timeSequence(
        from = "1/1/1900", 
        to = "1/1/1910", 
        by = "year", 
        format = "%m/%d/%Y")
    class(td)[1]
    print(td)
    ###
    
    
    ### QUARTERLY SEQUENCES ###
    

    # Create quarterly sequence using 'timeSequence' function:
    # p. 25
    tS = timeSequence(
        from = "1/1/1900", 
        to = "10/1/1902", 
        by = "quarter",
        format = "%m/%d/%Y")
    print(tS)
    # Rmetrics prefers ISO-8601 Input:
    # ... jump to the next century
    tS = timeSequence(from = "2000-01-01", to = "2002-10-01", 
        by = "quarter", format = "%Y-%m-%d")
    print(tS)
    tS = timeSequence(from = "20000101", to = "20021001", 
        by = "quarter", format = "%Y%m%d")
    print(tS)
    tS = timeSequence(from = "2000-01-01 16:15:00", to = "2002-10-01 16:15:00", 
        by = "quarter", format = "%Y-%m-%d %H:%M:%S")
    print(tS)
    ###
    
    
    ### MONTHLY SEQUENCES ###
    
    
    # Create monthly sequences using "timeCalendar" function:   
    # p. 25
    # January 1, 1900 to March 1, 1901  
    td = timeCalendar(
        y = rep(1900:1901, each = 12, length = 15),
        m = rep(1:12, length = 15) )
    print(td)
    td@FinCenter
    tD = timeDate(td, FinCenter = "GMT")
    print(tD)
    tD = timeDate(td, zone = td@FinCenter, FinCenter = "GMT")
    print(tD)
    ###
    
        
    # ... another monthly sequence using "timeSequence"
    # Create Dates with First Day in Month:
    tS = timeSequence(
        from = "1/1/1900",
        to = "3/1/1901",
        by = "month",
        format = "%m/%d/%Y")
    print(tS)
    ###
    
    
    # ... another monthly sequences using "timeSequence"
    # Create Dates with the Last Day in [previous] month
    tS = timeSequence(
        from = "1/1/1900", 
        to = "3/1/1901",
        by = "month",
        format = "%m/%d/%Y") - 24*3600
    print(tS)
    ###
    
    
    # ... another monthly sequences using "timeSequence"
    # Rmetrics
    # There are some fancy R addons:
    #  timeLastDayInMonth   the last day in a given month and year
    #  timeNdayOnOrAfter    date in month that is a n-day ON OR AFTER date
    #  timeNdayOnOrBefore   date in month that is a n-day ON OR BEFORE date
    #  timeNthNdayInMonth   n-th ocurrance of a n-day in year/month
    #  timeLastNdayInMonth  the last n-day in year/month
    # Create dates with the first day in month ...
    tD = timeSequence(from = "1900-01-01", to = "1901-03-01", 
        by = "month", format = "%Y-%m-%d")
    print(tD)
    # ... now look for the first Monday in Month:
    # Nore, "n"-day is a "Mon"-day, nth=1 for the 1st occurrence.
    tD = timeNthNdayInMonth(charvec = as.character(td), nday = 1, nth = 1)
    print(tD)
    ###
    
        
    ### WEEKLY SEQUENCES ###
    
    
    # Create weekly sequences using "timeSequence" function
    # p. 26
    tS = timeSequence(
        from = "1/1/1990", 
        to = "3/1/1990", 
        by = "week",
        format = "%m/%d/%Y")
    print(tS)
    ###
    
    
    # ... another weekly sequence using "timeSequence"
    tD = timeSequence("2000-01-01", "2000-12-31", 
        by = "week", format = "%Y-%m-%d")
    print(tD)
    # Starts on?
    getDayOfWeek(tD[1])
    # Returns 6, a Saturday
    # ... start on Monday
    tD = tD + 24*3600
    print(tD)
    ###
        
    
    ### DAILY SEQUENCES ###
    
    
    # Create daily sequences using "timeSequence" function:
    # p. 26
    # ... for January 2004
    tS = timeSequence("2004-01-01", "2004-01-31", format = "%Y-%m-%d")
    print(tS)
    # ... use "timeCalendar"
    tS = timeCalendar(2004, 1, 1:31)
    print(tS)
    # ... use "timeDate
    tD = timeDate(c(
        paste("2004-01-0", 1:9, sep = ""), 
        paste("2004-01-", 10:31, sep = "")), zone = myFinCenter)
    print(tD)
    ###
    
    
    # Exclude weekends using "is.weekday" and "is.weekend" functions:
    # Rmetrics
    #   In R there are no functions for is.weekday() and
    #   is.weekend(), let us add them:
    is.weekday = function(x) {  
        wday = (x@Data)$wday
        return(!(wday == 0 | wday == 6)) }
    is.weekend = function(x) {
        return(!is.weekday(x)) }
    # ... January 2004
    tD = timeCalendar(2004, 1, 1:31)
    print(tD)
    # ... exclude weekends, i.e. Saturdays and Sundays
    print(tD[isWeekday(tD)])
    # ... list weekends
    print(tD[isWeekend(tD)])
    ###
    
        
    # Print - New York Stock Exchange - Holiday Calendar for 2000
    # p. 26
    NYSE2000 = holidayNYSE(2000)
    print(NYSE2000)
    NYSE2000@FinCenter = "America/NewYork"
    ###
 

    # Extract Business Days in January 2000 - New York
    # Note, the 'timeSequence' function cannot exclude directly holidays,
    # therefore we dot it in a 2 step approach using the function 'is.bizday'
    # Create Sequence ...
    tD = timeSequence(
        from = "2000-01-01", 
        to = "2000-01-31",
        by = "day",
        format = "%Y-%m-%d", 
        FinCenter = "America/NewYork")
    print(tD)
    # Remove NYSE Holidays ...
    tD = tD[isBizday(tD, NYSE2000)]
    print(tD)
    ###
    

    ### INTRA-DAY IRREGULARLY SPACED SEQUENCES ###
    
    
    # Create a sequence of hourly observations from 9 AM to 3 PM 
    # p. 27
    # What is the current FinCenter?
    print(myFinCenter)
    # Create 'timeDate' Object:
    # ... on Jan 3 and Jan 4, 2000
    tD = timeCalendar(
        y = 2000, 
        m = 1,
        d = rep(3:4, each = 7), 
        h = rep(9:15, 2) )
    print(tD) 
    # Where we are ?
    print(tD@FinCenter)
    # Create the same for New York ...
    tdNY = timeCalendar(
        y = 2000, 
        m = 1,
        d = rep(3:4, each = 7), 
        h = rep(9:15, 2), 
        min = rep(0, times = 14),
        s = rep(0, times = 14),
        FinCenter = "America/NewYork" )
    print(tdNY) 
    
    y = 2000
    m = 1
    d = rep(3:4, each = 7)
    h = rep(9:15, 2)
    min = rep(0, times = 14)
    s = rep(0, times = 14)
    FinCenter = "America/NewYork"
    
    # What time was it in Zurich ?
    tdZUR = timeDate(tdNY, zone = tdNY@FinCenter)
    print(tdZUR)
    ###
    
        
    # ... another sequence of minute observations from 9 AM to 3 PM
    # on Jan 3 and Jan 4, 2000, 6 hours per day
    # One day ...
    min = rep(0:59, 6)
    hrs = rep(9:14, each = 60) 
    # Two days ...
    # Create it for Los Angeles:
    tdLA = timeCalendar(
        y = 2000,
        m = rep(1, 720),
        d = rep(3:4, each = 360),
        h = c(hrs, hrs),
        min = c(min, min),
        FinCenter = "America/LosAngeles")
    print(tdLA[c(1:5, 361:365)])
    # Confirm Financial Center:
    print(tdLA@FinCenter)
    ###
 

# ------------------------------------------------------------------------------
# Section 2.2.4 - Miscellaneous Time and Date Functions

    
    # Use miscellaneous utility functions:
    # p. 28
    # Return Julian Counts as 'difftime' in Seconds:
    print(tdLA[1:10])
    print(julian(tdLA)[1:10])
    # Return Julian Counts as 'difftime' in Days:
    print(julian(tdLA, "days")[1:10])
    # Return 'timeDate' s integer counts:
    as.integer(julian(tdLA))[1:10]
    ###
    
    
    # Extract 'timeDate' atoms:
    # Rmetrics
    print(atoms(tdLA)[1:10, ])
    ###
 
       
# ------------------------------------------------------------------------------
# Section 2.2.5 - Creating 'timeSeries' Objects


    # The function 'readSeries' allows you to read data from a
    # spreadsheet file and transforms it directly to a 'timeSeries' object.
    args(readSeries)
    ###


    # Create a "timeSeries" object from Normal Random Data:
    # p. 28
    # Create data frame ...
    my.df = data.frame(
        x = abs(rnorm(10, mean = 5)),
        y = abs(rnorm(10, mean = 10)) )
    my.df
    my.td = timeCalendar(y = 1990:1999, m = 12, d = 28)
    my.td
    # Create time series ...
    my.ts = timeSeries(
        data = my.df, 
        charvec = my.td@Data, 
        units = colnames(my.df),
        FinCenter = "GMT")
    print(my.ts)
    # Change Position Names ...
    rownames(my.ts@Data) = substr(rownames(my.ts@Data), 1, 4)
    print(my.ts)
    ###
    
    
    # Write a summary 'timeSeries' objects:
    # Rmetrics:
    #   The summary method prints out the information obtained in 
    #   a 'timeSeries' objects:
    getClass("timeSeries")
    # Function:
    summary.timeSeries = function(x) {
        # Series Name:
        cat("\nTime Series:        ")
        cat("\n Name:              ", substitute(x))    
        # Data Matrix:
        Dim = dim(x@Data)
        cat("\nData Matrix:        ")
        cat("\n Dimension:         ", Dim)
        cat("\n Column Names:      ", colnames(x@Data) )
        firstName = rownames(x@Data)[1]
        lastName = rownames(x@Data)[Dim[1]]
        cat("\n Row Names:         ", firstName, " ... ", lastName)
        # Date/Time Positions:
        positions = seriesPositions(x)
        cat("\nPositions:          ")
        cat("\n Start:             ", as.character(start(positions)))
        cat("\n End:               ", as.character(end(positions)))
        # Other Attributes:
        cat("\nAttributes:         ")
        cat("\n Format:            ", x@format)
        cat("\n FinCenter:         ", x@FinCenter)
        cat("\n Units:             ", x@units)
        cat("\n Title:             ", x@title)
        cat("\n Documentation:     ", x@documentation)
        cat("\n") 
    }  
    summary(my.ts)
    ###
    
    
    # Change Content of 'timeSeries' Slots:
    # p. 28
    my.ts@title = 
        "My timeSeries"
    my.ts@documentation = c(
        "Simulated annual price data using the",
        "S-PLUS function rnorm")
    my.ts@units = c(
        "USD",
        "USD")
    # Change Colnames:
    colnames(my.ts@Data) = my.ts@units
    summary(my.ts)
    ###


    # The file "yhoo.df.csv" contains data representing daily 
    # transaction information of Yahoo stock, with the following 
    # six columns: Date, Open, High, Low, Close, Volume.
    # Data are downloadable and can be updated from Yahoo's web site.
    URL = "http://www.itp.phys.ethz.ch/econophysics/R/data/textbooks/"
    SRC = "ZivotWang/data/yhoo.df.csv"
    DATA = paste(URL, SRC, sep = "") 
    download.file(DATA, destfile = "yhoo.df.csv")
    yhoo.df = readSeries("yhoo.df.csv")
    print(yhoo.df[1,])
    print(end(yhoo.df))
    ###
    
    
    # Convert Data Frame to Time Series:
    # p. 30
    yhoo.df = as.data.frame(yhoo.df)
    yhoo.df[1:5, ]
    td = timeDate(
        charvec = rownames(yhoo.df), 
        format = "%Y-%m-%d", 
        FinCenter = "GMT")
    td[1:5]
    # Next:
    yhoo.ts = timeSeries(
        data = yhoo.df, 
        charvec = td@Data, 
        format = "%Y-%m-%d",
        FinCenter = "GMT")
    yhoo.ts[1:5, ]
    # Add Colnames:
    colnames(yhoo.ts@Data) <- colnames(yhoo.df)
    yhoo.ts[1:5, ]
    # Print Summary
    summary(yhoo.ts)
    ###
    
    
    ### HIGH FREQUENCY DATA ###
    
    
    # Example using Tsay's data for 3M, date information is expressed 
    # as the day of the month and the number of seconds from midnight 
    # Data is for December 1999.
    # Columns are: day - integer representing the trading day of the 
    # month, sec - trade.time integer representing the trading time 
    # recorded as the number of seconds from midnight, price transaction 
    # price in dollars. 
    # Downloaded from: http://www.gsb.uchicago.edu/fac/ruey.tsay/teaching/fts/
    file = paste(dataPath, "highFreq3M.df.csv", sep = "")
    zfile = zip.file.extract(file, "Rdata.zip")
    highFreq3M.df = read.table(zfile, header = TRUE, sep = ";")
    ###
    
    
    # Convert Tsay's data for 3M to 'timeSeries' object:
    # p. 30
    # Date information is expressed as the day of the month and 
    # the number of seconds from midnight Data is for December 1999. 
    # Create "timeSeries" from nonstandard input:
    d = highFreq3M.df[, 1]
    h = trunc(highFreq3M.df[, 2]/3600)
    min = trunc((highFreq3M.df[, 2] - h*3600)/60)
    s = highFreq3M.df[,2] - h*3600 - min*60
    date.time = timeCalendar(y = 1999, m = 12, d, h, min, s)
    hf3M.ts = timeSeries(data = highFreq3M.df[, 3], date.time,
        units = "3M")
    # Show Results: 
    class(hf3M.ts)
    start(hf3M.ts)
    end(hf3M.ts)
    hf3M.ts[1:5, ]
    summary(hf3M.ts)
    ###
    

# ------------------------------------------------------------------------------
# Section 2.2.6 - Aggregating and Disaggregating Time Series


    # NOTE:
    #   "Rmetrics has no special function to aggregate or diasaggregate
    #   a 'timeSeries' object. Use the very powerful function 'applySeries"
    #   which can fulfill all the tasks which you expect from a function
    #   for aggregating and disaggregating 'timeSeries' objects.
    ###
    
    
    # The file "singleIndex.dat.csv" contains the monthly closing 
    # prices for Microsoft Corporation and the S&P 500 index.
    # Data are downloadable and can be updated from Yahoo's web site.
    singleIndex.dat = readSeries("singleIndex.dat.csv")
    print(singleIndex.dat[1,])
    print(end(singleIndex.dat))
    ###
    
    
    # Aggregate - Subsetting "End-of-Year" data:
    # p. 31/32
    dec.vals = ("12" == months(seriesPositions(singleIndex.dat)))
    annual.p = singleIndex.dat[dec.vals, ]
    annual.p
    # To my opinion the year 2001 has no end-of-year entry, 
    #   it shouldn't appear!
    # Here, I see no reason for using the function pickClose ...
    # Add Title:
    annual.p@title = "Subsetted End-of-Year Data"
    # Summary:
    summary(annual.p)
    ### 
    
    
    # Aggregate - Subsetting "End-of-Month" data for 1991:
    # p. 33
    # Extract Year from calendar atoms:
    ninetyone.vals = ("1991" == atoms(seriesPositions(singleIndex.dat))[,"Y"])
    monthly.p = singleIndex.dat[ninetyone.vals, "MSFT"]
    monthly.p
    # Add title:
    monthly.p@title = "Subsetted End-of-Month Data"
    # Summary:
    summary(monthly.p)
    ###
    
    
    # The file "msft.dat.csv" contains data representing the open, 
    # high, low, close and volume information for Microsoft stocks. 
    # Data are downloadable and can be updated from Yahoo's web site.
    URL = "http://www.itp.phys.ethz.ch/econophysics/R/data/textbooks/"
    SRC = "ZivotWang/data/msft.dat.csv"
    DATA = paste(URL, SRC, sep = "") 
    download.file(DATA, destfile = "msft.dat.csv")
    msft.dat = readSeries("msft.dat.csv")
    print(msft.dat[1,])
    print(end(msft.dat))
    ###
    
    
    # Aggregate - Daily volume weighted average prices on a monthly scale:
    # p. 34
    # First, a note on Rmetrics aggregateSeries() function:
    args(applySeries)
    #   The function can handle every kind of blocks!
    #   The arguments "from" and "to" are two "timeDate" position vectors 
    #   which size the blocks, the arguments "include.from" and "include.to"
    #   are two logicals, which determine if the start and end date/time
    #   points of the investigation should be included or not? By default,
    #   the starting point is included, the endpoint not.
    # Preprocess Data: Use Rmetrics cutSeries() function:
    args(cutSeries)
    msft.dat.smpl = cutSeries(
        msft.dat, from = timeDate("2000-10-01"), to = timeDate("2001-08-31" ))
    msft.dat.smpl[1:5, ]
    nRow = nrow(msft.dat.smpl@Data)
    msft.dat.smpl[(nRow-5):nRow, ]
    # Create from-to Calendar Blocks:
    y = c(rep(2000, 3), rep(2001, 9))
    m = c(10:12, 1:9)
    from = timeCalendar(y, m)[-12]
    to = timeCalendar(y, m)[-1] - 24*3600
    data.frame(from, to)
    # Aggregate to Monthly Means:
    applySeries(x = msft.dat.smpl, from = from, to = to, FUN = colMeans)
    # Now Volume Weighted: Write vol.wtd.avg.price() function:
    vol.wtd.avg.price = function(x) {
        VolumeSum = as.double(sum(x[, "Volume"]))
        nrowx = length(x[1, ])
        Open = x[1, "Open"]
        High = max(x[, "High"])
        Low = min(x[, "Low"])
        Close = x[nrowx, "Close"]
        vwap.Open = sum(  x[, "Open"] * x[, "Volume"] ) / VolumeSum
        vwap.Close = sum(  x[, "Close"] * x[, "Volume"] ) / VolumeSum
        c(Open, High, Low, Close, vwap.Open, vwap.Close, VolumeSum) }   
    # Aggregate to Volume Weighted Monthly Means:
    msft.vwap.dat = applySeries(x = msft.dat.smpl, from = from, 
        to = to, FUN = vol.wtd.avg.price, colNames = c("Open", "High", 
        "Low", "Close", "vwap.Open", "vwap.Close", "Volume") )
    msft.vwap.dat[, -7]
    ###
    
    
    # The file "IP.dat.csv" contains data representing seasonally 
    # adjusted U.S. Industrial Production Index. 
    # The file "CPI.dat.csv" contains data representing seasonally 
    # adjusted U.S. Consumer Price Index (CPI). 
    # Data are downloadable from Economagics's web site.
    # Start with IP:
    URL = "http://www.itp.phys.ethz.ch/econophysics/R/data/textbooks/"
    SRC = "ZivotWang/data/IP.dat.csv"
    DATA = paste(URL, SRC, sep = "") 
    download.file(DATA, destfile = "IP.dat.csv")
    IP.dat = readSeries("IP.dat.csv")
    print(IP.dat[1,])
    print(end(IP.dat))
    # Next CPI:
    URL = "http://www.itp.phys.ethz.ch/econophysics/R/data/textbooks/"
    SRC = "ZivotWang/data/CPI.dat.csv"
    DATA = paste(URL, SRC, sep = "") 
    download.file(DATA, destfile = "CPI.dat.csv")
    CPI.dat = readSeries("CPI.dat.csv")
    print(CPI.dat[1,])
    print(end(CPI.dat))
    ###
    
    
    # Disaggregate - Interpolating monthly CPI to daily CPI:
    # p. 35
    # CPI Data is monthly - thus we have to disaggregate ...
    c(start(CPI.dat), end(CPI.dat))
    cpi = cutSeries(CPI.dat, from = "1990-12-01", to = "2001-02-01")
    # Return head and tail of the series:
    head(cpi)
    tail(cpi)
    ###
    
    
    # The file "DowJones30.csv" contains closing prices for 30 stocks
    # represented in the Dow Jones Industrial Average Index. 
    # Data are downloadable from Yahoo's web site.
    URL = "http://www.itp.phys.ethz.ch/econophysics/R/data/textbooks/"
    SRC = "ZivotWang/data/DowJones30.csv"
    DATA = paste(URL, SRC, sep = "") 
    download.file(DATA, destfile = "DowJones30.csv")
    DowJones30 = readSeries("DowJones30.csv")
    print(DowJones30[1,])
    print(end(DowJones30))
    ###
    
    
    # Disaggregate - Using "before", "after", "interp" methods
    # p. 35/36
    # Disaggregation is a somewhat arbitrary process because we don't 
    # know to which dates the CPI data exactly belong. We only know
    # the month but not the day. Here we assign the last date in a 
    # month to the date of measurement of the value.
    # Rmetrics
    #    has the function alignDailySeries() to align time series:
    args(alignDailySeries)
    # The function assigns values to all days from the first until
    # the last date of the input time series. After that one has 
    # the choice to remove weekend days or not. By default weekends
    # are excluded.
    # Start with the "before" method:
    #   Note, we remove the first two data records to become aligned
    #   with the MSFT data
    cpi.daily.before = 
        alignDailySeries(cpi, method = "before")[-(1:2)]
    cpi.daily.before[c(1:3, 21:23)]
    # ... I think this series starts properly on 1/1/1991
    # Next consider the "after" method:
    cpi.daily.after = 
        alignDailySeries(cpi, method = "after")[-(1:2)]
    cpi.daily.after[c(1:3, 21:23)]
    # ... I think also this series starts properly on 1/1/1991
    # Next consider the "interp"-olation method:
    cpi.daily.interp = 
        alignDailySeries(cpi, method = "interp", include.weekends = TRUE)
    # ... that we miss no days according to different holiday rules
    # Cut nicely:
    cpi.daily.interp = 
        cutSeries(cpi.daily.interp, "1991-01-01", "2000-12-31")
    cpi.daily.interp[c(1:3, 21:23)]
    # ... note weekends are excluded
    #
    # We also align the MSFT data:   
    msft.daily.p = DowJones30[, "MSFT"] 
    msft.daily.interp = 
        alignDailySeries(msft.daily.p, method = "interp", 
            include.weekends = TRUE)
    c(start(msft.daily.interp), end(msft.daily.interp)) 
    msft.daily.interp = cutSeries(msft.daily.interp, 
        from = start(cpi.daily.interp), to = end(cpi.daily.interp))
    # Cut nicely:
    msft.daily.interp = 
        cutSeries(msft.daily.interp, "1991-01-01", "2000-12-31")
    msft.daily.interp[c(1:3, 21:23)]
    # Ceck Dimensions:
    dim(cpi.daily.interp@Data)
    dim(msft.daily.interp@Data)
    # Remove weekends (if you like):
    cpi.daily.interp = alignDailySeries(cpi.daily.interp, method = "interp")
    msft.daily.interp = alignDailySeries(msft.daily.interp, method = "interp")
    # Again Ceck Dimensions:
    dim(cpi.daily.interp@Data)
    dim(msft.daily.interp@Data)
    #
    # Finally we compute the real prices:
    msft.daily.rp = 100*(msft.daily.interp/cpi.daily.interp)
    msft.daily.rp[c(1:3, 21:23)]
    # Plot:
    par(mfrow = c(1, 1))
    msft.daily.interp@title = "MSFT - Real and Nominal Prices"
    plot(msft.daily.interp, type = "l", ylab = "Price")
    lines(msft.daily.rp, col = "red")
    ###

    
    # The file "shiller.annual.csv" holds Robert Shiller's financial
    # and economic data.
    URL = "http://www.itp.phys.ethz.ch/econophysics/R/data/textbooks/"
    SRC = "ZivotWang/data/shiller.annual.csv"
    DATA = paste(URL, SRC, sep = "") 
    download.file(DATA, destfile = "shiller.annual.csv")
    shiller.annual = readSeries("shiller.annual.csv")
    print(shiller.annual[1,])
    print(end(shiller.annual))
    ###
    
    
    # Disaggregate:
    # p. 37
    # Rmetrics 
    #   we don't have not yet a disaggregation function.
    #   Altough these functions can be easily implemented there are
    #   dozens of possibilities how to dot it. Here we show an example
    #   using cubic spline interpolations.
    # Select Data:
    div.annual  = shiller.annual[, "dividend"]     
    # Write Your Personal Disaggregation Function:
    disaggregateAnnualSeries = function(data, k, out.positions, 
        type = "spline") {
        # Here we assume an univariate timeSeries as input
        data = cumsum(as.vector(seriesData(data)))
        n = length(data)
        # Use cubic Spline Interpolation:
        if (type == "spline") {
            ans = spline(x = (1:n)*k, data, n = n*k, xmin = 1, xmax = n*k)$y
            ans = c(ans[1], diff(ans)) }
        else {
            # Here you can add further approaches based on
            # other approximations, interpolations, imputations,
            # and regression schemes ...
            stop("Wrong type selected") }
        # Convert Result to timSeries Object
        timeSeries(ans, out.positions, FinCenter = "GMT", units = "dividend") } 
    # Create disaggregated Series:
    monthly.dates = timeCalendar(
        y = rep(1871:2000, each = 12), m = rep(1:12, times = 130) )
    div.monthly = disaggregateAnnualSeries(
        data = div.annual, k = 12, out.positions = monthly.dates )
    # Print:
    div.monthly[1:24, ]
    # ... Note, there are smaller differences for the first year
    # which is extrapolated from the spline fit. 
    ###
    
            
    # Compare just the numbers with SPlus for the first 72 months:
    # Rmetrics
    # Annual Data:
    time.annually = as.numeric((1:6)*12)
    div.annually = as.vector(unlist(seriesData(shiller.annual[1:6, "dividend"])))
    # Now Monthly:
    time.monthly = 1:72
    div.monthly.R = as.vector(div.monthly)[1:72]
    div.monthly.SPlus = c(  
        0.02999, 0.01867, 0.01916, 0.01963, 0.02009, 0.02054,
        0.02097, 0.02140, 0.02181, 0.02220, 0.02259, 0.02296,
        0.02332, 0.02367, 0.02400, 0.02433, 0.02463, 0.02493,
        0.02522, 0.02549, 0.02575, 0.02599, 0.02623, 0.02645,
        0.02666, 0.02685, 0.02703, 0.02720, 0.02736, 0.02751,
        0.02764, 0.02776, 0.02787, 0.02796, 0.02804, 0.02811,
        0.02817, 0.02818, 0.02816, 0.02809, 0.02798, 0.02783,
        0.02764, 0.02741, 0.02714, 0.02683, 0.02648, 0.02608,
        0.02567, 0.02530, 0.02501, 0.02479, 0.02465, 0.02458,
        0.02458, 0.02465, 0.02479, 0.02501, 0.02530, 0.02567,
        0.02607, 0.02636, 0.02650, 0.02650, 0.02636, 0.02607,
        0.02563, 0.02505, 0.02432, 0.02345, 0.02243, 0.02127)   
    # Plot it:
    div = c(div.monthly.SPlus, div.monthly.R)
    par(mfrow = c(2, 1), cex = 0.7)
    plot(1870+time.monthly/12, div.monthly.SPlus, ylim = range(div),
        xlab = "Year", ylab = "Dividend", main = "First six years")
    points(1870+time.monthly/12, div.monthly.R, col = "green")
    # Use year-midpoints
    points(1870+time.annually/12 - 0.5, div.annually/12, col = "red")
    # Annual Values:
    apply(matrix(div.monthly.SPlus, byrow = TRUE, ncol = 12), 1, sum)
    apply(matrix(div.monthly.R, byrow = TRUE, ncol = 12), 1, sum)
    # ... note the extrapolation part looks different.
    ###
        
    
# ------------------------------------------------------------------------------
# Section 2.2.7 - Merging Time Series
    
    
    # The file "IP.dat.csv" contains data representing seasonally 
    # adjusted U.S. Industrial Production Index.
    # The file "CPI.dat.csv" contains data representing seasonally 
    # adjusted U.S. Consumer Price Index (CPI). 
    # Data are downloadable and can be updated from Economagics's web site. 
    # Rmetrics
    IP.dat = readSeries("IP.dat.csv")
    print(IP.dat[1, ])
    print(end(IP.dat))
    CPI.dat = readSeries("CPI.dat.csv")
    print(CPI.dat[1, ])
    print(end(CPI.dat))
    ###
    
    
    # Merge a 'timeSeries' with a 'mtrix' using the "mergeSeries" function:
    # p.39 
    # Rmetrics
    #   has a function named "mergeSeries" which can be used for 
    #   S-Plus' function "seriesMerge". The "mergeSeries" function  
    #   merges a 'timeSeries' object with a 'matrix' object having  
    #   the same number of rows.
    # Show the arguments of the "mergeSeries" function:
    args(mergeSeries)   
    # make notations consistent and unique ...
    CPI.dat[c(1:2, length(CPI.dat@positions))]
    IP.dat[c(1:2, length(IP.dat@positions))]
    c(start(IP.dat), end(IP.dat))
    c(start(CPI.dat), end(CPI.dat))
    # Write CPI as matrix:
    CPI.mat = as.matrix(CPI.dat[-(1:72),])
    # Merge:
    IP.CPI.dat = mergeSeries(IP.dat, CPI.mat)
    IP.CPI.dat[1:2, ]
    ###
    
    
    # Plot:
    # Rmetrics
    par(mfrow = c(1, 1), cex = 0.7)
    IP.CPI.dat@title = "IP and CPI"
    plot(IP.CPI.dat, type = "l", ylab = "Index")
    abline(h = 0, lty = 3)
    title(main = IP.CPI.dat@title)
    ### 


# ------------------------------------------------------------------------------
# Section 2.2.8 - Dealing with Missing Values


    # Show the arguments of the "interpNA" function:
    # p. 40
    # Let us a write a simple function for NA imputation:
    interpNA = 
    function(x, method = c("linear", "constant", "before", "after")) {
        # Description:
        #   Interpolates missing values in a vector     
        # Which Method?
        method = method[1]
        f = 0
        if (method == "before") { method = "constant"; f = 0 }
        if (method == "after") { method = "constant"; f = 1 }
        # Interpolate:
        n = length(x)
        idx = (1:n)[!is.na(x)]
        x = approx(idx, x[idx], 1:n, method = method, f = f)$y
        # Return Value:
        matrix(x, ncol = 1) }    
    ###
    
    
    # Use "interpNA" Function:
    # p. 40
    dates = c(
        "01/02/1990", "01/03/1990", "01/04/1990", "01/05/1990", "01/09/1990",
        "01/10/1990", "01/12/1990", "01/15/1990", "01/16/1990", "01/17/1990",
        "01/18/1990", "01/19/1990", "01/22/1990", "01/23/1990", "01/24/1990",
        "01/25/1990", "01/26/1990", "01/29/1990", "01/30/1990", "01/31/1990")
    close = c(
        2810.15, 2809.73, 2796.08, 2773.25, 2766.00, 2750.64, 2689.21, 
        2669.37, 2692.62, 2659.13,      NA, 2677.90, 2600.45, 2615.32, 
        2604.50, 2561.04, 2559.23, 2553.38, 2543.24, 2590.54)
    djia.close = 
        timeSeries(close, dates, format = "%m/%d/%Y", FinCenter = "GMT",
        units = "djia.close")
    djia.close[10:12]
    dimNames = dimnames(djia.close@Data)
    # Use linear interpolation scheme:
    djia.close@Data <- interpNA(x = seriesData(djia.close))
    dimnames(djia.close@Data) <- dimNames
    djia.close[10:12]
    # ... Try to implement spline interpolation
    ###


# ------------------------------------------------------------------------------
# Section 2.3 - TIME SERIES MANIPULATIONS 


    # In this Chapter we deal only with "Date" data, therefore we set
    myFinCenter = "GMT"
    ###
    
    
    # The function 'readSeries' allows you to read data from a
    # spreadsheet file and transforms it ditrectly to a 'timeSeries'
    # object.
    args(readSeries)
    ### 
      
      
# ------------------------------------------------------------------------------    
# Section 2.3.1 - PART A: Creating Lags


    # The file "singleIndex.dat.csv" contains the monthly closing 
    # prices for Microsoft Corporation and the S&P 500 index.
    # Data are downloadable and can be updated from Yahoo's web site.
    singleIndex.dat[1,]
    end(singleIndex.dat)
    ###
    
    
    # Create lagged/leading 'timeSeries' using the "laggedSeries" function:
    # p.41 
    # Rmetrics
    #   has a method named "lag" which can be used for S-Plus' function 
    #   "tslag". The "lag" method computes lagged or leading values for 
    #   a 'timeSeries' object.
    # Show the arguments of the "lagSeries" method:
    args(lagSeries)
    ###
    
    
    # Print the head of the 'timeSeries' object:
    # p. 41
    singleIndex.dat[1:5, ]
    ###
    
    
    # Create lagged 'timeSeries' object:
    # p. 41
    lagSeries(singleIndex.dat[1:5, ])
    # ... the first five from the the whole series
    lagSeries(singleIndex.dat)[1:5, ]
    ###
    
    
    # Create lagged 'timeSeries' object where NAs are trimmed:
    # p. 41
    lagSeries(singleIndex.dat, trim = TRUE)[1:5, ]
    ###
    
    
    # Create leading 'timeSeries' object:
    # p. 41/42
    lagSeries(singleIndex.dat[1:5, ], k = -1)
    ###
    
    
    # Create multiple lagged 'timeSeries' object
    td.lagged = lagSeries(singleIndex.dat[1:5, ], k = c(1, 3))
    td.lagged
    # Note, that the ordering is different from S-Plus!
    # S-Plus like ordered ...
    td.lagged[, c(1, 3, 2, 4)]
    ###
    
    
    # ... another multiple lagged 'timeSeries' object
    ltd = lagSeries(singleIndex.dat[1:5, ], k = -1:1)
    ltd
    # S-Plus like ordered ...
    ltd[, c(1, 4, 2, 5, 3, 6)]
    ###
    
    
    # Create a S-Plus synonym function call:
    # Rmetrics
    tslag = lagSeries
    tslag(singleIndex.dat[1:5, ])
    ###
    

# ------------------------------------------------------------------------------    
# Section 2.3.1 - PART B: Creating Differences    
    

    # Create Differences 'timeSeries' 
    # p. 42 
    # Show the arguments of the "diff.timeSeries" method:
    args(diffSeries)
    ###
    

    # Difference time series with lag 1:
    # p. 43
    diffSeries(singleIndex.dat[1:5,], lag = 1, trim = FALSE)
    # You can use default settings ...
    diffSeries(singleIndex.dat[1:5,])
    ###
    
    
    # Difference time series with lag 2:
    # p. 43
    # Fill with zeros for NAs ...
    diffSeries(singleIndex.dat[1:5,], lag = 2, trim = FALSE , pad = 0)
    ###
    
    
    # Difference time series twice with lag 1:
    # p. 43
    diffSeries(singleIndex.dat[1:5,], lag = 1, diff = 2)
    # pad the series ...
    # Rmetrics
    diffSeries(singleIndex.dat[1:5,], lag = 1, diff = 2, pad = 0)
    # trim the series ...
    # Rmetrics
    diffSeries(singleIndex.dat[1:5,], lag = 1, diff = 2, trim = TRUE)
    ###
    

# ------------------------------------------------------------------------------    
# Section 2.3.3 - PART A: Computing Asset Returns - Compute Returns


    # Rmetrics has a function named "returnSeries" which creates
    # simple or continuously compounded returns from a price series.
    # The functionality is similar to the S-Plus function "getReturns".
    
    
    # Show the arguments of the "diff.timeSeries" method:
    # p. 46
    args(returnSeries)
    ###
    
    
    # Print column names:
    # R has no function for colIds(), use ...
    # Rmetrics
    colnames(singleIndex.dat@Data)
    # ... or write a wrapper:
    colIds = function(x) {
        if (class(x) != "timeSeries") 
            stop("x must be a timeSeries object")
        return(colnames(x@Data)) }
    colIds(singleIndex.dat)
    ###
    
    
    # Print Series:
    # p. 46
    singleIndex.dat[1:3, ]
    print(singleIndex.dat[1:3, ])
    ###
    
    
    # Create a series of discrete returns:
    # p. 46
    ret.d = returnSeries(singleIndex.dat, type = "discrete", 
        percentage = TRUE, trim = FALSE, digits = 5)
    ret.d[1:3, ]
    ###
    
    
    # Create a trimmed series of discrete returns:
    # p. 47
    ret.d = returnSeries(singleIndex.dat, type = "discrete", 
        trim = FALSE, digits = 5)
    ret.d[1:3, ]
    ###
    
    
    # Create a series of continuous returns:
    # p. 47
    ret.cc = returnSeries(singleIndex.dat, digits = 5)
    ret.cc[1:3, ]
    ###
    
    
    # Create a trimmed series of continuous percentage returns:
    # Rmetrics
    ret.cc = returnSeries(singleIndex.dat, percentage = TRUE)
    ret.cc[1:3, ]
    # back to returns, devide the 'timeSeries' by 100 ...
    ret.cc = ret.cc/100
    ret.cc[1:3, ]
    ###
    

    # Annual compounded returns on monthly time steps.
    # S-Plus ha a special function to do this in Rmetrics we just use
    # the function apply:
    # p. 47
    # First Create "from" - "to" Calendar Blocks:
    ret.cc = returnSeries(singleIndex.dat, type = "continuous")
    c(start(ret.cc), end(ret.cc))
    # Last Days in Month:
    from = timeSequence("1990-03-01", "2000-03-01", "month",
        format = "%Y-%m-%d") - 24*3600
    to   = timeSequence("1991-02-01", "2001-02-01", "month",
        format = "%Y-%m-%d") - 24*3600 
    # Annual Sum Aggregates on Monthly Scale:
    applySeries(x = ret.cc, from, to, colSums)[1:3, ]
    rbind(
        colSums(seriesData(ret.cc[1:12, ])),
        colSums(seriesData(ret.cc[2:13, ])),
        colSums(seriesData(ret.cc[3:14, ])))
    # Note, that the rownames are to my feeling somewhat strange.
    # "The annual return reported for Feb 1990 is the sum of the 
    # twelve monthly returns from February 1990 through January 
    # 1991." Why is the rowname at the beginning of the period 
    # and not at the end?
    ###
    
    
    # Next, annual returns on an annual step size (non-overlapping):
    # p. 47
    ret.cc = returnSeries(singleIndex.dat, type = "continuous")
    c(start(ret.cc), end(ret.cc))
    # Last Days in Month:
    from = timeCalendar(y = 1990:2000, m = 2) - 24*3600
    to = timeCalendar(y = 1991:2001, m = 1) - 24*3600
    applySeries(x = ret.cc, from = from, to = to, FUN = colSums)[1:3, ]
    rbind(
        colSums(seriesData(ret.cc[1:11, ])),
        colSums(seriesData(ret.cc[12:23, ])),
        colSums(seriesData(ret.cc[24:35, ])))
    ### 
    
    
    # Next, monthly >>discrete<< returns on an annual step size
    # p. 48
    # Last Days in Month:
    ret.d = returnSeries(singleIndex.dat, type = "discrete", digits = 12)
    from = timeSequence("1990-03-01", "2000-03-01", "month",
        format = "%Y-%m-%d") - 24*3600
    to   = timeSequence("1991-02-01", "2001-02-01", "month",
        format = "%Y-%m-%d") - 24*3600 
    colProds = function (x) { round (
        apply(x+1, MARGIN = 2, FUN = prod) - 1 , digits = 6 ) }
    applySeries(x = ret.d, from, to, FUN = colProds)[1:3, ]
    colProds(seriesData(ret.d)[1:12, ])
    ### 
    
    
    # Next, the same non-overlapping:
    # p. 48
    # Last Days in Month:
    ret.d = returnSeries(singleIndex.dat, type = "discrete", 
        trim = FALSE, digits = 12)
    from = timeCalendar(y = 1990:2000, m = 2) - 24*3600
    to = timeCalendar(y = 1991:2001, m = 1) - 24*3600
    colProds = function (x) { round (
        apply(x+1, MARGIN = 2, FUN = prod) - 1 , digits = 4 ) }
    applySeries(x = ret.d, from, to, FUN = colProds)[1:3, ]
    ###
    

# ------------------------------------------------------------------------------
# Section 2.4 - Visalizing Time Series


    # Rmetrics uses the standard plotting functions available in R.
    #   Beside some minor differences these are quite similar to those
    #   available in S-Plus.
    # Trellis Plots are supported by R, but not yet implemented in
    #   Rmetrics. So Trellis graphs in the following examples are are
    #   drawn by the standard plotting functions.
    ###
    
    
    # The function 'readSeries' allows you to read data from a
    # spreadsheet file and transforms it ditrectly to a 'timeSeries'
    # object.
    args(readSeries)
    ###
    

# ------------------------------------------------------------------------------
# Section 2.4.1 - Plotting 'timeSeries' Objects Using the Generic Plot Function
    

    # The file "singleIndex.dat.csv" contains the monthly closing 
    # prices for Microsoft Corporation and the S&P 500 index.
    # Data are downloadable and can be updated from Yahoo's web site.
    # Rmetrics
    singleIndex.dat = readSeries("singleIndex.dat.csv")
    print(singleIndex.dat[1, ])
    print(end(singleIndex.dat))
    ###
    
    
    # Plot a single series [Figure 2.1]
    # p. 49
    msft.p = singleIndex.dat[, "MSFT"]
    msft.p@title = "Monthly Closing Price on Microsoft"
    msft.p@documentation = c(
        "Monthly closing price adjusted for stock",
        "splits and dividends.")
    msft.p@units = "US dollar price"
    msft.p@title
    msft.p@documentation
    msft.p@units
    # Plot:
    par(mfrow = c(2, 1), cex = 0.7)
    plot(msft.p, type = "l", ylab = msft.p@units, 
        main = msft.p@title, col = "steelblue4")
    # Use solid grid lines ...
    plot(msft.p, type = "l", ylab = msft.p@units, 
        main = msft.p@title, col = "steelblue4")
    grid(lty = "solid")
    ###
    
    
    # Plot multiple series on the same graph [Figure 2.2]
    # p. 49
    singleIndex.dat@units = c("US dollar price", "US dollar price")
    singleIndex.dat@title = "MSFT and SP500"
    # Multivariate Plot:
    par(mfrow = c(2, 1), cex = 0.7)
    plot(singleIndex.dat, col = "steelblue4", main = singleIndex.dat@title)
    # With Legend and Plot Arguments:
    # > legend(0.1, 1400, legend = colnames(singleIndex.ts), lty = c(1, 3))
    plot(singleIndex.dat, lty = c(1, 3), xlab = "Year", 
        ylab = "Index - Stock Price", col = "steelblue4")
    title(singleIndex.dat@title)
    ### 

    
    # Create a two panel plot [Figure 2.3]
    par(mfrow = c(2, 1), cex = 0.7)
    plot(singleIndex.dat[, "MSFT"], type = "l",
      main = "Monthly Price on Microsoft", col = "steelblue4")
    plot(singleIndex.dat[, "SP500"], type = "l",
      main = "Monthly Price on S&P 500 Index", col = "steelblue4")
    ###
    
    
    # The file "msft.dat.csv" contains data representing the open, 
    # high, low, close and volume information for Microsoft stocks. 
    # Data are downloadable and can be updated from Yahoo's web site.
    msft.dat = readSeries("msft.dat.csv")
    print(msft.dat[1, ])
    print(end(msft.dat))
    ###
       
    
    # Create a High-Low-Open-Close and Volume Plot [Figure 2.4]
    # p. 51/52
    par(mfrow = c(2, 1), cex = 0.7)
    msft.dat[1:5, ]
    ohlcDailyPlot(msft.dat)   
    ###
        

# ------------------------------------------------------------------------------
# Section - 2.4.2 - Trellis Plotting Functions


    # Sorry, 
    # Rmetrics has not yet implemented functions for easy to
    # use Trellis plots ...
    # Therefore, we use traditional plot graphics ...
    # Note, data sets are loaded in example 'xmpZWCh02Sec1.R' 
    ###
    
      
    # The file "DowJones30.csv" contains closing prices for 30 stocks
    # represented in the Dow Jones Industrial Average Index. 
    # Data are downloadable and can be updated from Yahoo's web site.   
    DowJones30 = readSeries("DowJones30.csv")
    print(DowJones30[1,])
    print(end(DowJones30))
    ###
    
      
    # Create a 6-panel series plot [Figure 2.5]:
    # p. 52/53
    DJ.ret = returnSeries(DowJones30[, 1:6], percentage = TRUE)
    DJ.ret@units
    par(mfrow = c(3, 2), cex = 0.5)
    for ( i in 1:6 ) {
        plot(
            DJ.ret[, i], 
            type = "l", 
            xlab = "Year", 
            ylab = "Return", 
            col = "steelblue4")
        title(main = DJ.ret@units[i])  }
    ###
        
        
    # Create a 6-panel histogram plot [Figure 2.6]:
    # p. 53/54
    par(mfrow = c(3, 2), cex = 0.5)
    for ( i in 1:6 ) {
        hist(
            seriesData(DJ.ret[, i]),
            probability = TRUE,
            n = 30,
            main = DJ.ret@units[i],
            xlim = c(-10, 10),
            xlab = "Return", 
            col = "steelblue4",
            border = "white" ) }
    ###

    
    # Create a 6-panel Quantile-Quantile Plot [Figure 2.7]:
    # p. 53/54
    par(mfrow = c(3, 2), cex = 0.5)
    for ( i in 1:6 ) {
        qqnorm(
            seriesData(DJ.ret[, i]), 
            ylim = c(-15, 15),
            main = DJ.ret@units[i],
            col = "steelblue4")
        qqline(seriesData(DJ.ret[, i]),
            lty = 3) 
        grid() }
    ###


################################################################################

