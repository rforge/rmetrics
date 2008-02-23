\name{fCalendar-package}


\alias{fCalendar-package}


\alias{fCalendar}


\docType{package}


\title{Utilities and Tools Package}


\description{

    Package of calendar, date and time tools and utilities for 
    Rmetrics.
    
}


\details{

    \tabular{ll}{
        Package:    \tab fCalendar\cr
        Type:       \tab Package\cr
        Version:    \tab 270.73\cr
        Date:       \tab 2008\cr
        License:    \tab GPL Version 2 or later\cr
        Copyright:  \tab (c) 1999-2008 Diethelm Wuertz, Rmetrics Foundation, GPL\cr
        URL:        \tab \url{http://www.rmetrics.org}
    }

}


\section{Overview of Topics:}{

    This help file describes the concepts and methods behind the S4
    'timeDate' class used in Rmetrics for financial data and time 
    management together with the management of public and ecclesiastical
    holidays.
    
    The 'timeDate' class fulfills the conventions of the ISO 8601 
    standard as well as of the ANSI C and POSIX standards. Beyond
    these standards Rmetrics has added the "Financial Center" concept
    which allows to handle data records collected in different time 
    zones and mix them up to have always the proper time stamps with 
    respect to your personal financial center, or alternatively to the GMT
    reference time. It can thus also handle time stamps from historical 
    data records from the same time zone, even if the financial 
    centers changed day light saving times at different calendar
    dates.
    
    Evenmore 'timeDate' is almost compatible with the 'timeDate'
    class in Insightful's SPlus 'timeDate' class. Thus if you move between 
    the two worlds of R and SPlus, you have not to rewrite your
    code, this is very important mainly for business applications.
    
    The 'timeDate' class offers not only date and time functionality
    it als offers very sophisticated calendar manipulations concerned
    with business days, weekends, and public and ecclesiastical holidays. 
    
    The information given in this help page is presented in three
    major chapters:
       
    1. S4 'timeDate' Class and Functions\cr
    2. Operations on 'timeDate' Objects\cr
    2. Daylight Saving Time and Financial Centers\cr
    3. Holidays and Holiday Calendars
 
}  


\section{1. S4 'timeDate' Class and Generator Functions}{
    
    
    Date and time stamps are represented by an S4 object of class 'timeDate'. 
        
    \preformatted{
    setClass("timeDate",
        representation(
            Data = "POSIXct",
            format = "character",
            FinCenter = "character"
        ))
    }
    
    They have three slots.  The \code{@Data} slot holds the time
    stamps which are \code{POSIXct} formatted as specified in the 
    \code{@format} slot. The time stamps are local and belong to the 
    financial center expressed through the slot \code{@FinCenter}.
    
    
    There are several possibilities to generate a 'timeDate' object. The
    most forward procedure is to use one of the follown functions:\cr
    
    \code{timeDate} -- Creates a 'timeDate' object from scratch, \cr
    \code{timeSequence} -- creates a sequence of 'timeDate' objects,\cr
    \code{timeCalendar} -- creates a 'timeData' object from calendar atoms,\cr
    \code{Sys.timeDate} -- returns the current date and time as a 'timeDate' object.\cr 
    
    
    Using the function \code{timeDate} we can create 'timeDate' objects 
    from scratch specifying a character vector of time stamps and the
    financial center to which belongs the character vector. By default
    the financial center is not defined, i.e we use "GMT" as the reference
    to all date/time operations. Referencing dtaes/times with respect
    to your local time settings, you can modify and set the variable 
    \code{myFinCenter} to your local financial center.\cr
    
    Examples:
    \preformatted{
        # Show My local Financial Center - Note, by Default this is "GMT"
        print(myFinCenter)
        
        # Compose Character Vectors of Dates and Times:
        Dates <- c("1989-09-28","2001-01-15","2004-08-30","1990-02-09")
        Times <- c(  "23:12:55",  "10:34:02",  "08:30:00",  "11:18:23")
        charvec = paste(Dates, Times)
        
        # Create a 'timeDate' object for your Financial Center?
        timeDate(charvec)
        
        # From now on my financial center will be Zurich:
        myFinCenter <- Zurich
        timeDate(charvec)
        timeDate(charvec, FinCenter = "Tokyo")
        
    }

    NOTE: Rmetrics has implemented an automated date/time format identifyer
    for many common date/time formats which tries to recognize automatically 
    the format for the character vector of dates and times, try to see it\cr
    \code{.whichFormat(charvec)}.
    
    NOTE: Rmetrics automatically enforces that the midnight standard on dates 
    and times is always fulfilled, try to see it\cr
    \code{.midnightStandard("2008-01-31 24:00:00")}\cr
  
    
    Alternatively we can  create a sequence of 'timeDate' objects with the
    help of the function \ code{timeSequence}. This can be done in several
    ways, either specifying the range of the data through the arguments
    \code{from} and \code{to}, or when \code{from} is missing, one can
    input the \code{length.out} of the desired series. Note in the case
    of a monthly sequence, you have further options, e.g. to generate the
    series with the first or last day in each month, or to use more
    complex rules like the last or n-th Friday in every month. \cr
    
    Examples:
    \preformatted{
        # Lets work in an international environment:
        myFinCenter <- "GMT"
        
        # You get it in GMT - your Financial Center?
        timeDate(charvec)
        
        # Daily January 2008 Sequence:
        timeSequence(from = "2008-01-01", to = "2008-01-31", by = "day")
        
        # Monthly 2008 Sequence:
        tS = timeSequence(from = "2008-01-01", to = "2008-12-31", by = "month")
        tS
        
        # Do you want the last Day or the last Friday in Month Data ?
        timeLastDayInMonth(tS)
        timeLastNdayInMonth(tS, nday = 5)
        
    }
    
    
    A third possibility allows to create 'timeDate' objects  from calendar 
    atoms. You specify values or vectors of equal lengths of integers
    denoting year, month day, hour, minute and seocnds. If every day has
    the same time stamp, you  can also just add it by an offset.
    \cr

    Examples:
    \preformatted{
        # Monthly calendar for Current Year
        currentYear
        timeCalendar() 
        
        # Daily 'timeDates' for January data from Tokyo local time 16:00
        timeCalendar(2008, m=1, d=1:31, h=16, zone="Tokyo", FinCenter="Zurich")
        
        # Or add16 hours in seconds ...
        timeCalendar(2008, m=1, d=1:31, zone="Tokyo", FinCenter="Zurich") + 16*3600
     
    }

}
  

\section{2. Operations on 'timeDate' Objects}{

    
    We can perform many operations on 'timeDate' objects. We can add
    and subtract them, we can round and truncate them, we can subset 
    them, we can coerce them from and transform them to other objects.
    These are only a few options among many others. \cr
    
    \bold{Math Operations}
    
    Math operations allow to add and subtract dates and times,
    and perform logical operations on 'timeDate' objects.\cr
    
    Examples:
    \preformatted{
        # Date and Time Now:
        now = Sys.timeDate()
        
        # One Hour Later:
        now + 3600
        
        # Which date/time is earlier or later ?
        tC = timeCalendar() 
        tR = tC + round(3600*rnorm(12))
        tR > tC
        
    }
    
    \bold{Lagging}
    
    We can generate suitably lagged and iterated differences:\cr
    
    \code{diff.timeDate} -- Returns suitably lagged and iterated differences.\cr
    
    Examples:
    \preformatted{
        # Monthly Dates 2008 and January 2009:
        tC = c(timeCalendar(2008), timeCalendar(2009)[1])
        
        # Number of monthly days and in total 2008:
        diff(tC)
        sum(as.integer(diff(tC)))
        
    }
    
    \bold{Rounding and Truncating}
    
    Dates and times can be rounded truncated with the help of the functions.
    These functions are useful if we consider lower frequencies then
    seconds, e.g. hourly.\cr
    
    \code{round} -- rounds objects of class 'timeDate', \cr
    \code{trunc} -- truncates objects of class 'timeDate'. \cr
    
    Examples:
    \preformatted{
        # Round the Random Time Stamps to the Nearest Hour:
        tR = tC + round(3600*rnorm(12))
        tR
        round(tR, "h")
        
        # Truncate by Hour or to the Next Full Hour::
        trunc(tR, "h")
        trunc(tR + 3600, "h")
        
    }

    
    \bold{Subsetting}
    
    Subsetting a 'timeDate' is a very important issue in the management of 
    dates and times. Rmetrics offers several functions which are useful in 
    this context:\cr
    
    \code{"["} -- Extracts or replaces subsets from 'timeDate' objects, \cr
    \code{window, cut} -- extract a piece from a 'timeDate' object, \cr
    
    In this context it is also important to know the earlist \code{start}
    and latest \code{to} time stamp together with the total number of
    time stamps. \cr
      
    \code{start} -- extracts the first entry of a 'timeDate' object, \cr
    \code{end} -- extracts the last entry of a 'timeDate' object, \cr
    \code{length} -- returns the length of a 'timeDate' object. \cr 

    Examples:
    \preformatted{
        # Create Monthly Calendar for next year
        tC = timeCalendar(currentYear + 1)
        tC
        
        # The first Quarter - Several Alternative Solutions:
        tC[1:3]
        tC[-(4:length(tC))]
        window(tC, start = tC[1], end = tC[3])
        cut(tC, from = tC[1], to = tC[3])
        tC[tC < tC[4]]
        
        # The Quarterly Series:
        tC[seq(3, 12, by = 3)]
        
        # Start, end and length of 'timeDate' objects
        start(tC)
        end(tC)
        length(tC) 
        %
        % # Extract a special Date:
        % tC = timeCalendar(2008)
        % tC[tC[4]]
        % tC[format(tC[4])]
        % tC[format("2001-01-01"])     ... needs check ?
           
    }
    
    
    Weekdays, weekends, business days, and holidays can easily be
    subsetted using the following functions: \cr
    
    \code{isWeekday} -- tests if a date is a weekday or not, \cr
    \code{isWeekend} -- tests if a date is a weekend day or not, \cr
    \code{isBizday} -- tests if a date is a business day or not, \cr
    \code{isHoliday} -- tests if a date is a holiday day or not. \cr
    
    
    Examples:
    \preformatted{
        # A 'timeDate' Sequence around Easter 2008
        Easter(2008)
        tS <- timeSequence(Easter(2008, -14), Easter(2008, +14))
        tS
        
        # Subset weekdays and business days:
        tW <- tS[isWeekday(tS)]; tW
        getDayOfWeek(tW)
        tB <- tS[isBizday(tS, holidayZURICH())]; tB
        getDayOfWeek(tB)
        
    }
    
    The functions \code{blockStart} and \code{blockEnd} gives us 
    time stamps for equally sized blocks. \cr
    
    
    \code{blockStart} -- Creates start dates for equally sized blocks, \cr
    \code{blockEnd} -- Creates end dates for equally sized blocks. \cr
     
    
    Examples:
    \preformatted{
        # 'timeDate' object for the last 365 days:
        tS = timeSequence(length.out = 360)
        
        # Subset Pointers for blocks of exactly 30 days:
        blockStart(tS, 30)
        blockEnd(tS, 30)
        Sys.timeDate()
        
    }
    
    \bold{Coercions and Transformations}
    
    'timeDate' objects are not living in an isolated world. Coercions and 
    transformations allow 'timeDate' objects to communicate with other
    formatted time stamps. Be aware, in most cases information can be lost
    if the other date.time classes dont't support this functionality.    
    There exist several methods to coerce and transform \code{timeDate}
    objects into other objects.\cr
    

    \code{as.timeDate} -- Implements Use Method, \cr
    \code{as.timeDate.default} -- default Method, \cr
    \code{as.timeDate.POSIXt} -- returns a 'POSIX' object as 'timeDate' object, \cr
    \code{as.timeDate.Date} -- returns a 'POSIX' object as 'timeDate' object. \cr 
    

    \code{as.character.timeDate} -- Returns a 'timeDate' object as 'character' string, \cr
    \code{as.double.timeDate} -- returns a 'timeDate' object as 'numeric' object, \cr
    \code{as.data.frame.timeDate} -- returns a 'timeDate' object as 'data.frame' object, \cr
    \code{as.POSIXct.timeDate} -- returns a 'timeDate' object as 'POSIXct' object, \cr
    \code{as.POSIXlt.timeDate} -- returns a 'timeDate' object as 'POSIXlt' object, \cr
    \code{as.Date.timeDate} -- returns a 'timeDate' object as 'Date' object. \cr 

    
    The user or maintainers of other date/time classes can add his own 
    generic functions like for example \code{as.timeDate.zoo} and 
    \code{as.zoo.timeDate}. \cr
    
    
    \bold{Concatenations and Reorderings}
    
    Sometimes one likes to concatenate and reorder 'timeDate' objects.  
    'timeDate' objects can be concatenated, replicated, sorted and
    resampled, unified and reverted. The generic functions to do that 
    are:\cr

    \code{c} -- Concatenates 'timeDate' objects, \cr
    \code{rep} -- replicates a 'timeDate' object, \cr
    \code{sort} -- sorts a 'timeDate' object, \cr
    \code{sample} -- resamples a 'timeDate' object,\cr
    \code{unique} -- makes a 'timeDate' object unique, \cr
    \code{rev} -- reverts a 'timeDate' object. \cr
    
    NOTE: Concatenating 'timeDate' objects takes care on possible different
    financial centers belongig to each object to be concatenated. In such
    a case, the time stamps will all be transformed to the financial
    center of the first tamp stamp used in conatenation: \cr 

    Examples:
    \preformatted{
        # Concatenate the local timpe stamps to Zurich time ...
        ZH = timeDate("2008-01-01 16:00:00", zone = "GMT", FinCenter = "Zurich")
        NY = timeDate("2008-01-01 18:00:00", zone = "GMT", FinCenter = "NewYork")
        c(ZH, NY)
        c(NY, ZH)
        
        # Rordering:
        tC = timeCalendar(); tC
        tS = sample(tC); tS
        tO = sort(tS); tO
        tV = rev(tO); tV
        tU = unique(c(tS, tS)); tU
        
    }

}


\section{3. Daylight Saving Time and Financial Centers}{

    To each financial center worldwide we can attribute a function 
    with the appropriate Daylight Saving Time Rules. Almost 400
    prototypes are made available through the Olson time zone
    data base. The cities and regions can be listed using the
    command \code{listFinCenter}. The DST rules for a specific
    financial center can be viewed by their name, e.g. \code{Zurich()}.
    Additional financial centers can be added by the user taking care
    of the format specification of the DST functions.\cr
    
    
    \bold{Setting Financial Centers}
    
    
    \bold{List of Financial Centers}
    
    
    Examples:
    \preformatted{
        # What is my current Financial Center - Set it to "GMT" 
        print(myFinCenter)
        myFinCenter = "GMT"
        
        # List the Financial Centers Worldwide:
        listFinCenter()
        
        # List European Financial Centers:
        listFinCenter("Europe")
        
        # Show Daylight Saving Time for Zurich
        Zurich()
        
    }
    
    \bold{DST Rules}
    
}


\section{3. Holidays and Holiday Calendars}{


    Complications begin when we need to know which of the dates business 
    dates and which are weekends and holidays. This results in difficult 
    programming tasks, not difficult in an algorithmic sense, but difficult 
    because it can become tedious to implement the rules of the calendar 
    themselves, for example the date of Easter.

    In the following we briefly summarize the functions which allow to 
    calculate the dates of ecclesiastical and public holidays. With the 
    help of these functions we can create business and holiday calendars. \cr

    
    \bold{Special Dates:}
    
    \code{timeFirstDayInMonth} -- Computes the first day in a given month and year, \cr
    \code{timeLastDayInMonth} -- Computes the last day in a given month and year, \cr
    \code{timeFirstDayInQuarter} -- Computes the first day in a given quarter and year, \cr
    \code{timeLastDayInQuarter} -- Computes the last day in a given quarter and year, \cr
    
    \code{timeNdayOnOrAfter} -- Computes date that is a "on-or-after" n-day, \cr  
    \code{timeNdayOnOrBefore} --b Computes date that is a "on-or-before" n-day, \cr
    
    \code{timeNthNdayInMonth} -- Computes n-th ocurrance of a n-day in year/month, \cr
    \code{timeLastNdayInMonth} -- Computes the last n-day in year/month. \cr
    
    
    \bold{Holidays:}
    
    
    Holidays may have two origins, ecclesiastical and public/federal. The 
    ecclesiastical calendars of Christian churches are based on cycles 
    of moveable and immoveable feasts. Christmas, December 25, is the 
    principal immoveable feast. Easter is the principal moveable feast, 
    and dates of most other moveable feasts are determined with respect 
    to Easter. However, the moveable feasts of the Advent and Epiphany 
    seasons are Sundays reckoned from Christmas and the Feast of the Epiphany, 
    respectively.
    
    Examples:
    \preformatted{
        # List Holidays available in Rmetrics
        listHolidays()
        
        # The date of Easter for the next 5 years:
        Easter(currentYear:(currentYear+5))
        
    }
    
    
    \bold{Holiday Calendars:}
    
    
    \code{holidayZURICH} -- Zurich Business Calendar, \cr
    \code{holidayNYSE} -- NYSE Stock Exchange Holiday Calendar, \cr
    \code{holidayZURICH} -- TSX Holiday Calendar. \cr
    
    
    Thanks to all the Rmetrics users who gave us many additional
    information concerned with local holidays. 

}





\keyword{package}

