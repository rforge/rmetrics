\name{TimeDateClass}

\alias{TimeDateClass}

\alias{rulesFinCenter}
\alias{listFinCenter}

\alias{timeDate}
\alias{timeDate-class}

\alias{timeCalendar}
\alias{timeSequence}
\alias{Sys.timeDate}

\alias{timeLastDayInMonth}
\alias{timeFirstDayInMonth}
\alias{timeLastDayInQuarter}
\alias{timeFirstDayInQuarter}
\alias{timeNdayOnOrAfter}
\alias{timeNdayOnOrBefore}
\alias{timeNthNdayInMonth}
\alias{timeLastNdayInMonth}
\alias{blockStart}
\alias{blockEnd}

\alias{is.timeDate}
\alias{print.timeDate}
\alias{summary.timeDate}
\alias{format.timeDate}

\alias{isWeekday}
\alias{isWeekend}
\alias{isBizday}
\alias{weekDay}

%%

\alias{[.timeDate}
\alias{+.timeDate}
\alias{-.timeDate}
\alias{Ops.timeDate}
\alias{diff.timeDate}
\alias{difftimeDate}

\alias{c.timeDate}
\alias{rep.timeDate}

\alias{start.timeDate}
\alias{end.timeDate}

\alias{modify.timeDate}
\alias{unique.timeDate}
\alias{rev.timeDate}

\alias{as.character.timeDate}
\alias{as.data.frame.timeDate}
\alias{as.POSIXct.timeDate}
\alias{julian.POSIXt}
\alias{julian.timeDate}
\alias{atoms.timeDate}
\alias{months.timeDate}


\title{timeDate Class, Functions and Methods}


\description{
    
    A collection and description of functions and methods
    for managing date and time around the globe for any 
    financial center. The concept allows for dealing
    with time zones, day light saving time and 
    holiday calendars independent of the date and 
    time specifications of the operating system 
    implemented on your computer. This is an important 
    issue especially for R running under Microsoft's 
    Windows operating system.
    \cr
    
    The functions for Financial Centers are:

    \tabular{ll}{   
    \code{rulesFinCenter} \tab Returns DST rules for a financial center, \cr
    \code{listFinCenter} \tab Lists all supported financial centers. }
    
    The functions for the Generation of 'timeDate' Objects are:
    
    \tabular{ll}{
    \code{timeDate} \tab S4: Creates 'timeDate' object from a character vector, \cr
    \code{timeCalendar} \tab S4: Creates 'timeDate' object from calendar atoms, \cr
    \code{timeSequence} \tab S4: Creates regularly spaced object of class 'timeDate', \cr
    \code{Sys.timeDate} \tab Returns system time as an object of class 'timeDate'. }         

    The functions for Special Monthly 'timeDate' Sequences are:
    
    \tabular{ll}{
    \code{timeFirstDayInMonth} \tab Computes the first day in a given month and year, \cr
    \code{timeLastDayInMonth} \tab Computes the last day in a given month and year, \cr
    \code{timeFirstDayInQuarter} \tab Computes the first day in a given quarter and year, \cr
    \code{timeLastDayInQuarter} \tab Computes the last day in a given quarter and year, \cr
    \code{timeNdayOnOrAfter} \tab Computes date that is a "on-or-after" n-day, \cr  
    \code{timeNdayOnOrBefore} \tab Computes date that is a "on-or-before" n-day, \cr
    \code{timeNthNdayInMonth} \tab Computes n-th ocurrance of a n-day in year/month, \cr
    \code{timeLastNdayInMonth} \tab Computes the last n-day in year/month \cr
    \code{blockStart} \tab Computes start dates for numeric blocks of dates, \cr
    \code{blockEnd} \tab Computes end dates for numeric blocks of dates. }           
         
    The functions for the Representation of 'timedate' Objects are:
    
    \tabular{ll}{       
    \code{is.timeDate} \tab Checks if the object is of class 'timeDate', \cr
    \code{print.timeDate} \tab S3: Prints 'timeDate', 'FinCenter' and 'Data' Slot, \cr
    \code{summary.timeDate} \tab S3: Summarizes details of a 'timeDate' object, \cr
    \code{format.timeDate} \tab Formats 'timeDate' as ISO conform character string. }
    
    Additional Calendar Date Functions:
    
    \tabular{ll}{ 
    \code{isWeekday} \tab Tests if a date is a weekday or not, \cr
    \code{isWeekend} \tab Tests if a date falls on a weekend or not, \cr
    \code{isBizday} \tab Tests if a date is a business day or not, \cr
    \code{weekDay} \tab Returns the day of the week.}
    
     Methods for Math Operations with 'timeDate' Objects are:

    \tabular{ll}{   
    \code{[.timeDate} \tab Extracts, replaces subsets from 'timeDate' objects, \cr
    \code{+.timeDate} \tab Performs arithmetic + ops on 'timeDate' objects, \cr
    \code{-.timeDate} \tab  Performs arithmetic - ops on 'timeDate' objects, \cr
    \code{Ops.timeDate} \tab 'Ops' generic functions for 'timeDate' objects, \cr
    \code{diff.timeDate} \tab Returns suitably lagged and iterated differences, \cr
    \code{difftimeDate} \tab Returns a difference of two 'timeDate' objects, \cr
    \code{c.timeDate} \tab Concatenates objects of class 'timeDate', \cr
    \code{rep.timeDate} \tab Replicates objects of class 'timeDate', \cr
    \code{start.timeDate} \tab Extracts the first object of a 'timeDate' vector, \cr
    \code{end.timeDate} \tab Extracts the last object of a 'timeDate' vector, \cr
    \code{modify.timeDate} \tab Sorts, rounds or truncates a 'timeDate' vector, \cr
    \code{unique.timeDate} \tab Remove duplicates from a 'timeDate' vector, \cr
    \code{rev.timeDate} \tab Reverts  a 'timeDate' vector object. }          
            
    Mthods for Transformations of 'timeDate' Objects are:
        
    \tabular{ll}{       
    \code{as.character.timeDate} \tab Returns a 'timeDate' object as character string, \cr
    \code{as.data.frame.timeDate} \tab Returns a 'timeDate' object as data frame, \cr
    \code{as.POSIXct.timeDate} \tab Returns a 'timeDate' object as POSIXct object, \cr
    \code{as.POSIXlt} \tab As POSIXlt, Use Method (Overwrite), \cr
    \code{as.POSIXlt.default} \tab Default method for POSIXlt, \cr
    \code{julian.timeDate} \tab Returns Julian day counts since 1970-01-01, \cr
    \code{julian.POSIXt} \tab A Julian Patch, \cr
    \code{atoms.timeDate} \tab Returns date/time atoms from a 'timeDate' object, \cr
    \code{atoms} \tab Extract atoms, Use Method, \cr
    \code{atoms.default} \tab Default method for atoms, \cr
    \code{months.timeDate} \tab Extract months atom from a 'timeDate' object. }
    
        
}


\usage{
rulesFinCenter(FinCenter = myFinCenter)
listFinCenter(pattern = "*")

timeDate(charvec, format = NULL, zone = myFinCenter, FinCenter = myFinCenter) 
timeCalendar(y = currentYear, m = 1:12, d = NULL, h = NULL, min = NULL, 
    s = NULL, zone = myFinCenter, FinCenter = myFinCenter)
timeSequence(from = "2004-01-01", to = format(Sys.time(), "\%Y-\%m-\%d"), by = "day", 
    length.out = NULL, format = "", zone = myFinCenter, FinCenter = myFinCenter)
Sys.timeDate(FinCenter = myFinCenter) 

timeFirstDayInMonth(charvec, format = "\%Y-\%m-\%d", FinCenter = "GMT")
timeLastDayInMonth(charvec, format = "\%Y-\%m-\%d", FinCenter = "GMT")
timeFirstDayInQuarter(charvec, format = "\%Y-\%m-\%d", FinCenter = "GMT")
timeLastDayInQuarter(charvec, format = "\%Y-\%m-\%d", FinCenter = "GMT")

timeNdayOnOrAfter(charvec, nday = 1, format = "\%Y-\%m-\%d", 
    FinCenter = "GMT")
timeNdayOnOrBefore(charvec, nday = 1, format = "\%Y-\%m-\%d",   
    FinCenter = "GMT")
timeNthNdayInMonth(charvec, nday = 1, nth = 1, format = "\%Y-\%m-\%d", 
    FinCenter = "GMT")
timeLastNdayInMonth(charvec, nday = 1, format = "\%Y-\%m-\%d", 
    FinCenter = "GMT")
    
blockStart(x, block = 20)
blockEnd(x, block = 20)

is.timeDate(object) 
\method{print}{timeDate}(x, \dots)
\method{summary}{timeDate}(object, \dots)
\method{format}{timeDate}(x, \dots)

isWeekday(x)
isWeekend(x)
isBizday(x, holidays = holiday.NYSE())
weekDay(x)

\method{[}{timeDate}(x, \dots, drop = TRUE)
\method{+}{timeDate}(e1, e2)
\method{-}{timeDate}(e1, e2)
\method{Ops}{timeDate}(e1, e2)
\method{diff}{timeDate}(x, lag = 1, differences = 1, \dots)
difftimeDate(time1, time2, 
    units = c("auto", "secs", "mins", "hours", "days", "weeks"))

\method{c}{timeDate}(\dots, recursive = FALSE)
\method{rep}{timeDate}(x, times, \dots)
\method{start}{timeDate}(x, \dots)
\method{end}{timeDate}(x, \dots)
\method{modify}{timeDate}(x, method = c("sort", "round", "trunc"),
    units = c("secs", "mins", "hours", "days"))
\method{unique}{timeDate}(x, incomparables = FALSE, \dots)
\method{rev}{timeDate}(x)

\method{as.character}{timeDate}(x, \dots)
\method{as.data.frame}{timeDate}(x, \dots)
\method{as.POSIXct}{timeDate}(x, tz = "")
\method{julian}{timeDate}(x, \dots)
\method{atoms}{timeDate}(x, \dots)
\method{months}{timeDate}(x, abbreviate = NULL)
}


\arguments{
  
    \item{abbreviate}{
        [months] - \cr
        currently not used.
        }
    \item{block}{
        an integer value specifying the length in number of records
        for numerically sized blocks of dates.
        }
    \item{by}{
        a character string, containing one of "sec", "min",
        "hour", "day", "week", "month" or "year".
        This can optionally be preceded by an integer and a
        space, or followed by "s". 
        }
    \item{charvec}{
        a character vector of dates and times.
        }
    \item{differences}{
        [lag] - \cr
        an integer indicating the order of the difference.
        }
    \item{drop}{
        ["["] - \cr
        a logical flag, by default \code{TRUE}.
        }
    \item{e1, e2}{
        ["+"][["-"]["Ops"] - \cr
        usually objects of class \code{timeDate}, in the case of
        addition and subtraction \code{e2} may be of class
        \code{numeric}.
        }
    \item{FinCenter}{
        a character with the the location of the  
        financial center named as "continent/city". 
        }
    \item{format}{
        the format specification of the input character vector.
        }
    \item{from, to}{
        starting date, required, and end date, ptional. If supplied 
        \code{to} must be after \code{from}.
        }
    \item{h, min, s}{
        hours of the days (0-23), defaults are 0,
        minutes of the days (0-59), defaults are 0, and 
        seconds of the days (0-59), defaults are 0. 
        }
    \item{holidays}{
        [isBizday] - \cr
        holiday dates from a holiday calendar. An object of class
        \code{timeDate}.
        }
    \item{incomparables}{
        [unique] - \cr
        not used.
        }
    \item{lag}{
        [lag] - \cr
        an integer indicating which lag to use.  
        }
    \item{length.out}{
        length.out integer, optional. Desired length  
        of the sequence, if specified "to" will be ignored.
        }
    \item{nday}{
        an integer vector with entries ranging from 
        \code{0} (Sunday) to \code{6} (Saturday).
        }
    \item{method}{
        [modify] - \cr
        a character string defining the modification method, one of
        \code{"sort"}, \code{"round"}, or \code{"trunc"}. 
        }
    \item{nth}{
        an integer vector numbering the n-th occurence.
        }     
    \item{object}{
        [is.timeData][summary] - \cr
        an object of class \code{timeDate}.
        }
    \item{pattern}{
        a pattern character string which can be recognized
        by the \code{grep} function. Wild cards are allowed.
        }
    \item{recursive}{
        [c] - \cr
        a logical. If recursive  is set to \code{TRUE}, the function 
        recursively descends through lists combining all their elements 
        into a vector. 
        }
    \item{time1, time2}{
        [difftime] - \cr
        two objects objects of class \code{timeDate}.
        }
    \item{times}{
        [rep] - \cr
        an optional non-negative integer. A vector giving the number of 
        times to repeat each element if of length \code{length(x)}, or 
        to repeat the whole vector if of length \code{1}.
        }
    \item{tz}{
        inputs the time zone to POSIX objects, i.e. the time zone, 
        \code{zone}, or financial center string, \code{FinCenter}, 
        as used by \code{timeDate} objects.
        }
    \item{units}{
        a character string denoting the date/time units in which the 
        results are desired.
        }
    \item{x}{
        [isWeekday][isWeekend][isBizday][weekDay] - \cr
        an object of class \code{timeDate}.
        \cr
        [format][print] - \cr
        an object of class \code{timeDate}.
        }
    \item{y, m, d}{
        calendar years (e.g. 1997), defaults are 1960,
        calendar months (1-12), defaults are 1, and 
        calendar days (1-31), defaults are 1. 
        }
    \item{zone}{
        the time zone or financial center where the data were recorded.
        }   
    \item{\dots}{
        arguments passed to other methods.
        }
       
}


\value{

    \code{rulesFinCenter}\cr
    \code{listFinCenter}
    \cr
    the first function returns a printed list of DST rules, the second
    lists time zones available in the database.
    \cr
    
    \code{timedate}\cr
    \code{timeCalendar}\cr
    \code{timeSequence}
    \cr
    return a S4 object of class 'timeDate'.
    \cr
    
    \code{Sys.timeDate}
    \cr
    returns the system time as an object of class 'timeDate'.
    \cr
    
    \code{timeLastDayInMonth}\cr
    \code{timeNdayOnOrAfter}\cr
    \code{timeNdayOnOrBefore}\cr
    \code{timeNthNdayInMonth}\cr
    \code{timeLastNdayInMonth}
    \cr
    return \code{timeDate} objects on special dates. For the function 
    \code{timeLastDayInMonth} the last day in a given month and year 
    will be returned, for \code{timeNdayOnOrAfter} the date in the 
    specified month that is a n-day  on or after a given date, for 
    \code{timeNdayOnOrBefore} the date that is a n-day on or before a 
    given date, for \code{timeNthNdayInMonth} the \code{nth} ocurrance 
    of a \code{nday} (nth = 1,...,5) in \code{year}, \code{month}, and
    for {timeLastNdayInMonth} the last \code{nday} in \code{year},
    \code{month}. 
    \cr
    
    \code{is.timeDate}
    \cr
    returns \code{TRUE} or \code{FALSE} depending on whether its
    argument is of \code{timeDate} type or not.
    \cr
    
    \code{isWeekday}\cr
    \code{isWeekend}\cr
    \code{isBizday}\cr
    \code{weekDay}
    \cr
    The first thres functions return logical vectors indicating
    if a date is a weekday, a weekend day, or a business.day. The
    function \code{weekDay} returns a three letter character string
    with the names of the day of the week.
    \cr
    
    \emph{Print Method:}
    \cr
    prints the financial center and date and time vector for a 
    \code{timeDate} object.
    \cr
    
    \emph{Summary Method:}
    \cr
    returns a summary report of the details of a \code{timeDate} 
    object. This includes the starting and end date, the number of 
    dates the format and financial center in use.
    \cr
    
    \emph{Format Method:}
    \cr
    returns an ISO conform formatted character string.
    \cr

    \code{"["}
    \cr
    returns a subset from a \code{timeDate} object.
    \cr
    
    \code{"+"}\cr
    \code{"-"}
    \cr
    the plus operator \code{"+"} performs arithmetic "+" operation on 
    \code{timeDate} objects, and 
    the minus operator \code{"-"} returns a \code{difftime} object if 
    both arguments \code{e1} and \code{e2} are \code{timeDate} objects, 
    or returns a \code{timeDate} object \code{e2} seconds earlier than 
    \code{e1}.
    \cr
    
    \code{Ops.timeDate}
    \cr
    returns the \code{Ops} grouped object.
    \cr
    
    \code{diff}\cr
    \code{difftimeDate}
    \cr
    For the first function, \code{diff.timeDate}, if \code{x} is a vector 
    of length \code{n} and \code{differences=1}, then the computed result 
    is equal to the successive differences \code{x[(1+lag):n] - x[1:(n-lag)]}. 
    If \code{difference} is larger than one this algorithm is applied
    recursively to \code{x}. Note that the returned value is a vector 
    which is shorter than \code{x}.
    The second function, \code{difftimeDate}, takes a difference of two 
    \code{timeDate} objects and returns an object of class \code{difftime} 
    with an attribute indicating the units.
    \cr
    
    \code{c}\cr
    \code{rep}
    \cr
    \code{c} returns all its arguments to be coerced to a \code{timeDate} 
    object which is the type of the returned value, and 
    \code{rep} returns a vector of repeated elements belonging to the 
    same class as \code{x}.
    \cr
    
    \code{start}\cr
    \code{end}
    \cr
    return from \code{x} the earliest or latest entry as an object of 
    class \code{timeDate}, respectively.
    \cr
    
    \code{modify}\cr
    \code{rev}
    \cr
    \code{modify} returns \code{x} as a sorted, rounded or truncated 
    object of the same class, depending on the \code{method} selection, 
    and
    \code{rev} returns \code{x} as a \code{timeDate} object in reversed 
    order.
    \cr
    
    \code{as.character}\cr
    \code{as.data.frame}
    \cr
    return a \code{timeDate} object trnasformed into a character or a
    data frame formatted object.
    \cr
       
    \code{as.POSIXct}
    \cr
    return a \code{timeDate} object trnasformed into a POSIX type
    formatted object.
    \cr
    
    \code{julian}
    \cr
    return a \code{timeDate} object as a Julian count.
    \cr
    
    \code{atoms}\cr
    \code{months.timeDate}
    \cr
    extrac from a \code{timeDate} object the calendar atoms, i.e,
    the year, month, day, and optionally hour, minute and second.
    
}


\details{

    For the management of chronological objects under R three concepts 
    are available: The first is the implementation of date and time in R's 
    \code{chron} package neglecting locals, time zones and day light saving 
    times. This approach is appropriate for economic time series. The second 
    approach, available in R's base package implements the POSIX standard 
    to date and time objects, named \code{"POSIXt"}. Unfortunately, the 
    representation of these objects is operating system dependent and 
    especially under MS Windows several problems appear in the management 
    of time zones and day light saving times. Here we present a solution
    to overcome these difficulties with POSIX objects and introduce a 
    new S4 class of 'timeDate' objects which allow for powerful methods 
    to represent dates and times in different financial centers around 
    the world. Many of the basic functionalities of these objects are in 
    common with S-Plus' \code{timeDate} objects and thus many of your 
    privately written functions for \code{FinMetrics} may also be used 
    within R's environment.

    A major difference is the time zone concept which is replaced by the
    "Financial Center" concept. The \code{FinCenter} character variable
    specifies where you are living and at which financial center you are
    working. With the variable \code{myFinCenter} you can overwrite the
    default setting with your personal settings.  
    With the specification of the \code{FinCenter} your system knows what
    rules rules for day light saving times should be applied, what is your
    holiday calendar, what are your interest rate conventions. (Not all
    specifications are already implemented.) Many other 
    aspects can be easily accessed when a financial center is named. So we 
    can distinguish between Frankfurt and Zurich, which both belong to the 
    same time zone, but differed in DST changes in the eighties and have
    different holiday calendars. Futhermore, since the underlying time 
    refers to "GMT" and DST rules and all other information is available 
    in local (ASCII) databases, we are sure, that R delivers with such a 
    date/time concept on every computer independent of the operating system 
    in use, identical results. 

    Another important feature of the \code{"timeDate"} concept used here is 
    the fact that we don't rely on American or European ways to write dates.
    We use consequently the ISO-8601 standard for date and time notations.
    \cr
    
    
    \bold{Financial Centers}
    \cr\cr
    There are two functions concerned with the financial centers. The 
    first, \code{rulesFinCenter}, lists the daylight saving rules for a 
    selected financial center, and the second, \code{listFinCenter}, 
    lists all centers available in the database. There is no dependency 
    on the POSIX implementation of your operating system because all 
    time zone and day light saving time information is stored locally 
    in ASCII files. It is important to say, that the \code{TZ} environment 
    variable must set to \code{"GMT"} in your system environment that 
    there are no conflicts with the POSIX time zone management.
       
    Through the definition of financial centers it becomes possible to 
    introduce in the future a specification structure for financial
    centers, which includes further information for a center like
    holiday calendars, currency and interest rate conventions or many
    others.
    \cr
    
    
    \bold{Generation of timeDate Objects}
    \cr\cr
    We have defined a 'timeDate' class which is in many aspects similar
    to the S-Plus class with the same name, but has also some important
    advantageous differeneces. The S4 class has four Slots, the \code{Data} 
    slot which holds date and time as 'POSIXlt' objects in the standard
    ISO-8601 format, the \code{Dim} slot which gives the dimension
    of the data object, the \code{format} specification slot and the
    \code{FinCenter} slot which holds the name of the financial center. 
    
    Three functions allow to cgenerate date/time objects: \code{timeDate} 
    from character vectors, \code{timeCalendar} from date and time atoms, 
    and \code{timeSequence} from a "from/to" or from a "from/length" sequence 
    specification. Note, time zone transformations are easily handled by
    by the \code{timeDate} functions which can also take \code{timeDate} 
    and \code{POSIXt} objects as inputs, while transforming them between 
    financial centers and/or time zones specified by the arguments \code{zone} 
    and \code{FinCenter}. Finally the function \code{Sys.timeDate} returns 
    current system time in form of a \code{timeDate} object.
    \cr
    
    
    \bold{Special Monthly timeDate Sequences}
    \cr\cr
    We have implemented five functions to generate special monthly 
    sequences. These are functions to compute the last day in a given 
    month and year, to compute the dates in a month that is a n-day on or 
    after a given date, to compute the dates in a month that is a n-day 
    on or before a specified date, to compute the n-th ocurrances of a 
    n-day for a specified year/month vectors, and finally to compute 
    the last n-day for a specified year/month value or vector. n-days
    are numbered from 0 to 6 where 0 correspond to the Sunday and 6 to 
    the Saturday.
    \cr
    
    
    \bold{Tests and Representation of timeDate Objects:}
    \cr\cr
    We have implemented four methods to test and represent
    \code{timeDate} objects. The method \code{is.timeDate} checks if 
    a given object is of class \code{"timeDate"}. The print method 
    returns the date/time in square \code{"[]"} brackets to dsitinguish
    the output from other date and time objects. On top of the date and
    time output the name of the \code{FinCenter} is printed. The summary 
    method returns a printed report with information about the 
    \code{"timeDate"} object. Finally, the format methods allows to 
    transform objects into a ISO conform formatted character strings.
    
    
    \bold{Mathematical Operations:}
    \cr\cr
    This is a collection of S3 methods for objects of class \code{timeDate}
    to perform mathematical operations. Included are methods to extracts 
    or replace subsets from \code{timeDate} objects, to perform arithmetic 
    \code{"+"} and \code{"-"} operations, to group 'Ops' generic functions, 
    to return suitably lagged and iterated differences, to return differences 
    of two \code{timeDate} objects, to concatenate objects, to replicate 
    objects, to rounds objects, to truncates objects, to extract the first 
    or last entry of a vector, to sort the objects of the elements of a 
    date/time vector, and to revert \code{timeDate} vector objects.
    \cr
    
    
    \bold{Transformation of Objects:}
    \cr\cr
    This is a collection of S3 methods for objects of class \code{timeDate}
    to transform those objects between different representations.
    Included are methods to transform \code{timeDate} objects to character 
    strings, to data frames, to POSIXct or POSIXlt objects, to Julian
    counts, to extract date/time atoms from calendar dates, and to 
    extract the months atom from a \code{timeDate} object.
   

}


\note{

    These functions were written for Rmetrics users using R and Rmetrics
    under Microsoft's Windows XP operating system where time zones, 
    daylight saving times and holiday calendars are not or insuffeciently 
    supported. The functions are untested for other system environments, 
    but may be used.
    
    The usage of the Ical Library and the introduction of the FinCenter
    concept was originally develloped for R Version 1.5. The 
    \code{timeDate} and \code{timeSeries} objects were added for 
    R Version 1.8.1. Minor changes were made to adapt the functions 
    for R Version 1.9.1. As a consequence, newer concepts like the 
    \code{Date} objects were not yet considered and included in this 
    collection of date and time concepts.
    
    Note, the date/time conversion from an arbitry timezone to GMT cannot
    be unique, since date/time objects appear twice during the hour
    when DST changes. A bookkeeping which takes care if DST is effective 
    or not is not yet included. However, in most applications this is 
    not important since the markets are closed on weekends, especially 
    at times when DST usually changes. It is planned for the future to
    implement the DST change properly.
    
    The ISO-8601 midnight standard has been implemented. Note, that for
    example "2005-01-01 24:00:00" is a valid date/time string.
    
    Beside the examples given in the manual pages additional demo files
    are available with much more examples including also those from the 
    book of Zivot and Wang.

}


\references{

Bateman R., (2000);
    \emph{Time Functionality in the Standard C Library},
    Novell AppNotes, September 2000 Issue, 73--85.

ISO-8601, (1988);
    \emph{Data Elements and Interchange Formats -
    Information Interchange, Representation of Dates and Time},
    International Organization for Standardization,
    Reference Number ISO 8601, 14 pages.

James D.A., Pregibon D. (1992),
    \emph{Chronological Objects for Data Analysis},
    Reprint.
    
Ripley B.D., Hornik K. (2001);
    \emph{Date-Time Classes},
    R-News, Vol. 1/2 June 2001, 8--12.
    
Zivot, E., Wang J. (2003);
    \emph{Modeling Financial Time Series with S-Plus},
    Springer, New-York.
}


\seealso{

    We recommend to inspect the help pages for the POSIX time and date 
    class, \code{?Dates},  and the help pages from the contributed R packages
    \code{chron} and \code{date}.
}


\examples{
## SOURCE("fCalendar.023A-TimeDateClass")

## c -
   xmpCalendar("\nStart: Create Character Vectors > ") 
   dts = c("1989-09-28", "2001-01-15", "2004-08-30", "1990-02-09")
   tms = c(  "23:12:55",   "10:34:02",   "08:30:00",   "11:18:23")
   dts; tms
   
## timeDate -
   xmpCalendar("\nNext: Construct timeDate Object from Character Vector > ") 
   timeDate(dts, format = "\%Y-\%m-\%d", FinCenter = "GMT" )
   timeDate(dts, format = "\%Y-\%m-\%d", FinCenter = "Europe/Zurich") 
   timeDate(paste(dts, tms), format = "\%Y-\%m-\%d \%H:\%M:\%S", 
     zone = "GMT", FinCenter = "GMT")
   timeDate(paste(dts, tms), format = "\%Y-\%m-\%d \%H:\%M:\%S", 
     zone = "Europe/Zurich", FinCenter = "Europe/Zurich")
   timeDate(paste(dts, tms), format = "\%Y-\%m-\%d \%H:\%M:\%S", 
     zone = "GMT", FinCenter = "Europe/Zurich") 
    
## timeCalendar - 
   xmpCalendar("\nNext: Create a timeDate Object From Calendar Atoms > ")
   timeCalendar( m = c(9, 1, 8, 2), d = c(28, 15, 30, 9), 
     y = c(1989, 2001, 2004, 1990), FinCenter = "GMT") 
   timeCalendar(m = c(9, 1, 8, 2), d = c(28, 15, 30, 9), 
     y = c(1989, 2001, 2004, 1990), FinCenter = "Europe/Zurich")
   timeCalendar(h = c(9, 14), min = c(15, 23)) 
  
## timeSequence - 
   xmpCalendar("\nNext: Create a Regularly Spaced timeDate Objects > ")
   timeSequence(from = "2004-03-12", to = "2004-04-11", 
        format = "\%Y-\%m-\%d", FinCenter = "GMT")  
   timeSequence(from = "2004-03-12", to = "2004-04-11", 
        format = "\%Y-\%m-\%d", FinCenter = "Europe/Zurich")
   
## timeDate - 
   xmpCalendar("\nNext: Note, ISO and American Formats are Auto-Detected > ")
   timeDate("2004-12-11", FinCenter = "GMT")    
   timeDate("12/11/2004", FinCenter = "GMT")   
  
## SOURCE("fBasics.A0-SPlusCompatibility")
## SOURCE("fBasics.E1-TimeDateClass")
## SOURCE("fBasics.E2-TimeDatemethods")
## SOURCE("fBasics.Z1-BasicsTools")

## c -
   xmpCalendar("\nStart: Create Character Vectors > ") 
   dts = c("1989-09-28", "2001-01-15", "2004-08-30", "1990-02-09")
   tms = c(  "23:12:55",   "10:34:02",   "08:30:00",   "11:18:23")
   dts; tms
   
## "+/-" - 
   xmpCalendar("\nStart: Add One Day to a Given timeDate Object > ")
   GMT = timeDate(dts, FinCenter = "GMT")
   ZUR = timeDate(dts, FinCenter = "Europe/Zurich")
   GMT + 24*3600
   ZUR[2] - ZUR[1]
  
## "[" - 
   xmpCalendar("\nNext: Subsets from and Lops for timeDate Objects > ")
   GMT[GMT < GMT[2]]
   ZUR[ZUR < ZUR[3]] == ZUR[1:3]

## diff - 
   xmpCalendar("\nNext: Suitably Lagged and Iterated Differences > ")
   diff(GMT)
   diff(GMT, lag = 2)
   diff(GMT, lag = 1, diff = 2)
   difftimeDate(GMT[1:2], GMT[-(1:2)])
   
## c | rep - 
   xmpCalendar("\nNext: Concatenate and Replicate timeDate Objects > ") 
   c(GMT[1:2], ZUR[1:2])
   c(ZUR[1:2], GMT[1:2])
   rep(ZUR[2], times = 3)
   rep(ZUR[2:3], times = 2) 

## round | truncate -
   xmpCalendar("\nNext: Round and Truncate timeDate Objects > ")
   modify(GMT, "round", "days")
   modify(ZUR, "round", "days")
   modify(GMT, "trunc", "days")
   modify(ZUR, "trunc", "days")

## start | end | sort - 
   xmpCalendar("\nNext: Extract First/Last, Sort timeDate Objects > ")
   c(start(ZUR), end(ZUR))
   modify(ZUR, "sort") 

## as -
   xmpCalendar("\nNext: Convert timeDate Objects to Other Objects > ")
   as.character(ZUR)
   as.data.frame(ZUR)
   as.POSIXct(ZUR)
   
## julian - 
   xmpCalendar("\nNext: Julian Time in Days Since 1970-01-01 > ")
   julian(ZUR)  
   as.integer(julian(ZUR))
   julian(ZUR, "days")  
   as.integer(julian(ZUR, "days"))
    
## atoms - 
   xmpCalendar("\nNext: Atoms from a timeDate Object > ")
   atoms(ZUR)
   atoms(ZUR)[,3]
   atoms(ZUR)[, "d"] 
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\keyword{chron}

