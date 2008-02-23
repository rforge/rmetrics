\name{TimeDateClass}


\title{timeDate Class}


\alias{myUnits}

\alias{show.timeDate}
\alias{show,timeDate-method}


\alias{plot.timeDate}
\alias{points.timeDate}
\alias{lines.timeDate}

\alias{format.timeDate}


\description{

    A collection and description of functions and methods
    for managing date and time around the globe for any
    financial center. The concept allows for dealing
    with several time zones at the same time, day light
    saving time and holiday calendars independently of the
    date and time specifications of the operating system
    implemented on a specific computer.
    \cr

    The functions to represent 'timeDate' objects:
    \tabular{ll}{
    \code{print.timeDate} \tab Prints 'timeDate' Object, \cr
    \code{plot.timeDate} \tab Plots with GMT 'timeDate' x-axis, \cr
    \code{points.timeDate} \tab Adds points to a 'timeDate' plot, \cr
    \code{lines.timeDate} \tab Adds lines to a 'timeDate' plot, \cr
    \code{format.timeDate} \tab Formats 'timeDate' as ISO conform character string, }

}


\usage{
myUnits
currentYear

\method{plot}{timeDate}(x, y, \dots)

\method{format}{timeDate}(x, \dots)
}


\arguments{

 
    \item{x}{
        [format][print][plot] - \cr
        an object of class \code{timeDate}.
        }
    \item{y}{
        a numeric vector.
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
    return a S4 object of \code{\link{class}} \code{"timeDate"}.
    \cr

    \code{Sys.timeDate}
    returns the system time as class \code{"timeDate"} object.
    \cr

%     \code{is.timeDate}
%     \cr
%     returns \code{TRUE} or \code{FALSE} depending on whether its
%     argument is of \code{timeDate} type or not.
%     \cr

    \emph{print}
    \cr
    prints the financial center and date and time vector for a
    \code{timeDate} object.
    \cr

    \emph{summary}
    \cr
    returns a summary report of the details of a \code{timeDate}
    object. This includes the starting and end date, the number of
    dates the format and financial center in use.
    \cr

    \emph{format}
    \cr
    returns an ISO conformly formatted character string.
    \cr

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


    \bold{Generation of timeDate Objects}
    \cr\cr
    We have defined a 'timeDate' class which is in many aspects similar
    to the S-Plus class with the same name, but has also some important
    advantageous differeneces. The S4 class has four Slots, the \code{Data}
    slot which holds date and time as 'POSIXct' objects in the standard
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

    originally, these functions were written for Rmetrics users using R and
    Rmetrics under Microsoft's Windows XP operating system where time zones,
    daylight saving times and holiday calendars are not or insuffeciently
    supported. The functions are untested for other system environments,
    but may be used.

    The usage of the Ical Library and the introduction of the FinCenter
    concept was originally develloped for R Version 1.5. The
    \code{timeDate} and \code{timeSeries} objects were added for
    R Version 1.8.1. Minor changes were made to adapt the functions
    for R Version 1.9.1. As a consequence, newer concepts like the
    \code{Date} objects were not yet considered and included in this
    collection of date and time concepts. With R Version 2.3.0 a major
    update has been made adding many new generic functions and renaming
    a few already existing functions, please be aware of this.

    Note, the date/time conversion from an arbitry timezone to GMT cannot
    be unique, since date/time objects appear twice during the hour
    when DST changes. A bookkeeping which takes care if DST is effective
    or not is not yet included. However, in most applications this is
    not necessary since the markets are closed on weekends, especially
    at times when DST usually changes. It is planned for the future to
    implement the DST change properly.

    The ISO-8601 midnight standard has been implemented. Note, that for
    example "2005-01-01 24:00:00" is a valid date/time string.

    Beside the examples given in the manual pages additional demo and
    test files are available with much more examples including also those
    from the book of Zivot and Wang (2003).

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

    \code{timeDateCoercion}, \code{timeDateSpecDates}

    We also recommend to inspect the help pages for the POSIX time and date
    class, \code{\link{Dates}}, and the help pages from the contributed R packages
    \code{chron} and \code{date}.
}


\author{
    Diethelm Wuertz for the Rmetrics \R-port; some improvements: % bug fixes 
    Martin Maechler.
}

\examples{
## timeDate -

   Sys.timeDate()           # direct
   timeDate(Sys.time())     # transformed from "POSIX(c)t"
   
   # Local Time in Zurich
   timeDate(Sys.time(), FinCenter = "Zurich")  

   # Character Vector Strings:
   dts = c("1989-09-28", "2001-01-15", "2004-08-30", "1990-02-09")
   tms = c(  "23:12:55",   "10:34:02",   "08:30:00",   "11:18:23")

   dts ; tms

   t1 <- timeDate(dts, format = "\%Y-\%m-\%d", FinCenter = "GMT" )
   t1
   stopifnot(identical(t1, timeDate(dts, FinC = "GMT"))) # auto-format
   
   timeDate(dts, format = "\%Y-\%m-\%d", FinCenter = "Europe/Zurich")
   
   timeDate(paste(dts, tms), format = "\%Y-\%m-\%d \%H:\%M:\%S",
            zone = "GMT", FinCenter = "GMT")
   
   timeDate(paste(dts, tms), zone = "Europe/Zurich", FinCenter = "Europe/Zurich")
   
   timeDate(paste(dts, tms), format = "\%Y-\%m-\%d \%H:\%M:\%S",
            zone = "GMT", FinCenter = "Europe/Zurich")

   ## non standard format:
   timeDate(paste(20:31, "03.2005", sep="."), format = "\%d.\%m.\%Y")


   # Note, ISO and American Formats are Auto-Detected:
   timeDate("2004-12-31", FinCenter = "GMT")
   timeDate("12/11/2004", FinCenter = "GMT")
   timeDate("1/31/2004") # auto-detect American format

   ## from POSIX?t, and using NAs
   lsec <- as.POSIXlt(.leap.seconds) ; lsec[c(2,4:6)] <- NA
   timeDate(lsec)

   dtms <- paste(dts,tms)
   dtms[2:3] <- NA
   timeDate(dtms, FinCenter = "Europe/Zurich")# but in GMT

% would need  change in R :
%    tms[3] <- dts[2] <- NA
%    timeDate(paste(dts,tms), FinCenter = "Europe/Zurich")# but in GMT


## timeCalendar -
   
   currentYear
   timeCalendar() # 12 months of current year
   timeCalendar(m = c(9, 1, 8, 2), d = c(28, 15, 30, 9),
                y = c(1989, 2001, 2004, 1990), FinCenter = "GMT")
   timeCalendar(m = c(9, 1, 8, 2), d = c(28, 15, 30, 9),
                y = c(1989, 2001, 2004, 1990), FinCenter = "Europe/Zurich")
   timeCalendar(h = c(9, 14), min = c(15, 23))

## timeSequence -
   
   timeSequence(from = "2004-03-12", to = "2004-04-11",
        format = "\%Y-\%m-\%d", FinCenter = "GMT")
   timeSequence(from = "2004-03-12", to = "2004-04-11",
        format = "\%Y-\%m-\%d", FinCenter = "Europe/Zurich")

## print | summary | format -

   tC = timeCalendar()
   print(tC)
   summary(tC)
   format(tC)
}


\keyword{chron}

