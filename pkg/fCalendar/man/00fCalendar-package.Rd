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
    management together with the management of public and ecclestical
    holidays.
    
    The 'timeDate' class fulfills the conventions of the ISO 8601 
    standard as well as of the ANSI C and POSIX standards. Beyond
    these standards Rmetrics has added the "Financial Center" concept
    which allows to handle data records collected in different time 
    zones and mix them up to have always the right time stamps
    with respect to your personal financial center, or to the GMT
    reference time. It can thus also handle time stamps from historical 
    data records from the same time zone, even if the financial 
    centers changed day light saving times at different calendar
    dates.
    
    Evenmore 'timeDate' is almost compatible with the 'timeDate'
    class in Insightful's 'timeDate' class. Thus if you move between 
    the two worlds of R and SPlus, you have not to rewrite your
    code, this is very important mainly for business applications.
    
    The 'timeDate' class offers not only date and time functionality
    it als offers very sophisticated calendar manipulations concerned
    with business days, weekends, and public and ecclestical holidays. 
    
    The information given in this help page is presented in three
    major chapters:
       
    1. S4 'timeDate' Class and Functions\cr
    2. Daylight Saving Time and Financial Centers\cr
    3. Holidays and Holiday Calendars
 
}  


\section{1. Working with 'timeDate' Objects}{


    % --------------------------------------------------------------------------
    \bold{Representation of S4 'timeDate' objects:}
    
    
    Date and time stamps are represented by an S4 object of class 'timeDate'. 
        
    \preformatted{
    setClass("timeDate",
        representation(
            Data = "POSIXct",
            format = "character",
            FinCenter = "character"
        ))
    }
    
    Thus 'timeDate' objects are represented by the \code{@Data} time
    stamps which are \code{POSIXct} formatted as specified in the 
    \code{@format} slot, and they belong to the financial center expressed
    through the slot \code{@FinCenter}.
    
    
    % --------------------------------------------------------------------------
    \bold{How to create 'timeDate' objects ?}
    
    
    There are several possibilities to create 'timeDate' objects:
    
    \code{timeDate} Creates a 'timeDate' object from scratch, \cr
    \code{timeSequence} creates a sequence of 'timeDate' objects,\cr
    \code{timeCalendar} creates a 'timeData' object from calendar atoms,\cr
    \code{Sys.timeDate} returns the current date and time as a 'timeDate' object.  
    
    
    1. Creating a 'timeDate' object from scratch, some examples ...
    
    \preformatted{
        # Show My local Financial Center - Note, by Default this is "GMT"
        print(myFinCenter)
        
        # Compose Character Vector of Dates:
        Dates <- c("1989-09-28","2001-01-15","2004-08-30","1990-02-09")
        Times <- c(  "23:12:55",  "10:34:02",  "08:30:00",  "11:18:23")
        charvec = paste(Dates, Times)
        
        # You get it in GMT - your Financial Center?
        timeDate(charvec)
        
        # From now on my financial center will be Zurich:
        myFinCenter <- ZurichtimeDate(charvec)
        timeDate(charvec)
        timeDate(charvec, FinCenter = "Tokyo")
        
        # Automated Format Identification:
        print(.whichFormat(charvec))
    }

    NOTE: Rmetrics has implemented an automated date/time format identifyer
    for many common date/time formats which tries to recognize automatically 
    the format for the character vector of dates and times. 
    
    NOTE: Rmetrics automatically enforces that the midnight standard on dates 
    and times is always fulfilled, try to see it 
    \code{.midnightStandard("2008-01-31 24:00:00")}
    
    
    2. Creating a sequence of 'timeDate' objects, some examples
    
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
    
    
    3. Creating a 'timeDate' object from calendar atoms, some examples ...

    
    \preformatted{
        # Monthly calendar for Current Year
        currentYear
        timeCalendar() 
        
        # Daily 'timeDates' for January data from Tokyo local time 16:00
        timeCalendar(2008, m=1, d=1:31, h=16, zone = "Tokyo", FinCenter = "Zurich") 
    }
    
    4. The current date and time
    
    \preformatted{
        # Current date and Time:
        Sys.timeDate()
        
        # What time is it now in Tokyo ?
        Sys.timeDate("Tokyo")
    }


    % --------------------------------------------------------------------------
    \bold{Math Operations on 'timeDate' Objects:}
    
    
    Math operations allow to add and subtract dates and times,
    and perform logical operations on 'timeDate' objects.\cr
    
    
    \describe{
        \item{\code{Ops}}{\cr
            Group 'Ops' operations on 'timeDate' objects,
            }
        \item{\code{+}}{\cr
            Performs + operation on 'timeDate' objects,
            }
        \item{\code{-}}{\cr
            Performs - operation on 'timeDate' objects.
            }
    }
            
            
    \code{Ops} Group 'Ops' operations on 'timeDate' objects,\cr
    \code{+} Performs + operation on 'timeDate' objects,\cr
    \code{-} Performs - operation on 'timeDate' objects,\cr
    
    
    \preformatted{
        
        # Date and Time now:
        now = Sys.timeDate()
        
        # One hour later:
        now - 3600

    }
    
    
    
    \code{round.timeDate} Rounds objects of class 'timeDate',\cr
    \code{trunc.timeDate} Truncates objects of class 'timeDate'. 
    
    
    \code{diff.timeDate} Returns suitably lagged and iterated differences,\cr
    \code{difftimeDate}Returns a difference of two 'timeDate' objects.
    
    \preformatted{
        # Current Time Difference between Zurich and Tokyo:
        ? now = Sys.timeDate("Zurich")
        ? now - timeDate(now, FinCenter = "Tokyo")
    }
    
    
    % --------------------------------------------------------------------------
    \emph{Subsetting 'timeDate' Objects:}
    
    Subsetting a 'timeDate' is a very important issue in the management of 
    dates and times. Rmetrics offers several functions which are usefule in 
    this context:\cr
    
    \code{[} \tab Extracts or replaces subsets from 'timeDate' objects, \cr
    \code{window, cut} \tab extract a piece from a 'timeDate' object, \cr
    \code{start} \tab extracts the first entry of a 'timeDate' object, \cr
    \code{end} \tab extracts the last entry of a 'timeDate' object, \cr
    \code{length} \tab returns the length of a 'timeDate' object. \cr 
    
    
    \preformatted{
        tC = timeCalendar()
        tC
        
        # The first quarter - Several alternative Solutions:
        tC[1:3]
        tC[-(4:length(tC))]
        window(tC, "2001-01-01", "2001-03-31")
        cut(tC, "2001-01-01", "2001-03-31")
        
        # The quarterly series:
        tC[seq(3, 12, by = 3)]
        
        # Start, end and length of a 'timeDate' object
        start(tC)
        end(tC)
        length(tC)
    }
    
    
    \code{blockStart} Creates start dates for equally sized blocks, \cr
    \code{blockEnd} Creates end dates for equally sized blocks.
     

    
    % --------------------------------------------------------------------------
    \emph{Coercion and Transformations of 'timeDate' Objects:}
    
    
    There exist several methods to coerce and transform \code{timeDate}
    objects into other objects:
    

    \code{as.timeDate} Implements Use Method,\cr
    \code{timeDate.default} default Method,\cr
    \code{as.timeDate.POSIXt} returns a 'POSIX' object as 'timeDate' object,\cr
    \code{as.timeDate.Date} returns a 'POSIX' object as 'timeDate' object. 
    

    \code{as.character.timeDate} Returns a 'timeDate' object as 'character' string,\cr
    \code{as.double.timeDate} returns a 'timeDate' object as 'numeric' object,\cr
    \code{as.data.frame.timeDate} returns a 'timeDate' object as 'data.frame' object,\cr
    \code{as.POSIXct.timeDate} returns a 'timeDate' object as 'POSIXct' object,\cr
    \code{as.POSIXlt.timeDate} returns a 'timeDate' object as 'POSIXlt' object,\cr
    \code{as.Date.timeDate} returns a 'timeDate' object as 'Date' object. 


    % --------------------------------------------------------------------------
    \emph{Concatenation, Ordering, and Sorting of timeDate Objects:}
    
    
    'timeDate' objects can be concatenated, replicated, sorted and
    resampled, unified and reverted. The generic functions to do that are:

    \code{c} Concatenates 'timeDate' objects,\cr
    \code{rep} Replicates a 'timeDate' object,\cr
    \code{sort} Sorts a 'timeDate' object,\cr
    \code{sample} Resamples a 'timeDate' object,\cr
    \code{unique} Makes a 'timeDate' object unique,\cr
    \code{rev} Reverts a 'timeDate' object. 
    
    Here are same examples:
    
    \preformatted{
        # Compose Character Vector of Dates:
        Dates <- c("1989-09-28","2001-01-15","2004-08-30","1990-02-09")
        Times <- c(  "23:12:55",  "10:34:02",  "08:30:00",  "11:18:23")
        charvec = paste(Dates, Times)
        
        # Concatenate the local timpe stamps to Zurich time ...
        ZH = timeDate(charvec, FinCenter = "Zurich")
        NY = timeDate(charvec, FinCenter = "NewYork")
    }
    

}


\section{2. Daylight Saving Time and Financial Centers}{

    To each financial center worldwide we can attribute a function 
    with the appropriate Daylight Saving Time Rules. Almost 400
    prototypes are made available through the Olson time zone
    data base. The cities and regions can be listed using the
    command \code{listFinCenter}. The DST rules for a specific
    financial center can be viewed by their name, e.g. \code{Zurich()}.
    Additional financial centers can be added by the user taking care
    of the format specification of the DST functions.
    
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
}

\section{3. Holidays and Holiday Calendars}{


    % --------------------------------------------------------------------------
    \emph{Holidays:}
    
    
    
    % --------------------------------------------------------------------------
    \emph{Holiday Calendars:}
    
    
    \code{holidayZURICH}\cr
    \code{holidayNYSE}\cr
    \code{holidayZURICH}
    
    
    Thanks to all the Rmetrics users who gave us many additional
    information concerned with local holidays. 

}





\keyword{package}

