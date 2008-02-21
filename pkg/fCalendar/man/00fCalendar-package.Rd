\name{fCalendar-package}


\alias{fCalendar-package}


\alias{fCalendar}


\docType{package}


\title{Utilities and Tools Package}


\description{

    Package of calendar utilities for Rmetrics.
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

    1. Daylight Saving Time and Financial Centers\cr
    2. S4 'timeDate' Class and Functions\cr
    3. Holidays and Haliday Calendars
    
    
}  


\section{1. Daylight Saving Time and Financial Centers}{

    To each financial center worldwide we can attribute a function 
    with the appropriate Daylight Saving Time Rules. Almost 400
    prototypes are made available through the Olson time zone
    data base. The cities and regions can be listed using the
    command \code{listFinCenter}. The DST rules for a specific
    financial center can be listed by its name, e.g. \code{Zurich()}.
    Additional financial centers can be added by the user taking care
    of the format specification of the DST functions.
    
    There is also function which allows to updata the tables ...
    
}


\section{2. Creating timeDate Objects from Scratch}{


    \emph{S4 'timeDate' Class:}
    
    
    \emph{Creating timeDate Objects from Scratch:}
    
    
    \code{timeDate}\cr
    \code{timeSequence}\cr
    \code{timeCalendar}\cr
    \code{Sys.timeDate}\cr
    
    
    \emph{Math Operations on 'timeDate' Objects:}
    
    
    \tabular{ll}{
    \code{Ops.timeDate} \tab Group 'Ops' operations on 'timeDate' objects,\cr
    \code{+.timeDate} \tab Performs + operation on 'timeDate' objects,\cr
    \code{-.timeDate} \tab Performs - operation on 'timeDate' objects,\cr
    \code{diff.timeDate} \tab Returns suitably lagged and iterated differences,\cr
    \code{difftimeDate} \tab Returns a difference of two 'timeDate' objects,\cr
    \code{round.timeDate} \tab Rounds objects of class 'timeDate',\cr
    \code{trunc.timeDate} \tab Truncates objects of class 'timeDate'.}

    
    \emph{Coercion and Transformations of 'timeDate' Objects:}
    
    
    There exist several methods to coerce and transform \code{timeDate}
    objects into other objects:
    
    \tabular{ll}{
    \code{as.timeDate} \tab Implements Use Method,\cr
    \code{timeDate.default} \tab Default Method,\cr
    \code{as.timeDate.POSIXt} \tab Returns a 'POSIX' object as 'timeDate' object,\cr
    \code{as.timeDate.Date} \tab Returns a 'POSIX' object as 'timeDate' object.}
    
    \tabular{ll}{
    \code{as.character.timeDate} \tab Returns a 'timeDate' object as 'character' string,\cr
    \code{as.double.timeDate} \tab Returns a 'timeDate' object as 'numeric' object,\cr
    \code{as.data.frame.timeDate} \tab Returns a 'timeDate' object as 'data.frame' object,\cr
    \code{as.POSIXct.timeDate} \tab Returns a 'timeDate' object as 'POSIXct' object,\cr
    \code{as.POSIXlt.timeDate} \tab Returns a 'timeDate' object as 'POSIXlt' object,\cr
    \code{as.Date.timeDate} \tab Returns a 'timeDate' object as 'Date' object.}


    \emph{Concatenation, Ordering, and Sorting of timeDate Objects:}
    
    
    \tabular{ll}{
    \code{c.timeDate} \tab Concatenates 'timeDate' objects,\cr
    \code{rep.timeDate} \tab Replicates a 'timeDate' object,\cr
    \code{sort.timeDate} \tab Sorts a 'timeDate' object,\cr
    \code{sample.timeDate} \tab Resamples a 'timeDate' object,\cr
    \code{unique.timeDate} \tab Makes a 'timeDate' object unique,\cr
    \code{rev.timeDate} \tab Reverts a 'timeDate' object.}

}



\section{3. Holidays and Haliday Calendars}{


    \emph{Holidays:}
    
    \emph{Holiday Calendars:}

}


\keyword{package}

