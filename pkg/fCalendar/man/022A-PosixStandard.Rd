\name{PosixStandard}

\alias{PosixStandard}


\title{Summary of Posix Standard}


\description{
        
    A summary and description how Rmetrics uses the
    POSIX standard.
    \cr
    
    The functions from R's base Package are:
     
    \tabular{ll}{
    \code{strptime} \tab Turns charater representations into POSIXlt, \cr
    \code{as.POSIXlt} \tab Converts into POSIXlt, \cr
    \code{ISOdatetime} \tab Returns an object of class POSIXct, \cr
    \code{ISOdate} \tab Returns an object of class POSIXct, \cr
    \code{seq} \tab Creates sequence of class POSIXct, \cr
    \code{as.POSIXct} \tab Converts into POSIXct, \cr
    \code{Sys.timeDate} \tab Returns system time as an object of class 'timeDate', \cr
    \code{format} \tab Turns POSIXt into character representations, \cr
    \code{strftime} \tab Represents an alias for format.POSIXlt, \cr
    \code{as.character} \tab Calls format function, \cr
    \code{Sys.time} \tab Returns system time, \cr
    \code{Sys.timezone} \tab Returns system timezone, \cr
    \code{print} \tab Prints POSIXt objects, generic function, \cr
    \code{"-"} \tab Subtracts, \cr
    \code{time1 lop time2} \tab Logical Operation into PSOIXlt, \cr
    \code{time1 - time2} \tab Returns an obejct of class difftime, \cr
    \code{difftime} \tab Takes a difference, \cr
    \code{as.difftieme} \tab Converts into difftime, \cr
    \code{round} \tab Rounds difftime, generic function, \cr
    \code{as integer} \tab Converts POSIXt into integer, \cr
    \code{as.numeric} \tab Converts POSIXt into integer, \cr
    \code{as.character} \tab Converts POSIXt into character, \cr
    \code{as.data.frame} \tab Converts POSIXt into data frame, \cr
    \code{"["} \tab Subsets a POSIXt object, \cr
    \code{c} \tab Concatenates POSIXt objects, \cr
    \code{min, max, range} \tab Minimum, Maximum, Range of POSIXt objects, \cr
    \code{sort, rev, uniq} \tab Sorts, reverts, makes unique POSIXt objects, \cr
    \code{Sys.Date} \tab Returns current Date as Date object, \cr
    \code{format} \tab Turns Date into character representations, \cr
    \code{print} \tab Prints POSIXt objects, generic function, \cr
    \code{seq} \tab Creates sequence of class Date. }
     
    The function addons are:
     
    \tabular{ll}{
    \code{isPOSIX} \tab Checks for an object of class POSIX. }
                      
}


\usage{
isPOSIX(x)
}


\arguments{
  
    \item{x}{
        an argument to be checked.
        }
        
}

\author{

    Diethelm Wuertz for this summary.
    
}


\examples{
## SOURCE("fCalendar.022A-PosixStandard")

}


\keyword{programming}

