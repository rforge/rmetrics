\name{timeDateMathOps}

\alias{timeDateMathOps}

\alias{Ops.timeDate}
\alias{+.timeDate}
\alias{-.timeDate}
\alias{diff.timeDate}
\alias{difftimeDate}
\alias{round.timeDate}
\alias{trunc.timeDate}


\title{timeDate Mathematical Operations}


\description{
    
    A collection and description of functions for 
    mathematical and logical operations on 'timeDate'
    objects.
    \cr
     
    The functions are:
    
    \tabular{ll}{
    \code{Ops.timeDate} \tab Group 'Ops' generic functions for 'timeDate' objects, \cr
    \code{+.timeDate} \tab Performs arithmetic + operation on 'timeDate' objects, \cr
    \code{-.timeDate} \tab Performs arithmetic - operation on 'timeDate' objects, \cr
    \code{diff.timeDate} \tab Returns suitably lagged and iterated differences, \cr
    \code{difftimeDate} \tab Returns a difference of two 'timeDate' objects, \cr
    \code{round.timeDate} \tab Rounds objects of class 'timeDate', \cr
    \code{trunc.timeDate} \tab Truncates objects of class 'timeDate'. }
       
}


\usage{
\method{Ops}{timeDate}(e1, e2)
\method{+}{timeDate}(e1, e2)
\method{-}{timeDate}(e1, e2)
\method{diff}{timeDate}(x, lag = 1, differences = 1, \dots)
difftimeDate(time1, time2, 
    units = c("auto", "secs", "mins", "hours", "days", "weeks"))
\method{round}{timeDate}(x, units = c("days", "hours", "mins"), ...) 
\method{trunc}{timeDate}(x, units = c("days", "hours", "mins"), ...)     
}


\arguments{
  
    \item{differences}{
        [diff] - \cr
        an integer indicating the order of the difference.
        }
    \item{e1, e2}{
        ["+"][["-"]["Ops"] - \cr
        usually objects of class \code{timeDate}, in the case of
        addition and subtraction \code{e2} may be of class
        \code{numeric}.
        }
    \item{lag}{
        [lag] - \cr
        an integer indicating which lag to use.  
        }

    \item{method}{
        [modify] - \cr
        a character string defining the modification method, one of
        \code{"sort"}, \code{"round"}, or \code{"trunc"}. 
        }     
    \item{time1, time2}{
        [difftime] - \cr
        two objects objects of class \code{timeDate}.
        }
    \item{units}{
        [difftimeDate][round][trunc] -\cr
        a character string denoting the date/time units in which the 
        results are desired.
        }
    \item{x}{
        [diff][round][trunc] -\cr
        an object of class \code{timeDate}.
        }  
    \item{\dots}{
        arguments passed to other methods.
        }
       
}


\value{
    
    \code{Ops.timeDate}
    \cr
    these are functions for mathematical operations. Group code{Ops} are 
    generic functions which manage mathematical operations. 
    
    \code{+.timeDate}\cr
    \code{-.timeDate}
    \cr
    The plus operator \code{"+"} performs arithmetic "+" operation on 
    \code{timeDate} objects, and the minus operator \code{"-"} returns a 
    \code{difftime} object if both arguments \code{e1} and \code{e2} 
    are \code{"timeDate"} objects, or returns a \code{"timeDate"} object 
    \code{e2} seconds earlier than \code{e1}.
    
    \code{diff.timeDate}
    \cr
    For the function, \code{diff.timeDate}, if \code{x} is a vector 
    of length \code{n} and \code{differences=1}, then the computed result 
    is equal to the successive differences \code{x[(1+lag):n] - x[1:(n-lag)]}. 
    If \code{difference} is larger than one this algorithm is applied
    recursively to \code{x}. Note that the returned value is a vector 
    which is shorter than \code{x}.
    
    \code{difftimeDate}
    \cr
    The function, \code{difftimeDate}, takes a difference of two 
    \code{timeDate} objects and returns an object of class \code{"difftime"} 
    with an attribute indicating the units.
    
    \code{round.timeDate}\cr
    \code{trunc.timeDate}
    \cr
    The two functions \code{round} and \code{trunc} allow to round or to 
    truncate \code{"timedate"} objects to the specified unit and return
    them as \code{"timedate"} objects.
    \cr

}


\seealso{

    %\code{timeDateClass}, \code{timeDateCoercion}, \code{timeDateSpecDates}
    
    We also recommend to inspect the help pages for the POSIX time and date 
    class, \code{?Dates}, and the help pages from the contributed R packages
    \code{chron} and \code{date}.
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## Create Character Vectors:
   dts = c("1989-09-28", "2001-01-15", "2004-08-30", "1990-02-09")
   dts
   tms = c(  "23:12:55",   "10:34:02",   "08:30:00",   "11:18:23")
   tms
   
## "+/-" - 
   # Add One Day to a Given timeDate Object:
   GMT = timeDate(dts, zone = "GMT", FinCenter = "GMT")
   GMT
   ZUR = timeDate(dts, zone = "GMT", FinCenter = "Europe/Zurich")
   ZUR
   GMT + 24*3600
   ZUR[2] - ZUR[1]
  
## "<" ">" "==" ... - 
   # Subsets from and Lops for timeDate Objects:
   GMT[GMT < GMT[2]]
   ZUR[ZUR < ZUR[3]] == ZUR[1:3]

## diff - 
   # Suitably Lagged and Iterated Differences:
   diff(GMT)
   diff(GMT, lag = 2)
   diff(GMT, lag = 1, diff = 2)
   difftimeDate(GMT[1:2], GMT[-(1:2)])  
}


\keyword{chron}

