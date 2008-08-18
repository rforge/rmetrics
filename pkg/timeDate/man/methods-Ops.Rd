\name{timeDateMathOps}


\alias{timeDateMathOps}


\alias{Ops,timeDate,timeDate-method}
\alias{+,timeDate,timeDate-method}
\alias{+,timeDate,numeric-method}
\alias{-,timeDate,timeDate-method}
\alias{-,timeDate,numeric-method}


\title{timeDate Mathematical Operations}


\description{
    
    Functions for mathematical and logical operations on 'timeDate'
    objects.
    \cr
     
    The functions are:
    
    \tabular{ll}{
    \code{Ops.timeDate} \tab Group 'Ops' generic functions for 'timeDate' objects, \cr
    \code{+.timeDate} \tab Performs arithmetic + operation on 'timeDate' objects, \cr
    \code{-.timeDate} \tab Performs arithmetic - operation on 'timeDate' objects. }
       
}


\usage{
\S4method{Ops}{timeDate,timeDate}(e1, e2)

\S4method{+}{timeDate,timeDate}(e1, e2)
\S4method{+}{timeDate,numeric}(e1, e2)
\S4method{-}{timeDate,timeDate}(e1, e2)
\S4method{-}{timeDate,numeric}(e1, e2)    
}


\arguments{
 
   \item{e1, e2}{
       usually objects of class \code{timeDate}, in the case of
       addition and subtraction \code{e2} may be of class
       \code{numeric}.
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
}


\keyword{chron}

