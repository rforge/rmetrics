\name{timeDateOrdering}

\alias{timeDateOrdering}

\alias{c.timeDate}
\alias{rep.timeDate}
\alias{sort.timeDate}
\alias{sample.timeDate}
\alias{unique.timeDate}
\alias{rev.timeDate}


\title{timeDate Concatenation, Sorting and Re-Ordering}


\description{
    
    A collection and description of generic functions 
    to concatenate, to sort and to reorder 'timeDate'
    aobjects
    \cr
 
    The functions are:
    
    \tabular{ll}{
    \code{c.timeDate} \tab Concatenates 'timeDate' objects, \cr
    \code{rep.timeDate} \tab Replicates a 'timeDate' object, \cr
    \code{sort.timeDate} \tab Sorts a 'timeDate' object, \cr
    \code{sample.timeDate} \tab Resamples a 'timeDate' object, \cr
    \code{unique.timeDate} \tab NMakes a 'timeDate' object unique, \cr
    \code{rev.timeDate} \tab Reverts  a 'timeDate' object. }
       
}


\usage{
\method{c}{timeDate}(\dots, recursive = FALSE)
\method{rep}{timeDate}(x, \dots)
\method{sample}{timeDate}(x, \dots)
\method{sort}{timeDate}(x, \dots)
\method{unique}{timeDate}(x, \dots)
\method{rev}{timeDate}(x)
}


\arguments{
      
    \item{recursive}{
        [c] - \cr
        a logical. If recursive  is set to \code{TRUE}, the function 
        recursively descends through lists combining all their elements 
        into a vector. 
        }
    \item{x}{
        an object of class \code{timeDate}.
        }  
    \item{\dots}{
        arguments passed to other methods.
        }
       
}


\value{
    
    \code{c}\cr
    \code{rep}\cr
    \code{sample}\cr
    \code{sort}\cr
    \code{unique}\cr
    \code{rev}
    \cr
    these are functions to concatenate, to replicate, to resample, to
    sort, to make unique, and to revert \code{timeDate} objects. The
    functions return an object of class \code{"timeDate"}.

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
## c -
   # Create Character Vectors:
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
   
## c - 
   # Concatenate and Replicate timeDate Objects:
   c(GMT[1:2], ZUR[1:2])
   c(ZUR[1:2], GMT[1:2])
   
## rep - 
   rep(ZUR[2], times = 3)
   rep(ZUR[2:3], times = 2)  
}


\keyword{chron}

