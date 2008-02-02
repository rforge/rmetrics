\name{subset}

\alias{subset}
\alias{[.timeDate}
\alias{window.timeDate}
\alias{cut.timeDate}
\alias{start.timeDate}
\alias{end.timeDate}
\alias{blockStart}
\alias{blockEnd}
\alias{length.timeDate}


\title{Subsetting}


\description{
    
    A collection and description of functions and methods
    for subsetting 'timeDate' objects.
    
    \tabular{ll}{
    \code{[.timeDate} \tab Extracts or replaces subsets from 'timeDate' objects, \cr
    \code{window.timeDate} \tab Extracts a piece from a 'timeDate' object, \cr
    \code{cut.timeDate} \tab A synonyme for the window function, \cr
    \code{start.timeDate} \tab Extracts the first entry of a 'timeDate' object, \cr
    \code{end.timeDate} \tab Extracts the last entry of a 'timeDate' object, \cr
    \code{length.timeDate} \tab Gets the length of a 'timeDate' object, \cr
    \code{blockStart} \tab Creates start dates for equally sized blocks, \cr
    \code{blockEnd} \tab Creates end dates for equally sized blocks. }
       
}


\usage{
\method{[}{timeDate}(x, \dots, drop = TRUE)
\method{window}{timeDate}(x, start , end, \dots)
\method{cut}{timeDate}(x, from , to, \dots)
\method{start}{timeDate}(x, \dots)
\method{end}{timeDate}(x, \dots)
\method{length}{timeDate}(x)

blockStart(x, block = 20)
blockEnd(x, block = 20)
}


\arguments{
  
    \item{block}{
        an integer value specifying the length in number of records
        for numerically sized blocks of dates.
        }
    \item{drop}{
        ["["] - \cr
        a logical flag, by default \code{TRUE}.
        }
    \item{from, to}{
        [cut] - \cr
        starting date, required, and end date, ptional. If supplied 
        \code{to} must be after \code{from}.
        }
    \item{start, end}{
        [window] - \cr
        starting date, required, and end date, ptional. If supplied 
        \code{to} must be after \code{from}.
        }
    \item{x}{
        an object of class \code{timeDate}.
        }
    \item{\dots}{
        arguments passed to other methods.
        }         
}


\value{
   
    All functions return an object of class \code{"timeDate"}.
    
    The function \code{[.timeDate} extracts or replaces subsets,
    the function \code{cut} extracts a piece,
    the functions \code{start} and \code{end} extract the first and last 
    element,
    the functions \code{blockStart} and \code{blockEnd} create vectors of 
    start and end values for equally sized blocks. 

}


\seealso{

    \code{timeDate}.
    
    We also recommend to inspect the help pages for the POSIX time and date 
    class, \code{?Dates}, and the help pages from the contributed R packages
    \code{chron} and \code{date}.
}


\examples{  
## Dates in April, currentYear: 
   tS = timeSequence(
      from = paste(currentYear, "-03-01", sep = ""),
      to = paste(currentYear, "-04-30", sep = ""))
   tS
    
## [ - Subsetting First Five Days:
   tS[1:5]   
   
## Subset Start and End Date:
   c(start(tS), end(tS))
}


\keyword{chron}

