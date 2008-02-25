\name{start}


\alias{start}
\alias{end}

\alias{start.timeDate}
\alias{end.timeDate}

\alias{min.timeDate}
\alias{max.timeDate}

\alias{range.timeDate}


\title{Terminal Times and Range}


\description{
    
    Extracts the time the first or last 
    observation was taken, or computes the
    range.
    
}


\usage{
\method{start}{timeDate}(x, \dots)

\method{end}{timeDate}(x, \dots)

\method{min}{timeDate}(\dots, na.rm = FALSE)

\method{max}{timeDate}(\dots, na.rm = FALSE)

\method{range}{timeDate}(\dots, na.rm = FALSE)
}


\arguments{
  
    \item{x}{
        an object of class \code{timeDate}.
        }
    \item{\dots}{
        [start][end] - \cr
        not used, \cr
        [min][max] -
        'timeDates' objects.
        } 
    \item{na.rm}{
        not used.
        }
                
}


\details{

    Note, the series will be time ordered before the start or
    end time stamps are extracted. Sorting is done in the
    way that the first observation appears in time before the 
    last observation.
    
}


\value{
   
    returns an object of class \code{timeDate}.

}


\examples{  
## timeCalendar - 
   # Random Calendar Dates:
   tR = sample(timeCalendar())
   sort(tR)
   tR
    
## start | end  -
   start(tR)
   end(tR)
   
## The First and Last Time Stamp:
   tR[1]
   tR[length(tR)]
   rev(tR)[1]
   
## The Range:
   c(start(tR), end(tR))
   range(tR)
}


\keyword{chron}

