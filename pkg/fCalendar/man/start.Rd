\name{start}


\alias{start}

\alias{start.timeDate}
\alias{end.timeDate}


\title{Terminal Times}


\description{
    
    Extracts the time the first or last 
    observation was taken. 
    
}


\usage{
\method{start}{timeDate}(x, \dots)

\method{end}{timeDate}(x, \dots)
}


\arguments{
  
    \item{x}{
        an object of class \code{timeDate}.
        }
    \item{\dots}{
        arguments passed to other methods.
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
}


\keyword{chron}

