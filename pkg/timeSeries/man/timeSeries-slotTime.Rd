\name{time}


\title{Get and Set Time stamps of a 'timeSeries'}


\alias{time}
\alias{time<-}

\alias{time,timeSeries-method}
\alias{time.timeSeries}

\alias{time<-.timeSeries}

\alias{sample,timeSeries-method}


\alias{getTime}  
\alias{setTime<-}


\description{
    
    Functions and methods extracting and modifying positions 
    of 'timeSeries' objects.
        
}


\usage{
getTime(x)         
setTime(x) <- value

\S4method{time}{timeSeries}(x, \dots)
\method{time}{timeSeries}(x) <- value
}


\arguments{

    \item{value}{
        a valid value for the component of \code{time(x)}.
        }
    \item{x}{
        an object of class \code{timeSeries}.
        } 
    \item{\dots}{
        optional arguments passed to other methods.
        }
}


\value{

    Returns a 'timeDate' object.
           
}


\examples{   
## Create Dummy timeSeries -      
   X <- timeSeries(matrix(rnorm(24), 12), timeCalendar())
   
## Return Series Positions -
   getTime(X)
   time(X)  
  
## Add / Subtract one Day from X
   setTime(X) <- time(X) - 24*3600 # sec
   X
   time(X) <- time(X) + 24*3600 # sec
   X
}


\keyword{chron}

