\name{runlengths}


\title{Runlengths of a Time Series}


\alias{runlengths}


\description{
    
    Computes runlengths of an univariate \code{timeSeries} object.
       
}


\usage{
runlengths(x, \dots)
}


\arguments{

    \item{x}{
        an univariate time series of class \code{timeSeries}.
        }
    \item{\dots}{
        arguments to be passed.
        }
        
}


\value{

    returns an object of class \code{timeSeries}.
        
}
    

\examples{
## random time series - 
   set.seed(4711)
   x <- rnorm(12)
   tS <- timeSeries(data=x, charvec=timeCalendar(), units="x")
   tS
   
## return runlengths -
   runlengths(tS)
}


\keyword{chron}

