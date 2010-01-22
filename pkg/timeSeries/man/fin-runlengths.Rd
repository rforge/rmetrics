\name{runlengths}


\title{Runlengths of a Time Series}


\alias{runlengths}


\description{
    
    Compute runlengths of an univariate time series.
       
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
   x.tS <- timeSeries(data=x, charvec=timeCalendar(), units="x")
   x.tS
   
## return runlengths -
   runlengths(x.tS)
}


\keyword{chron}

