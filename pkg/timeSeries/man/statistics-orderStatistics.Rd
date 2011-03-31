\name{orderStatistics}


\title{order Statistics}

\alias{orderStatistics}


\description{
    
    Computes order statistic of a 'timeSeries'. 
}


\usage{
orderStatistics(x)
}


\arguments{

    \item{x}{
        an object of class \code{timeSeries}.
        }

}


\value{

    \code{orderStatistics}
    \cr
    returns ...
   
}


\examples{
## Load Swiss Pension Fund Benchmark Data -  
   setRmetricsOptions(myFinCenter = "GMT")
   data(LPP2005REC)
   
## Compute Order Statistics -
   head(orderStatistics(LPP2005REC))
}


\keyword{chron}

