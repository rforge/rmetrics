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
        an univariate 'timeSeries' object.
        }

}


\value{

    Function \code{orderStatistics} returns the order statistic of
    an univariate 'timeSeries' object. The output is an object of
    class 'list'.
   
}


\examples{
## Load Swiss Pension Fund Benchmark Data -  
   setRmetricsOptions(myFinCenter = "GMT")
   X <- LPP2005REC[, "SPI"]
   colnames(X)
   
## Compute 1\% Order Statistics -
   N <- round(0.01*nrow(X))
   N
   OS <- orderStatistics(X)[[1]]
   OS[1:N, ]
}


\keyword{chron}

