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
## data -  
   # Microsoft Data:
   require(fEcofin)
   myFinCenter <<- "GMT"
   MSFT = as.timeSeries(data(msft.dat))
   head(MSFT)

}


\keyword{chron}

