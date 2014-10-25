\name{wealth}


\title{Conversion of an index to wealth}


\alias{index2wealth}


\description{
    
    Converts an index series to a wealth series normalizing the
    starting value to one.

}


\usage{
index2wealth(x)
}


\arguments{

    \item{x}{
        an object of class 'timeSeries'.
        }
        
}


\value{

    returns a time series object of the same class as
    the input argument \code{x} normalizing the starting
    value to one.
        
}
    

\examples{
## Load MSFT Open Prices  - 
   INDEX <- MSFT[1:20, 1]
   INDEX
   
## Compute Wealth Normalized to 100 - 
   100 * index2wealth(INDEX)
}


\keyword{chron}

