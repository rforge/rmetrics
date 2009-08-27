\name{cumulated}


\title{Cumulated Time Series from Returns}


\alias{cumulated}
\alias{cumulated.default}


\description{
    
    Compute cumulated financial series, e.g. prices 
    or indexes, from financial returns.

}


\usage{
cumulated(x, \dots)

\method{cumulated}{default}(x, method = c("continuous", "discrete", 
    "compound", "simple"), percentage = FALSE, \dots)
}


\arguments{

    \item{method}{
        a character string naming the method how the returns were
        computed.
        } 
    \item{percentage}{
        a logical value. By default \code{FALSE}, if \code{TRUE} the 
        series will be expressed in percentage changes.
        }   
    \item{x}{
        an object of class \code{timeSeries}.
        }
    \item{\dots}{
        arguments to be passed.
        }
        
}


\value{

    all functions return a time series object of the same class as
    the input argument \code{x}.
        
}
    

\examples{
## Load Microsoft Data - 
   data(MSFT)
   setRmetricsOptions(myFinCenter = "GMT")
   MSFT <- MSFT[1:10, "Close"]
   MSFT

## Discrete Return Series  - 
   MSFT = 100 * MSFT/as.numeric(MSFT[1, 1])
   MSFT.RET = returns(MSFT, method = "discrete")
   MSFT.RET
   
## Cumulative Series, Indexed to 100 - 
   cumulated(MSFT.RET, method = "discrete") 
}


\keyword{chron}

