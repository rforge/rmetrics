\name{cumulated}


\title{Cumulated Time Series from Returns}


\alias{cumulated}
\alias{cumulated.default}


\description{
    
    Calculate cumulated financial series, e.g. prices 
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
    

\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## data -  
   # Microsoft Data: 
   myFinCenter <<- "GMT"
   MSFT = as.timeSeries(data(msft.dat))[1:10, "Close"]
   MSFT

## cumulated -  
   # Index series to 100:
   MSFT = 100 * MSFT/MSFT@Data[1,1]
   MSFT
   MSFT.RET = returns(MSFT, method = "discrete")
   cumulated(MSFT.RET, method = "discrete")  
}


\keyword{chron}

