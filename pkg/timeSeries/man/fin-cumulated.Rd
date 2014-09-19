\name{cumulated}


\title{Cumulated Time Series from Returns}


\alias{cumulated}
\alias{cumulated.default}


\description{
    
    Computes a cumulated financial 'timeSeries', 
    e.g. prices or indexes, from financial returns.

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


\details{

    Note, the function \code{cumulated} assumes as input discrete 
    returns from a price or index series. Only then the cumulatrd 
    series agrees with the original price or index series. The 
    first values of the cumulated series cannot be computed, it
    is assumed that the series is indexed to 1.
        
}


\value{

    Returns a 'timeSeries' object of the same class as
    the input argument \code{x}.
        
}
    

\examples{
## Use the Microsofts' Close Prices Indexed to 1 - 
   MSFT.CL <- MSFT[, "Close"]
   MSFT.CL <- MSFT.CL/MSFT[[1, "Close"]]
   head(MSFT.CL)

## Compute Discrete Return -    
   MSFT.RET <- returns(MSFT.CL, method = "discrete")
   
## Cumulated Series and Compare - 
   MSFT.CUM <- cumulated(MSFT.RET, method = "discrete") 
   head(cbind(MSFT.CL, MSFT.CUM))
}


\keyword{chron}

