\name{financial}


\title{Financial Time Series}


\alias{financial}

\alias{cumulatedSeries}
\alias{durationSeries}



\description{
    
    A collection and description of functions 
    and methods dealing with special financial 
    timeseries objects.  
       
}


\usage{  
cumulatedSeries(x, index = 100)
durationSeries(x, trim = FALSE, units = c("secs", "mins", "hours"))   
}


\arguments{

    \item{index}{
        a numeric value to which the cumulated return series will be indexed.
        }   
    \item{trim}{
        a logical value. By default \code{TRUE}, the first missing 
        observation in the return series will be removed. 
        }
    \item{units}{
        [durationSeries] - \cr
        a character value or vector which allows to set the 
        units in which the durations are measured. By default durations
        are measured in seconds. 
        }     
    \item{x}{
        an object of class \code{timeSeries}.
        }
        
}


\value{

    all functions return an object of class \code{timeSeries}.
        
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## data -  
   # Microsoft Data: 
   myFinCenter <<- "GMT"
   MSFT = as.timeSeries(data(msft.dat))[1:20, "Open"]
   head(MSFT)

## durations -  
   # Durations in hours: Continuous Returns:
   durationSeries(MSFT, units = "hours")
}


\keyword{chron}

