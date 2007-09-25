\name{durations}


\title{Computes durations from a timeSeries}


\alias{durations}
\alias{durationSeries}



\description{
    
    Computes durations from an object of class 'timeSeries'.  
       
}


\usage{  
durationSeries(x, trim = FALSE, units = c("secs", "mins", "hours"))   
}


\arguments{
  
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

    returns an object of class \code{timeSeries}.
        
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
   durations(MSFT, units = "hours")
}


\keyword{chron}

