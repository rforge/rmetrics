\name{durations}


\title{Durations from a Time Series}


\alias{durations}
\alias{durationSeries}



\description{
    
    Compute durations from an object of class 'timeSeries'.  
       
}


\usage{  
durations(x, trim = FALSE, units = c("secs", "mins", "hours", "days"))   
}


\arguments{
  
    \item{x}{
        an object of class \code{timeSeries}.
        }
    \item{trim}{
        a logical value. By default \code{TRUE}, the first missing 
        observation in the return series will be removed. 
        }
    \item{units}{
        a character value or vector which allows to set the 
        units in which the durations are measured. By default durations
        are measured in seconds. 
        }     
        
}


\details{

    Durations measure how long it takes until we get the next record in a
    \code{timesSeries} object. We return a time series in which for each 
    time stamp we get the length of the period from when we got the
    last record. This period is measured in length specified by the
    argument \code{units}, for daily data use \code{units="days"}.
    
}

\value{

    returns an object of class \code{timeSeries}.
        
}


\examples{
## Compute Durations in days for the MSFT Sereries - 
   head(durations(MSFT, units = "days"))
   head(durations(MSFT, trim = TRUE, units = "days"))
}


\keyword{chron}

