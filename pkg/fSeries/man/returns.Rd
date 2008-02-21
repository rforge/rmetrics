\name{returns}


\title{Financial Returns}


\alias{returns}
\alias{returns.default}
\alias{returns.timeSeries}
\alias{returns.zoo}

\alias{returnSeries}
\alias{getReturns}


\description{
    
    Calculate financial returns from Prices or Indexes.
       
}


\usage{
returns(x, \dots)

\method{returns}{default}(x, method = c("continuous", "discrete", 
    "compound", "simple"), percentage = FALSE, \dots)
\method{returns}{timeSeries}(x, method = c("continuous", "discrete", 
    "compound", "simple"), percentage = FALSE, na.rm = TRUE, 
    trim = TRUE, \dots)
\method{returns}{zoo}(x, method = c("continuous", "discrete", 
    "compound", "simple"), percentage = FALSE, na.rm = TRUE, 
    trim = TRUE, \dots)
    
getReturns(\dots)
returnSeries(\dots)
}


\arguments{

    \item{percentage}{
        a logical value. By default \code{FALSE}, if \code{TRUE} the 
        series will be expressed in percentage changes.
        }
    \item{method}{
        ...
        }    
    \item{na.rm}{
        ...
        }    
    \item{trim}{
        ...
        }    
    \item{x}{
        an object of class \code{timeSeries}.
        }
    \item{\dots}{
        arguments to be passed.
        }
        
}


\value{

    all functions return an object of class \code{timeSeries}.
        
}


\note{

    The functions \code{returnSeries}, \code{getReturns}, 
    are synonymes for \code{returns.timeSeries}.

}
    

\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## data -  
   # Microsoft Data: 
   myFinCenter <<- "GMT"
   MSFT = as.timeSeries(data(msft.dat))[1:10, 1:4]
   head(MSFT)

## returnSeries -  
   # Continuous Returns:
   returns(MSFT)
   # Discrete Returns:
   returns(MSFT, type = "discrete")
   # Don't trim:
   returns(MSFT, trim = FALSE)
   # Use Percentage Values:
   returns(MSFT, percentage = TRUE, trim = FALSE)
}


\keyword{chron}

