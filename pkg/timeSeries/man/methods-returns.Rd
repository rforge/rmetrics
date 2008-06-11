\name{returns}


\title{Financial Returns}


\alias{returns}
\alias{returns,ANY-method}
\alias{returns,timeSeries-method}

\alias{returnSeries}
\alias{getReturns}


\description{
    
    Compute financial returns from prices or indexes.
       
}


\usage{
returns(x, \dots)

\S4method{returns}{ANY}(x, method = c("continuous", "discrete", 
    "compound", "simple"), percentage = FALSE, \dots)
\S4method{returns}{timeSeries}(x, method = c("continuous", "discrete", 
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

