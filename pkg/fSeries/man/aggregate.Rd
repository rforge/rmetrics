\name{aggregate}


\title{timeSeries Class, Functions and Methods}


\alias{aggregate}
\alias{aggregate.timeSeries}


\description{
    
    Aggregates a 'timeSeries' Object.      
    
}


\usage{
\method{aggregate}{timeSeries}(x, by = c("monthly", "quarterly"), 
    FUN = colMeans, units = NULL, \dots)
}


\arguments{
  
    \item{by}{
        [aggregate] - \cr
        a character string denoting the aggregation period, either 
        \code{"monthly"} or \code{"quarterly"}.
        }
    \item{FUN}{
        the function to be applied.
        }               
    \item{units}{
        an optional character string, which allows to overwrite the 
        current column names of a \code{timeSeries} object. By default 
        \code{NULL} which means that the column names are selected
        automatically.
        }
    \item{x}{
        an object of class \code{timeSeries}.
        }  
    \item{\dots}{
        arguments passed to other methods.
        }
       
}


\value{

    returns an aggregated S4 object of class \code{timeSeries}.
        
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## data - 
   x = as.timeSeries(data(msft.dat))
   
## aggregate -
   aggregate(x)   
}


\keyword{chron}

