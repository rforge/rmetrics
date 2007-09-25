\name{merge}


\alias{merge}
\alias{merge.timeSeries} 
  

\title{Merge two timeSeries objects}


\description{
    
    Merges two timeSeries objects.         
   
}


\usage{
\method{merge}{timeSeries}(x, y, units = NULL, \dots)
}


\arguments{
         
    \item{units}{
        an optional character string, which allows to overwrite the 
        current column names of a \code{timeSeries} object. By default 
        \code{NULL} which means that the column names are selected
        automatically.
        }
    \item{x, y}{
        two objects of class \code{timeSeries}.
        } 
    \item{\dots}{
        arguments passed to other methods.
        }
       
}


\value{

    returns an S4 object of class \code{timeSeries}.
        
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## data - 
   x = as.timeSeries(data(msft.dat))[1:20, "Open"]
   
## aggregate -
   merge(x, returnSeries(x))
}


\keyword{chron}

