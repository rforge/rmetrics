\name{bind}


\title{Bind two timeSeries objects}

\alias{merge}
\alias{cbind}
\alias{rbind}
\alias{merge,timeSeries,timeSeries-method}
\alias{cbind.timeSeries} 
\alias{rbind.timeSeries} 
\alias{c.timeSeries} 

\description{
  
  Binds two 'timeSeries' objects either by column or row.        
  
}


\usage{
\S4method{merge}{timeSeries,timeSeries}(x, y, units = NULL, ...)  
\method{cbind}{timeSeries}(x, y, units = NULL, ...)  
\method{rbind}{timeSeries}(x, y, units = NULL, ...)
\method{cbind}{timeSeries}(x, y, units = NULL, ...)
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
    arguments passed to returned timeSeries object.
  }
}


\value{

  returns a S4 object of class \code{timedate}. 
}


\examples{
## data - 
   x = as.timeSeries(data(msft.dat))[1:12, ]

## cbind -
   cbind(x[, "Open"], returns(x[, "Open"]), units = c("Open", "Return"))

## rbind -
   rbind(x[1:3, "Open"], x[10:12, "Open"])
}


\keyword{chron}

