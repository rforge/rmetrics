\name{str-methods}


\title{Object Str}


\alias{str}
\alias{str,timeSeries-method}


\description{
    
    Compactly Display the Structure of a 'timeSeries' Object.        
    
}


\usage{
\S4method{str}{timeSeries}(object, \dots)
}


\arguments{

    \item{object}{  
        an object of class \code{timeSeries}.
        }
    \item{\dots}{
        arguments passed to other methods.
        }

}


\value{

    returns a str report for an object of class \code{timeSeries}.
        
}


\examples{
## Load Microsoft Data Set -
   data(MSFT)
   X = MSFT[1:12, 1:4]
   colnames(X) <- abbreviate(colnames(X), 4)
    
## Display Structure - 
   str(X)
}


\keyword{chron}

