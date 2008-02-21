\name{print}


\title{Print a Time Series}


\alias{show.timeSeries}
\alias{show,timeSeries-method}


\description{
    
    Print 'timeSeries' pbjects.
    
}


\usage{
show.timeSeries(object)
}


\arguments{

    \item{object}{  
        an object of class \code{timeSeries}.
        }
        
}


\value{

    prints an object of class \code{timeSeries}.
        
}


\examples{
## print -  
   myFinCenter <<- "GMT"
   EDHEC = as.timeSeries(data(edhec.tS))[1:12, 1:4]
   colnames(EDHEC) <- abbreviate(colnames(EDHEC), 6)
   print(EDHEC)
}


\keyword{chron}

