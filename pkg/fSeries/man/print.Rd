\name{print}


\title{Prints timeSeries objects}


\alias{show.timeSeries}
\alias{show,timeSeries-method}


\description{
    
    Prints objects of class timeSeries.
    
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



\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## print -  
   myFinCenter <<- "GMT"
   EDHEC = as.timeSeries(data(edhec))[1:12, 1:4]
   colnames(EDHEC) <- abbreviate(colnames(EDHEC), 6)
   print(EDHEC)
}


\keyword{chron}

