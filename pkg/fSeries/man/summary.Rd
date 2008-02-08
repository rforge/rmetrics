\name{summary}


\title{Object Summary}


\alias{summary}
\alias{summary.timeSeries}

\description{
    
    produces a result summary of a timeSeries object.         
    
}


\usage{
\method{summary}{timeSeries}(object, \dots)
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

    returns a summary report for an object of class \code{timeSeries}.
        
}



\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## data -  
   EDHEC = as.timeSeries(data(edhec.tS))[1:12, 1:4]
   colnames(EDHEC) <- abbreviate(colnames(EDHEC), 4)
    
## summary -
   summary(EDHEC)
}


\keyword{chron}

