\name{plot}


\title{Plot Reports}


\alias{plot}
\alias{plot.timeSeries}
\alias{lines.timeSeries}
\alias{points.timeSeries}


\description{
    
    Functions to plot timeSeries objects.
    
}


\usage{
\method{plot}{timeSeries}(x, \dots) 
\method{lines}{timeSeries}(x, \dots)
\method{points}{timeSeries}(x, \dots)
}


\arguments{

    \item{x}{  
        an object of class \code{timeSeries}.
        }
    \item{\dots}{
        arguments passed to other methods.
        }

}


\value{

    a plot or plot elements of an object of class \code{timeSeries}.
        
}



\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## data -  
   myFinCenter <<- "GMT"
   EDHEC = as.timeSeries(data(edhec))[1:12, 1:4]
   colnames(EDHEC) <- abbreviate(colnames(EDHEC), 6)

## plot -  
   plot(EDHEC[,1], type = "o", col = "steelblue", 
     main = "EDHEC", xlab = "1997", ylab = "Return")
}


\keyword{chron}

