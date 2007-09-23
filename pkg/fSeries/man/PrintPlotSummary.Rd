\name{PrintPlotSummary}


\title{Print, Plot and Summary Reports}


\alias{show.timeSeries}
\alias{show,timeSeries-method}

\alias{summary.timeSeries}
\alias{plot.timeSeries}
\alias{lines.timeSeries}
\alias{points.timeSeries}

\description{
    
    A collection and description of functions 
    to print, plot and summarize timeSeries 
    objects.         
    \cr     
    
    \tabular{ll}{   
    \code{print.timeSeries} \tab S3: Print method for a 'timeSeries' object, \cr
    \code{summary.timeSeries} \tab S3: Summary method for a 'timeSeries' object, \cr
    \code{plot.timeSeries} \tab S3: Plot method for a 'timeSeries' object, \cr
    \code{lines.timeSeries} \tab S3: Lines method for a 'timeSeries' object, \cr
    \code{points.timeSeries} \tab S3: Points method for a 'timeSeries' object. }
    
}


\usage{
show.timeSeries(object)

\method{summary}{timeSeries}(object, \dots)
\method{plot}{timeSeries}(x, \dots) 
\method{lines}{timeSeries}(x, \dots)
\method{points}{timeSeries}(x, \dots)
}


\arguments{

    \item{object, x}{  
        an object of class \code{timeSeries}.
        }
    \item{\dots}{
        arguments passed to other methods.
        }

}


\value{

    print, plot and summary reports for an object of class \code{timeSeries}.
        
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

## plot -  
   plot(EDHEC[,1], type = "o", col = "steelblue", 
     main = "EDHEC", xlab = "1997", ylab = "Return")
    
## summary -
   summary(EDHEC)
}


\keyword{chron}

