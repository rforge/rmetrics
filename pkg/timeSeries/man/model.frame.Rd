\name{model.fram}


\title{Model Frames for Time Series Objects}


\alias{model.frame}
\alias{model.frame.timeSeries}


\description{
    
    Allow to work with model frames for 'timeSeries' objects.               
}


\usage{
\method{model.frame}{timeSeries}(formula, data, ...) 
}


\arguments{

    \item{formula}{ 
        a model formula object. 
        }
    \item{data}{
        an object of class \code{timeSeries}.
        }
    \item{\dots}{
        arguments passed to the function \code{stats::model.frame}.
        }
        
}


\value{

    an object of class \code{timeSeries}.
        
}


\details{

    The function \code{model.frame} is a generic function which returns 
    in the R-ststs framework by default a data.frame with the variables 
    needed to use formula and any ... arguments. In contrast to this
    the method returns an object of class \code{timeSeries} when the
    argument data was not a \code{data.frame} but also an object of class
    \code{timeSeries}.
    
}



\note{

    This function is preliminary and untested.

}


\seealso{

    \code{\link{model.frame}}.
    
}
    

\examples{
## data -  
   # Microsoft Data:
   require(fEcofin)
   setRmetricsOptions(myFinCenter = "GMT")
   MSFT = as.timeSeries(data(msft.dat))[1:12, ]

## model.frame -
   # Extract High's and Low's:
   model.frame( ~ High + Low, data = MSFT)
   # Extract Open Prices and their log10's:
   base = 10
   Open = model.frame(Open ~ log(Open, base = `base`), data = MSFT)
   colnames(Open) <- c("MSFT", "log10(MSFT)")
   Open
    
}


\keyword{chron}

