\name{fFOLIOCON-class}


\alias{fPFOLIOCON}
\alias{class-fPFOLIOCON}

\alias{fPFOLIOCON-class}

\alias{show,fPFOLIOCON-method}


\title{Portfolio Constraints Handling}


\description{
  
    Creates a fPFOLIOCON object.
      
}


\usage{
\S4method{show}{fPFOLIOCON}(object)
}


\arguments{

    \item{object}{
        an object of class \code{fPFOLIOCON} as returned by the function
        \code{portfolioData}.
        }

}


%\references{}


\examples{
## getClass-
   getClass("fPFOLIOCON")
   
## getSlots - 
   getSlots("fPFOLIOCON")   
    
## data -
   data = as.timeSeries(data(smallcap.ts))

## portfolioConstraints -
   spec = portfolioSpec()
   setTargetReturn(spec) = mean(data)
   portfolioConstraints(data, spec, "LongOnly")
}


\keyword{models}

