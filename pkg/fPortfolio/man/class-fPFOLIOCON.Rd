\name{fFOLIOCON-class}


\alias{fPFOLIOCON}
\alias{fPFOLIOCON-class}

\alias{show.fPFOLIOCON}
\alias{show,fPFOLIOCON-method}


\title{Portfolio Constraints Handling}


\description{
  
    Creates a fPFOLIOCON object.
      
}


\usage{
show.fPFOLIOCON(object)
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
   portfolioConstraints(data, portfolioSpec(), "LongOnly")
}


\keyword{models}

