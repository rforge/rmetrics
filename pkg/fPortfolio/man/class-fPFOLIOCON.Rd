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


\references{

Wuertz, D., Chalabi, Y., Chen W., Ellis A. (2009);
    \emph{Portfolio Optimization with R/Rmetrics}, 
    Rmetrics eBook, Rmetrics Association and Finance Online, Zurich.
    
}


\examples{
## getClass-
   getClass("fPFOLIOCON")
   
## getSlots - 
   getSlots("fPFOLIOCON")   
    
## data -
   Data = SMALLCAP.RET
   print(head(Data))
   print(class(Data))

## portfolioConstraints -
   Spec = portfolioSpec()
   setTargetReturn(Spec) = mean(Data)
   Constraints = "LongOnly"
   portfolioConstraints(Data, Spec, Constraints)
}


\keyword{models}

