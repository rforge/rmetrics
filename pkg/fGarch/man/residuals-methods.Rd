\name{residuals-methods}


\docType{methods}


\alias{residuals-methods}
\alias{residuals,ANY-method}
\alias{residuals,fGARCH-method}


\title{Extract GARCH Model Residuals}


\description{

    Extracts residuals from a fitted GARCH object. 
}


\usage{
\S4method{residuals}{fGARCH}(object, standardize = TRUE)
}


\arguments{

    \item{object}{
        an object of class \code{fGARCH} as returned from the function
        \code{garchFit()}.
        }  
    \item{standardize}{
        a logical flag, should the residuals be standardized?
        }   

}


\note{

    \code{residuals} is a generic function which extracts residual values 
    from objects returned by modeling functions.
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## garchSim -
   x = garchSim(n = 500)
  
## garchFit
   fit = garchFit(~garch(1, 1), data = x)
   
## residuals - 
   residuals(fit)
}


\keyword{models}

