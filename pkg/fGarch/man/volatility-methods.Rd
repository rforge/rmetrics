\name{volatility-methods}


\docType{methods}


\alias{volatility-methods}
\alias{volatility,ANY-method}
\alias{volatility,fGARCH-method}


\title{Extract GARCH Model Volatility}


\description{

    Extracts volatility from a fitted GARCH object. 
    
}


\usage{
\S4method{volatility}{fGARCH}(object, type = c("sigma", "h"), \dots)
}


\arguments{

    \item{object}{
        an object of class \code{fGARCH} as returned from the function
        \code{garchFit()}.
        }  
    \item{type}{
        a character string denoting if the conditional standard
        deviations \code{"sigma"} or the variances \code{"h"} should 
        be returned.
        }
    \item{\dots}{
        additional arguments to be passed.
        }   

}


\note{

    \code{volatility} is a generic function which extracts volatility values 
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
   
## volatility - 
   volatility(fit)
}


\keyword{models}

