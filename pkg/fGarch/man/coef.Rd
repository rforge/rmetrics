\name{coef}


\alias{coef.fGARCH}


\title{Extract GARCH Model Coefficients}


\description{

    Extracts coefficients from a fitted GARCH object. 
    
}


\usage{
\method{coef}{fGARCH}(object, \dots)
}


\arguments{

    \item{object}{
        an object of class \code{fGARCH} as returned from the function
        \code{garchFit()}.
        }  
    \item{\dots}{
        additional arguments to be passed.
        }   

}


\note{

    \code{coef} is a generic function which extracts coefficients 
    from objects returned by modeling functions. \code{coefficients} 
    is an alias for it. 

}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## garchSim -
   x = garchSim(n = 500)
  
## garchFit
   fit = garchFit(~garch(1, 1), data = x)
   
## coef - 
   coef(fit)
}


\keyword{models}

