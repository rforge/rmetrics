\name{coef-methods}


\docType{methods}


\alias{coef-methods}
\alias{coef,ANY-method}
\alias{coef,fGARCH-method}


\title{GARCH Coefficients Methods}


\description{

    Coefficients methods for GARCH Modelling. 

}


\section{Methods}{
    \describe{

    \item{object = "ANY"}{
        Generic function.
        }
    \item{object = "fGARCH"}{
        Coefficients extractor function.
        }
        
    }
}


\note{

    \code{coef} is a generic function which extracts coefficients 
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
   
## coef - 
   coef(fit)
}


\keyword{models}

