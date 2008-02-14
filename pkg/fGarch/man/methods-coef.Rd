\name{coef-methods}


\docType{methods}


\alias{coef-methods}
\alias{coef,ANY-method}
\alias{coef,fGARCH-method}
\alias{coef,fGARCHSPEC-method}


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
        Extractor function for coefficients from a fitted
        GARCH model.
        }
    \item{object = "fGARCHSPEC"}{
        Extractor function for coefficients from a GARCH
        specification structure.
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
## garchSpec -
   # Use default parameters beside alpha:
   spec = garchSpec(model = list(alpha = c(0.05, 0.05)))
   spec
   coef(spec)
   
## garchSim -
   # Simulate an univariate "ts" series
   x = garchSim(spec, n = 200)
  
## garchFit - 
   fit = garchFit( ~ garch(1, 1), data = x)
   
## coef - 
   coef(fit)
}


\keyword{models}

