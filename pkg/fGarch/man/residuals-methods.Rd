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
        an object of class \code{"fGARCH"} as returned from the function
        \code{garchFit}.
        }  
    \item{standardize}{
        a logical flag, should the residuals be standardized?
        }   

}


\section{Methods}{
    \describe{

    \item{object = "ANY"}{
        Generic function
        }
    \item{object = "fGARCH"}{
        Extractor function for residual from an object of 
        class \code{"fGARCH"}.
        }
        
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
## Swiss Pension func Index -
   x = as.timeSeries(data(LPP2005REC))
  
## garchFit
   fit = garchFit(LPP40 ~ garch(1, 1), data = 100*x, trace = FALSE)
   fit
   
## residuals - 
   res = residuals(fit)
   head(res)
   class(res)
   
## slot - 
   res = slot(fit, "residuals")
   head(res)
}


\keyword{models}

