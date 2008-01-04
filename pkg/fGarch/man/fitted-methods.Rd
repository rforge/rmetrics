\name{fitted-methods}


\docType{methods}


\alias{fitted-methods}
\alias{fitted,ANY-method}
\alias{fitted,fGARCH-method}


\title{Extract GARCH Model Fitted Values}


\description{

    Extracts fitted values from a fitted GARCH object. 
    
}


\section{Methods}{
    \describe{

    \item{object = "ANY"}{
        Generic function. 
        }
    \item{object = "fGARCH"}{
        Extractor function for fitted values.
        }
        
    }
}


\note{

    \code{fitted} is a generic function which extracts fitted values 
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
   
## fitted - 
   fitted = fitted(fit)
   head(fitted)
   class(fitted)
   
## slot - 
   fitted = slot(fit, "fitted")
   head(fitted)
   class(fitted)
}


\keyword{models}

