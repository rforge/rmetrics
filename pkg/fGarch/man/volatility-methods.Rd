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


\section{Methods}{
    \describe{

    \item{object = "ANY"}{
        Generic function.
        }
    \item{object = "fGARCH"}{
        Extractor function for volatility or standard deviation from 
        an object of class \code{"fGARCH"}.
        }
        
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
## Swiss Pension func Index -
   x = as.timeSeries(data(LPP2005REC))
  
## garchFit
   fit = garchFit(LPP40 ~ garch(1, 1), data = 100*x, trace = FALSE)
   fit
   
## volatility - 
   # Standard Deviation:
   volatility = volatility(fit, type = "sigma")
   head(volatility)
   class(volatility)
   # Variance:
   volatility = volatility(fit, type = "h")
   head(volatility)
   class(volatility)
   
## slot - 
   volatility = slot(fit, "sigma.t")
   head(volatility)
   class(volatility)
   volatility = slot(fit, "h.t")
   head(volatility)
   class(volatility)
}


\keyword{models}

