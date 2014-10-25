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
\S4method{residuals}{fGARCH}(object, standardize = FALSE)
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


\details{

    The function extracts the \code{@residuals} slot from an
    object of class \code{"fGARCH"} as returned by the function
    \code{garchFit}.
    
    The class of the returned value depends on the input to the
    function \code{garchFit} who created the object. The returned
    value is always of the same class as the input object to the
    argument \code{data} in the function \code{garchFit}, i.e. if
    you fit a \code{"timeSeries"} object, you will get back from 
    the function \code{fitted} also a \code{"timeSeries"} object, 
    if you fit an object of class \code{"zoo"}, you will get back 
    again a \code{"zoo"} object. The same holds for a \code{"numeric"}
    vector, for a \code{"data.frame"}, and for objects of class
    \code{"ts", "mts"}.
    
    In contrast, the slot itself returns independent of the class 
    of the data input always a numceric vector, i.e. the function 
    call r\code{slot(object, "fitted")} will return a numeric vector.
    
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

