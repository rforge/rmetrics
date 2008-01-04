\name{predict-methods}


\docType{methods}


\alias{predict-methods}
\alias{predict,ANY-method}
\alias{predict,fGARCH-method}


\title{GARCH Prediction Function}


\description{

    Predicts a time series from a fitted GARCH object.

}


\usage{
\S4method{predict}{fGARCH}(object, n.ahead = 10, trace = FALSE, \dots)
}


\arguments{

    \item{n.ahead}{
        an integer value, denoting the number of steps to be forecasted,  
        by default 10.
        }
    \item{object}{
        an object of class \code{fGARCH} as returned by the function
        \code{garchFit}.
        }   
    \item{trace}{
        a logical flag. Should the prediction process be traced? By 
        default \code{trace=FALSE}.
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
        Predict function for objects of class \code{"fGARCH"}.
        }
        
    }
}


\value{

    returns a data frame with the foloowing columns:
    \code{"meanForecast"}, \code{meanError}, and \code{"standardDeviation"}.
        
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## garchSpec -
   spec = garchSpec()
   spec

## garchSim -
   x = garchSim(model = spec@model, n = 500)
   head(x) 

## garchFit - 
   fit = garchFit(~ garch(1, 1), data = x)
   fit
   
## predict -
   predict(fit, n.ahead = 10)
}


\keyword{models}

