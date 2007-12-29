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
        number of steps to be forecasted, an integer value, by default 10.
        }
    \item{object}{
        an object of class \code{fGARCH} as returned from the function
        \code{garchFit()}.
        }   
    \item{trace}{
        a logical flag. Should the prediction process be printed? By 
        default \code{trace=FALSE}.
        }  
    \item{\dots}{
        additional arguments to be passed.
        }   

}


\section{Methods}{
    \describe{

    \item{object = "ANY"}{
        Generic function
        }
    \item{object = "fGARCH"}{
        Predict
        }
        
    }
}


\value{

    returns ...
        
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
   # fit = garchFit(~garch(1, 1), data = x)
   # print(fit)
   ## Interactive Plot:
   ## plot(fit)
   ## Batch Plot:
   # plot(fit, which = 3)
   # summary(fit)
   
## predict -
   # predict(object, n.ahead = 10)
}


\keyword{models}

