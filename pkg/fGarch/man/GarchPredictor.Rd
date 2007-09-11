\name{garchPredictor}

\alias{garchPredictor}

\alias{predict.fGARCH}


\title{GARCH Prediction Function}


\description{

    Predicts a time series from a fitted GARCH object.

}


\usage{
    \method{predict}{fGARCH}(object, n.ahead = 10, trace = FALSE, \dots)
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

