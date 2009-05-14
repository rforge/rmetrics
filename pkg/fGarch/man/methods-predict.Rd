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
\S4method{predict}{fGARCH}(object, n.ahead = 10, trace = FALSE, mse = c("cond","uncond"), \dots)
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
     \item{mse}{
	If set to \code{"cond"}, \code{meanError} is defined as the conditional mean errors 
	\eqn{\sqrt{E_t[x_{t+h}-E_t(x_{t+h})]^2}}. If set to \code{"uncond"}, it is defined as 
	\eqn{\sqrt{E[x_{t+h}-E_t(x_{t+h})]^2}}.
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
    
    The number of records equals the number of forecasting steps
    \code{n.ahead}.
        
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## garchFit - 
   # Parameter Estimation of Default GARCH(1,1) Model:
   fit = garchFit(~ garch(1, 1), data = garchSim(), trace = FALSE)
   fit

## predict -
   predict(fit, n.ahead = 10)
   predict(fit, n.ahead = 10,mse="uncond")
}


\keyword{models}

