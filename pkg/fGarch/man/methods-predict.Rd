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
\S4method{predict}{fGARCH}(object, n.ahead = 10, trace = FALSE, mse = c("cond","uncond"),plot=FALSE,nx=NULL,crit_val=NULL,conf=NULL,\dots)
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
      \item{plot}{If set to \code{TRUE}, the confidence intervals are computed and plotted}
      \item{nx}{The number of observations to be plotted along with the predictions. The default is \code{round(n*0.25)}, where n is the 	sample size.}
      \item{crit_val}{The critical values for the confidence intervals when \code{plot} is set to \code{TRUE}. The intervals are defined 	as \eqn{\hat{x}_{t+h}} + \code{crit_val[2] * meanError} and \eqn{\hat{x}_{t+h}} + \code{crit_val[1] * meanError} if two critical 	values are provided and \eqn{\hat{x}_{t+h} \pm}  \code{crit_val * meanError} if only one is given. If you do not provide critical 	values, they will be computed automatically.
	}	
	\item{conf}{The confidence level for the confidence intervals if \code{crit_val} is not provided. 
	By default it is set to 0.95. The critical values are then computed using the conditional distribution 
	that was chosen to create the \code{object} with \code{garchFit} using the same \code{shape} and \code{skew} 
	parameters. If the conditionnal distribution was set to code{"QMLE"}, the critical values are computed using 
	the empirical distribution of the standardized residuals.
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
   set.seed(123)
   fit = garchFit(~ garch(1, 1), data = garchSim(), trace = FALSE)
   fit

## predict -
   predict(fit, n.ahead = 10)
   predict(fit, n.ahead = 10,mse="uncond")

## predict with plotting: critical values = +- 2

   predict(fit, n.ahead = 10, plot=TRUE, crit_val=2)

## predict with plotting: automatic critical values 
## for different conditional distributions

  set.seed(321)
  fit2 = garchFit(~ garch(1, 1), data = garchSim(), trace = FALSE, cond.dist="sged")

## 95% confidence level
 predict(fit2,n.ahead=20,plot=TRUE) 

 set.seed(444)
 fit3 = garchFit(~ garch(1, 1), data = garchSim(), trace = FALSE, cond.dist="QMLE")

## 90% confidence level and nx=100

 predict(fit3,n.ahead=20,plot=TRUE,conf=.9,nx=100) 

}


\keyword{models}

