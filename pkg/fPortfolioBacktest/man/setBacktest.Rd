\name{setBacktest}


\alias{setBacktest}

\alias{setWindowsFun<-} 
\alias{setWindowsParams<-}
\alias{setWindowsHorizon<-} 

\alias{setStrategyFun<-}
\alias{setStrategyParams<-} 
\alias{setSmootherFun<-}
\alias{setSmootherParams<-} 
\alias{setSmootherLambda<-} 
\alias{setSmootherDoubleSmoothing<-} 
\alias{setSmootherInitialWeights<-}
\alias{setSmootherSkip<-} 

\title{Specification of backtesting portfolios}


\description{
  
    Functions to set specifications for portfolio backtesting.
    \cr
    
    The functions are:
    
    \tabular{ll}{
   	\code{setWindowsFun} \tab Sets Windows function, \cr
   	\code{setWindowsParams} \tab Sets additional parameters for rolling windows function, \cr   	\code{setWindowsHorizon} \tab Sets Windows horizon, \cr   	\code{setStrategyFun} \tab Sets the portfolio Strategy function, \cr
   	\code{setStrategyParams} \tab Sets additional parameters for Strategy function, \cr   	\code{setSmootherFun} \tab Sets the Smoother function, \cr
   	\code{setSmootherParams} \tab Sets additional parameters for Smoother function, \cr	 \code{setSmootherLambda} \tab Sets the smoothing parameter Lambda, \cr   	\code{setSmootherDoubleSmoothing} \tab Sets setting for double smoothing, \cr   	\code{setSmootherInitialWeights} \tab Sets the initial weights to used in the smoothing, \cr
	\code{setSmootherSkip} \tab Sets the number of skipped months. }
    
}


\usage{
setWindowsFun(backtest) <- value
setWindowsParams(backtest) <- value
setWindowsHorizon(backtest) <- value

setStrategyFun(backtest) <- value
setStrategyParams(backtest) <- value
setSmootherFun(backtest) <- value
setSmootherParams(backtest) <- value
setSmootherLambda(backtest) <- value
setSmootherDoubleSmoothing(backtest) <- value
setSmootherInitialWeights(backtest) <- value
setSmootherSkip(backtest) <- value
}


\arguments{

    \item{backtest}{
        an S4 object of class \code{fPFOLIOBACKTEST}, the specification 
        to be modified, by default the default of the function 
        \code{portfolioBacktest()}.
        }
    \item{value}{
        a value for that component of \code{backtest} to be set.  
        Note for setting Params value is a list.
        }

}       


\details{
    
    The function \code{portfolioBacktest()} allows to set the values 
    for the specification structure from scratch. 
    
    To  modify individual settings one can use the set functions.
 
}


\references{

Wuertz, D., Chalabi, Y., Chen W., Ellis A. (2009);
    \emph{Portfolio Optimization with R/Rmetrics}, 
    Rmetrics eBook, Rmetrics Association and Finance Online, Zurich.
    
}


\keyword{models}

