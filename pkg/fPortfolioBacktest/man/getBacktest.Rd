\name{getBacktest}

\alias{getWindows.fPFOLIOBACKTEST}
\alias{getWindowsFun.fPFOLIOBACKTEST}
\alias{getWindowsParams.fPFOLIOBACKTEST}
\alias{getWindowsHorizon.fPFOLIOBACKTEST}
\alias{getStrategy.fPFOLIOBACKTEST}
\alias{getStrategyFun.fPFOLIOBACKTEST}
\alias{getStrategyParams.fPFOLIOBACKTEST}
\alias{getSmoother.fPFOLIOBACKTEST}
\alias{getSmootherFun.fPFOLIOBACKTEST}
\alias{getSmootherParams.fPFOLIOBACKTEST}						
\alias{getSmootherLambda.fPFOLIOBACKTEST}
\alias{getSmootherDoubleSmoothing.fPFOLIOBACKTEST}
\alias{getSmootherInitialWeights.fPFOLIOBACKTEST}
\alias{getSmootherSkip.fPFOLIOBACKTEST}
\alias{getMessages.fPFOLIOBACKTEST}

\alias{getWindows}
\alias{getWindowsFun}
\alias{getWindowsParams}
\alias{getWindowsHorizon}
\alias{getStrategy}
\alias{getStrategyFun}
\alias{getStrategyParams}
\alias{getSmoother}
\alias{getSmootherFun}
\alias{getSmootherParams}						
\alias{getSmootherLambda}
\alias{getSmootherDoubleSmoothing}
\alias{getSmootherInitialWeights}
\alias{getSmootherSkip}
\alias{getMessages}

\title{Portfolio Backtest Extractors}


\description{
  
    Functions to extract information from an object
    of class fPFOLIOBACKTEST. \cr
    
    The functions are:
    
    \tabular{ll}{
  	\code{getWindows} \tab Extract Windows slot, \cr   	
  	\code{getWindowsFun} \tab Extract Windows function, \cr	
  	\code{getWindowsParams} \tab Extract a list of Windows specific parameters, \cr   	
  	\code{getWindowsHorizon} \tab Extract Windows horizon, \cr	
  	\code{getStrategy} \tab Extract Strategy slot, \cr   	
  	\code{getStrategyFun} \tab Extract the portfolio Strategy function, \cr	
  	\code{getStrategyParams} \tab Extract a list of portfolio Strategy specific parameters, \cr  	
  	\code{getSmoother} \tab Extract the Smoother slot, \cr   	
  	\code{getSmootherFun} \tab Extract the Smoother function, \cr	
  	\code{getSmootherParams} \tab Extract a list of Smoothing specific parameters, \cr	
  	\code{getSmootherLambda} \tab Extract the smoothing parameter Lambda, \cr   	
  	\code{getSmootherDoubleSmoothing} \tab Extract setting for double smoothing, \cr   	
  	\code{getSmootherInitialWeights} \tab Extract the initial weights to used in the smoothing, \cr
	\code{getSmootherSkip} \tab Extract the number of skipped months, \cr  	
	\code{getMessages} \tab Extract the Message slot.}  
}
    

\usage{
\method{getWindows}{fPFOLIOBACKTEST}(object)
\method{getWindowsFun}{fPFOLIOBACKTEST}(object)
\method{getWindowsParams}{fPFOLIOBACKTEST}(object)
\method{getWindowsHorizon}{fPFOLIOBACKTEST}(object)
\method{getStrategy}{fPFOLIOBACKTEST}(object)
\method{getStrategyFun}{fPFOLIOBACKTEST}(object)
\method{getStrategyParams}{fPFOLIOBACKTEST}(object)
\method{getSmoother}{fPFOLIOBACKTEST}(object)
\method{getSmootherFun}{fPFOLIOBACKTEST}(object)
\method{getSmootherParams}{fPFOLIOBACKTEST}(object)
\method{getSmootherLambda}{fPFOLIOBACKTEST}(object)
\method{getSmootherDoubleSmoothing}{fPFOLIOBACKTEST}(object)
\method{getSmootherInitialWeights}{fPFOLIOBACKTEST}(object)
\method{getSmootherSkip}{fPFOLIOBACKTEST}(object)
\method{getMessages}{fPFOLIOBACKTEST}(object)
}


\arguments{

    \item{object}{
        an object of class \code{fPFOLIOBACKTEST} as returned by 
        function \code{portfolioBacktest}.
        }

}


\references{

W\"urtz, D., Chalabi, Y., Chen W., Ellis A. (2009);
    \emph{Portfolio Optimization with R/Rmetrics}, 
    Rmetrics eBook, Rmetrics Association and Finance Online, Zurich.
    
}


\examples{
## portfolioBacktest Specification -
   backtestSpec = portfolioBacktest()
   backtestSpec
   
## Get Windows Information - 
   getWindows(backtestSpec)
   getWindowsFun(backtestSpec)
   getWindowsParams(backtestSpec)
   getWindowsHorizon(backtestSpec) 
 
## Get Strategy Information -  
   getStrategy(backtestSpec) 
   getStrategyFun(backtestSpec) 
   getStrategyParams(backtestSpec) 
 
## Get Smoother Information -  
   getSmoother(backtestSpec)  
   getSmootherFun(backtestSpec) 
   getSmootherParams(backtestSpec) 
   getSmootherLambda(backtestSpec) 
   getSmootherDoubleSmoothing(backtestSpec)
   getSmootherInitialWeights(backtestSpec)
   getSmootherSkip(backtestSpec)  
 
## Get Messages Information -  	
   getMessages(backtestSpec) 
}


\keyword{models}

