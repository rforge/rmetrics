\name{getBacktest}

\alias{getWindows.fPFOLIOBACKTEST}\alias{getWindowsFun.fPFOLIOBACKTEST}\alias{getWindowsParams.fPFOLIOBACKTEST}\alias{getWindowsHorizon.fPFOLIOBACKTEST}\alias{getStrategy.fPFOLIOBACKTEST}\alias{getStrategyFun.fPFOLIOBACKTEST}\alias{getStrategyParams.fPFOLIOBACKTEST}\alias{getSmoother.fPFOLIOBACKTEST}\alias{getSmootherFun.fPFOLIOBACKTEST}\alias{getSmootherParams.fPFOLIOBACKTEST}						\alias{getSmootherLambda.fPFOLIOBACKTEST}\alias{getSmootherDoubleSmoothing.fPFOLIOBACKTEST}\alias{getSmootherInitialWeights.fPFOLIOBACKTEST}\alias{getSmootherSkip.fPFOLIOBACKTEST}\alias{getMessages.fPFOLIOBACKTEST}

\alias{getWindows}\alias{getWindowsFun}\alias{getWindowsParams}\alias{getWindowsHorizon}\alias{getStrategy}\alias{getStrategyFun}\alias{getStrategyParams}\alias{getSmoother}\alias{getSmootherFun}\alias{getSmootherParams}						\alias{getSmootherLambda}\alias{getSmootherDoubleSmoothing}\alias{getSmootherInitialWeights}\alias{getSmootherSkip}\alias{getMessages}

\title{Portfolio Backtesting Specification Extractors}


\description{
  
    A collection and description of functions 
    allowing to get information about an object
    of class fPFOLIOBACKTEST. \cr
    
    The functions are:
    
    \tabular{ll}{
  	\code{getWindows} \tab Extract Windows slot, \cr   	
  	\code{getWindowsFun} \tab Extract Windows function, \cr	
  	\code{getWindowsParams} \tab Extract a list of Windows specific parameters, \cr   	
  	\code{getWindowsHorizon} \tab Extract Windows horizon, \cr	
  	\code{getStrategy} \tab Extract Strategy slot, \cr   	
  	\code{getStrategyFun} \tab Extract the portfolio Strategy function, \cr	
  	\code{getStrategyParams} \tab Extract a list of portfolio Strategy specific parameters, \cr  	\code{getSmoother} \tab Extract the Smoother slot, \cr   	\code{getSmootherFun} \tab Extract the Smoother function, \cr	\code{getSmootherParams} \tab Extract a list of Smoothing specific parameters, \cr	\code{getSmootherLambda} \tab Extract the smoothing parameter Lambda, \cr   	\code{getSmootherDoubleSmoothing} \tab Extract setting for double smoothing, \cr   	\code{getSmootherInitialWeights} \tab Extract the initial weights to used in the smoothing, \cr
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
        an object of class \code{fPFOLIOBACKTEST}.}

}


\references{

Wuertz, D., Chalabi, Y., Chen W., Ellis A. (2009);
    \emph{Portfolio Optimization with R/Rmetrics}, 
    Rmetrics eBook, Rmetrics Association and Finance Online, Zurich.
    
}


\examples{
## get -
   
   # Create default backtest specification structure:
   backtest = portfolioBacktest()
   
   # Get:
   getWindowsFun(backtest)
   getSmootherFun(backtest)
   getSmootherInitialWeights(backtest)  
}


\keyword{models}

