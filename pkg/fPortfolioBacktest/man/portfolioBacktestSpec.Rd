\name{portfolioBacktest}


\alias{portfolioBacktest}


\title{Specification of portfolio backtesting}


\description{
  
    Specifies how the portfolio backtesting is performed.
    
}


\usage{
portfolioBacktest(
    windows = list(
        windows = "equidistWindows", 
        params = list(horizon = "12m")),
    strategy = list(
        strategy = "tangencyStrategy", 
        params = list()),
    smoother = list(
        smoother = "emaSmoother", 
        params = list(doubleSmoothing = TRUE, 
        lambda = "3m", skip = 0, 
        initialWeights = NULL)),
    messages = list())
}


\arguments{

    \item{windows}{
        a list, containing different arguments: windows, params 
        (horizon).
        }
    \item{strategy}{
        a list, containing different arguments: strategy, params.
        }
    \item{smoother}{
        a list, containing different arguments: smoother, params. 
        (doubleSmoothing, lambda, skip, initialWeights).
        }
     \item{messages}{
	    a list containing the backtesting messages.
	    }

}       


\details{     
    
    \bold{Retrieving and Modifying Specification Settings:}
                
    Information about the current portfolio backtest specification can be retrieved 
    by \code{"get"} functions. These include:
    
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
        
    To modify the setting from a portfolio specification use the 
    \code{"set"} functions:
    
    \tabular{ll}{
   	\code{setWindowsFun} \tab Sets Windows function, \cr   	
   	\code{setWindowsHorizon} \tab Sets Windows horizon, \cr   	
   	\code{setStrategyFun} \tab Sets the portfolio Strategy function, \cr   	
   	\code{setSmootherFun} \tab Sets the Smoother function, \cr	 
   	\code{setSmootherLambda} \tab Sets the smoothing parameter Lambda, \cr   	
   	\code{setSmootherDoubleSmoothing} \tab Sets setting for double smoothing, \cr   	
   	\code{setSmootherInitialWeights} \tab Sets the initial weights to used in the smoothing, \cr
	\code{setSmootherSkip} \tab Sets the number of skipped months, \cr
    \code{setType} \tab Sets type of portfolio optimization. }
        
    For details we refer to \code{link{setBacktest}}.
           
}


\value{
  
    returns an S4 object of class \code{"fPFOLIOBACKTEST"}.

}


\references{

W\"urtz, D., Chalabi, Y., Chen W., Ellis A. (2009);
    \emph{Portfolio Optimization with R/Rmetrics}, 
    Rmetrics eBook, Rmetrics Association and Finance Online, Zurich.
    
}


\examples{
## portfolioBacktest -
   # Show Default Portfolio Backtest Specifications:
   backtest = portfolioBacktest()
   
## getWindowsFun -
   # Get rolling windows function
   getWindowsFun(backtest) 
      
## setWindowsHorizon -
   # Modify Rolling windows horizon
   setWindowsHorizon(backtest) <- "12m"
   backtest
}


\keyword{models}

