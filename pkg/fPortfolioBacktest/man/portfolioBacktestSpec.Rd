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

