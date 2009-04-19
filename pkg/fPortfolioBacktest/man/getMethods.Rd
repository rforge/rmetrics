\name{getBacktest}

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
%\alias{getMessages} Already defined in fPortfolio


\title{Portfolio Backtest Extractors}


\description{
  
    Extractor functions to get information from objects
    of class fPFOLIOBACKTEST.
    
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
    
## Extract Windows Information -  	
   getWindows(backtestSpec) 
   getWindowsFun(backtestSpec) 
   getWindowsParams(backtestSpec) 
   getWindowsHorizon(backtestSpec) 
   
## Extract Strategy Information - 
   getStrategy(backtestSpec) 
   getStrategyFun(backtestSpec) 
   getStrategyParams(backtestSpec) 
   
## Extract Smoother Information - 
   getSmoother(backtestSpec) 
   getSmootherFun(backtestSpec) 
   getSmootherParams(backtestSpec) 						
   getSmootherLambda(backtestSpec) 
   getSmootherDoubleSmoothing(backtestSpec) 
   getSmootherInitialWeights(backtestSpec) 
   getSmootherSkip(backtestSpec) 
}


\keyword{models}

