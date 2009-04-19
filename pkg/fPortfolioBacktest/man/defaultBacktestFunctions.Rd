\name{defaultBacktestFunctions}


\alias{equidistWindows}
\alias{tangencyStrategy}
\alias{emaSmoother}


\title{User defined functions to perform portfolio backtesting}


\description{
  
    Default windows, strategy and smoothing functions used for portfolio 
    backtesting.
    
}
    

\usage{
equidistWindows(data, backtest = portfolioBacktest())

tangencyStrategy(data, spec = portfolioSpec(), constraints = "LongOnly", 
    backtest = portfolioBacktest())
    
emaSmoother(weights, spec, backtest)
}



\arguments{


    \item{data}{
        a multivariate time series described by an S4 object of class
        \code{timeSeries}. If your timeSerie is not a \code{timeSeries}
        object, consult the generic function \code{as.timeSeries} to 
        convert your time series.
        }
    \item{backtest}{
        an S4 object of class \code{fPFOLIOBACKTEST} as returned by the 
        function \code{portfolioBacktest}.
        }  
    \item{spec}{
        an S4 object of class \code{fPFOLIOSPEC} as returned by the function
        \code{portfolioSpec}.
        }
    \item{constraints}{
        a character string vector, containing the constraints of the form\cr
        \code{"minW[asset]=percentage"} for box constraints resp. \cr
        \code{"maxsumW[assets]=percentage"} for sector constraints.
        }
    \item{weights}{
	    a numeric vector, containing the portfolio weights of an asset
	}

}


\details{
    
    \bold{equidistWindows:}\cr   
    
    Defines equal distant rolling windows.
       
    The function requires two arguments: \code{data} and 
    \code{backtest}, see above.  To assign the horizon 
    value to the backtest specification structure, use the function 
    \code{setWindowsHorizon}. 

    
    \bold{tangencyStrategy:}\cr   
    
    A pre-defined tangency portfolio strategy.
       
    The function requires four arguments: \code{data}, \code{spec}, 
    \code{constraints} and \code{backtest}, see above.

    
    \bold{emaSmoother:}\cr   
    
    A pre-defined weights smoother (EMA) for portfolio backtesting.
       
    The function requires three arguments: \code{weights}, \code{spec} 
    and \code{backtest}, see above.  To assign initial starting weights, 
    smoothing parameter (lambda) or whether to perform double smoothing 
    to the backtest specification structure, use the functions 
    \code{setSmootherInitialWeights}, \code{setSmootherLambda}
    and \code{setSmootherDoubleSmoothing}, respectively.
    

}


\value{
    
    \code{equidistWindows}\cr
    function returns the "from" and "to" dates of the rolling window 
    in a list form.

    \code{tangencyStrategy}\cr
    function returns a S4 object of class \code{"fPORTFOLIO"}.
 
    \code{emaSmoother}\cr
    function returns a numeric vector of smoothed weights.
    
}



\references{

W\"urtz, D., Chalabi, Y., Chen W., Ellis A. (2009);
    \emph{Portfolio Optimization with R/Rmetrics}, 
    Rmetrics eBook, Rmetrics Association and Finance Online, Zurich.
    
}


\examples{
## Show default rolling windows function - 
   equidistWindows

## Show default portfolio strategy function -
   tangencyStrategy

## Swhow default weights smoother function -   
   emaSmoother
}


\keyword{models}

