\name{portfolioBacktesting}


\alias{portfolioBacktesting}
\alias{portfolioSmoothing}


\title{Portfolio Backtesting}


\description{
  
    Tests a portfolio by a rolling backtest.
    
}
    

\usage{  
portfolioBacktesting(formula, data, spec = portfolioSpec(), 
    constraints = "LongOnly", backtest = portfolioBacktest(), 
    trace = TRUE)

portfolioSmoothing(object, backtest, trace = TRUE)    
}


\arguments{
    \item{formula}{
        a formula describing the benchmark and assets used for backtesting
        in the form \code{backtest ~ assetA + ... + assetZ}. Here,
        \code{backtest} and \code{asset*} are column names of the
        \code{data} set.
        }
    \item{data}{
        an object of class \code{timeSeries}.
        }
    \item{spec}{
        an S4 object of class \code{fPFOLIOSPEC} as returned by the function
        \code{portfolioSpec}.
        }         
    \item{constraints}{
        a character string value or vector defining the constraints, for
        details we refer to \code{portfolioConstraints}.
        }      
    \item{backtest}{
        an S4 object of class \code{fPFOLIOBACKTEST} as returned by the function
        \code{portfolioBacktest}.
        }  
    \item{object}{
        a list as returned by the function \code{portfolioBacktesting}.
        }
    \item{trace}{
        a logical flag, by default TRUE. Should the backtersting be traced?
        }
}


\references{

Wuertz, D., Chalabi, Y., Chen W., Ellis A. (2009);
    \emph{Portfolio Optimization with R/Rmetrics}, 
    Rmetrics eBook, Rmetrics Association and Finance Online, Zurich.
    
}



\keyword{models}
