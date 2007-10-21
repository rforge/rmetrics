\name{PortfolioBacktestesting}

\alias{PortfolioBacktestesting}

\alias{portfolioBacktesting}

\alias{print.portfolioBacktest}
\alias{plot.portfolioBacktest}
\alias{summary.portfolioBacktest}

\title{Portfolio Backtestesting}


\description{
  
    Tests a portfolio by a rolling backtest.
    
}
    

\usage{  
portfolioBacktesting(formula, data, spec = portfolioSpec(), constraints = NULL, 
    portfolio = "minvariancePortfolio", horizon = "12m", smoothing = "6m", 
    trace = TRUE)

\method{print}{portfolioBacktest}(x, \dots)    
\method{plot}{portfolioBacktest}(x, which = "all", \dots)
\method{summary}{portfolioBacktest}(object, \dots)
}



\arguments{


    \item{constraints}{
        a character string vector, containing the constraints of the form\cr
        \code{"minW[asset]=percentage"} for box constraints resp. \cr
        \code{"maxsumW[assets]=percentage"} for sector constraints.
        }    
    \item{data}{
        an object of class \code{timeSeries}.
        }
    \item{formula}{
        a formula describing the benchmark and assets used for backtesting
        in the form \code{backtest ~ assetA + ... + assetZ}. Here,
        \code{backtest} and \code{asset*} are column names of the
        \code{data} set.
        }
    \item{horizon}{
        a character string, by default \code{"12m"}, which denotes 12
        months. The period over which the portfolio is rolled.
        }
    \item{object}{
        an object of class \code{portfolioBacktest} as returned by
        the function \code{portfolioBacktesting}.
        }
    \item{portfolio}{
        a character string, the function name of a user defined 
        portfolio function, which defines the rolling portfolio
        strategy. The default value is \code{"minvariancePortfolio"}.
        }   
    \item{smoothing}{
        a character string, by default \code{"6m"}, which denotes 6
        months. The period over which the rolling weights are smoothed
        by an exponential moving average.
        }
    \item{spec}{
        an S4 object of class \code{fPFOLIOSPEC} that specifies a 
        portfolio, see \code{\link{PortfolioSpec}} for a description
        of the slots.
        }    
    \item{trace}{
        a logical value. Should the calculation be traced?
        } 
    \item{x}{
        an object of class \code{portfolioBacktest} as returned by
        the function \code{portfolioBacktesting}.
        }
    \item{which}{
        a character string, which graphs should be dsiplayed?
        By default \code{"all"}, or alternatively one between \code{"1"}
        or \code{"4"}.  
        }  
    \item{\dots}{
        arguments to be passed.
        }

}


%\value{}
  

%\references{}


\seealso{

   \code{\link{PortfolioClass}},
   \code{\link{PortfolioData}},
   \code{\link{PortfolioSpec}},
   \code{\link{PortfolioConstraints}}.

}


\author{

    Diethelm Wuertz this Rmetrics port.
    
}


\examples{
## ...
}


\keyword{models}

