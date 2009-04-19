\name{backtestPlot}


\alias{backtestPlot}

\alias{backtestAssetsPlot}
\alias{backtestWeightsPlot}
\alias{backtestRebalancePlot}
\alias{backtestPortfolioPlot}
\alias{backtestDrawdownPlot}
\alias{backtestReportPlot}


\title{Portfolio Backtest Plots}


\description{

    Creates and displays plots of cumulative assets returns, of 
    portfolio weights, of rebalanced weights, of drawdowns 
    and of a report summary for backtesting.

}


\usage{   
% We should add a generic plot function for objects of class smoothed
% portfolios. DW
%
backtestPlot(object, which = "all", labels = TRUE, \dots)

backtestAssetsPlot(object, labels = TRUE, \dots)
backtestWeightsPlot(object, labels = TRUE, \dots)
backtestRebalancePlot(object, labels = TRUE, \dots)
backtestPortfolioPlot(object, labels = TRUE, \dots)
backtestDrawdownPlot(object, labels = TRUE, \dots)
backtestReportPlot(object, \dots)
}


\arguments{

    \item{object}{
        a list, returned from running the function 
        \code{portfolioSmoothing}.
        }
    \item{which}{
        an integer or string value. If the argument is an integer then it
        specifies which backtest plot should be displayed. If the argument
        take the character value \code{all}, which is the default, then all 
        6 available backtest plots will be displayed.
        }
    \item{labels}{
        a logical flag, determining if the the graph should be labeled
        automatically, which is the default case \code{labels=TRUE}. If 
        set to \code{FALSE} then the graph will be displayed
        undecorated and the user can it decorate by himself.
        }
    \item{\dots}{
        additional arguments passed to the function \code{plot}
       }
    

}


\details{

    These backtest plot summarises the results obtained from portfolio 
    backtesting.
    
    The function \code{backtestAssetsPlot} displays the set of possible
    assets to construct a portfolio. 
    
    The function \code{backtestWeightsPlot} displays the recommended 
	weights for investment. 
    
    The function \code{backtestRebalancePlot} displays the weight 
    changes over time for 
	individual assets and for the portfolio.

    The function \code{backtestPortfolioPlot} displays the daily, 
    benchmark and portfolio series
	of a portfolio backtest.

    The function \code{backtestDrawdownPlot} displays the daily 
    drawdowns for the
	benchmark and the portfolio.

    The function \code{backtestReportPlot} summarises the results 
    from a portfolio backtest.

}


\references{

W\"urtz, D., Chalabi, Y., Chen W., Ellis A. (2009);
    \emph{Portfolio Optimization with R/Rmetrics}, 
    Rmetrics eBook, Rmetrics Association and Finance Online, Zurich.
    
}


\examples{
## Specifications - 
   swxData <- 100 * SWX.RET 
   swxSpec <- portfolioSpec()
   swxBacktest <- portfolioBacktest()
   setWindowsHorizon(swxBacktest) = "18m"
   swxFormula <- LP40 ~ SBI + SPI + SII

## Portfolio Backtesting -
   portfolios <- portfolioBacktesting(
     formula = swxFormula, 
     data = swxData, 
     spec = swxSpec, 
     constraints = "LongOnly", 
     backtest = swxBacktest, 
     trace = TRUE)

## Smoothed Rebalnacing -   
   smoothedPortfolios <- portfolioSmoothing(
     object = portfolios, 
     backtest = swxBacktest)  

## Typical Plots -
   backtestAssetsPlot(smoothedPortfolios)
   backtestWeightsPlot(smoothedPortfolios)
   backtestRebalancePlot(smoothedPortfolios)
   backtestPortfolioPlot(smoothedPortfolios)
   backtestDrawdownPlot(smoothedPortfolios)
   backtestReportPlot(smoothedPortfolios)  

## backtestPlot Summary:  
   backtestPlot(smoothedPortfolios)    
}


\keyword{models}

