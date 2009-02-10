\name{backtestPlots}


\alias{backtestPlots}

\alias{backtestAssetsPlot}
\alias{backtestWeightsPlot}
\alias{backtestRebalancePlot}
\alias{backtestPortfolioPlot}
\alias{backtestDrawdownPlot}
\alias{backtestReportPlot}


\title{Portfolio Backtest Plots}


\description{

    Displays plots of assets, weights, rebalancing, portfolio, drawdown 
    and report for backtesting.

}


\usage{   
backtestPlots(object, which = "all", labels = TRUE, ...)

backtestAssetsPlot(object, labels = TRUE, ...)
backtestWeightsPlot(object, labels = TRUE, ...)
backtestRebalancePlot(object, labels = TRUE, ...)
backtestPortfolioPlot(object, labels = TRUE, ...)
backtestDrawdownPlot(object, labels = TRUE, ...)
backtestReportPlot(object, ...)

}


\arguments{

    \item{object}{
        a list, returned from running the function 
        \code{portfolioSmoothing}.
        }
   \item{which}{
        a numeric value, specifying which backtest plot to display.  
	Default is "all" which plots all 6 available backtest plots.
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

    These backtest plots summarises the results obtained
    from portfolio backtesting.
    
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

Wuertz, D., Chalabi, Y., Chen W., Ellis A. (2009);
    \emph{Portfolio Optimization with R/Rmetrics}, 
    Rmetrics eBook, Rmetrics Association and Finance Online, Zurich.
    
}


\keyword{models}

