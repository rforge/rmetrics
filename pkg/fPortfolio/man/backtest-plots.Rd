\name{backtest-plots}


\alias{backtestPlot}

\alias{backtestAssetsPlot}
\alias{backtestWeightsPlot}
\alias{backtestRebalancePlot}
\alias{backtestPortfolioPlot}
\alias{backtestDrawdownPlot}
\alias{backtestReportPlot}


\title{Portfolio backtesting plots}


\description{

    Creates and displays plots of cumulative assets returns, of 
    portfolio weights, of rebalanced weights, of drawdowns 
    and of a report summary for backtesting.

}


\usage{   
% We should add a generic plot function for objects of class smoothed
% portfolios. DW
%
backtestPlot(object, which="all", labels=TRUE, legend=TRUE, at=NULL, 
  format=NULL, cex=0.6, font=1, family="mono")

backtestAssetsPlot(object, labels=TRUE, legend=TRUE, at=NULL, format=NULL)
backtestWeightsPlot(object, labels=TRUE, legend=TRUE, at=NULL, format=NULL)
backtestRebalancePlot(object, labels=TRUE, legend=TRUE, at=NULL, format=NULL)
backtestPortfolioPlot(object, labels=TRUE, legend=TRUE, at=NULL, format=NULL)
backtestDrawdownPlot(object, labels=TRUE, legend=TRUE, at=NULL, format=NULL)
backtestReportPlot(object, cex=0.6, font=1, family="mono")
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
        a logical flag, determining if the graph should be labeled
        automatically. This is the default case \code{labels=TRUE}. If 
        set to \code{FALSE} then the graph will be displayed
        undecorated and the user can it decorate by himself.
        }
    \item{legend}{
        a logical flag, determining if to the graph a legend should
        be added. This is the default case \code{labels=TRUE}. If 
        set to \code{FALSE} then the graph will be displayed
        undecorated and the user can it decorate by himself.
        }
    \item{at}{
        if NULL the time-axis ticks will be selected automatically. If
        \code{at} is a vector of \code{timeData} character formatted 
        dates then the axis ticks ar taken from this vector. 
        }
    \item{format}{
        if NULL the time-axis ticks are labeled automatically. If
        \code{format} is a POSIX format string, tthen the label
        formats are taken from this string.
        }
    \item{cex, font, family}{
        font size, font and font family specification for the report.
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


\keyword{models}

