\name{backtestStats}


\alias{backtestStats}

\alias{rollingSigma}
\alias{rollingVaR}
\alias{rollingCVaR}
\alias{rollingDaR}
\alias{rollingCDaR}
\alias{rollingRiskBudgets}

%\alias{backtestStats.portfolioBacktesting}

%\alias{rollingSigma.portfolioBacktesting}
%\alias{rollingVaR.portfolioBacktesting}
%\alias{rollingCVaR.portfolioBacktesting}
%\alias{rollingDaR.portfolioBacktesting}
%\alias{rollingCDaR.portfolioBacktesting}
%\alias{rollingRiskBudgets.portfolioBacktesting}


\title{Rolling Portfolio Backtest Statistics}


\description{

    Computes rolling statistics for backtest analysis

}


\usage{   
backtestStats(object, FUN = "rollingSigma", \dots)

rollingSigma(object)
rollingVaR(object)
rollingCVaR(object)
rollingDaR(object)
rollingCDaR(object)
%rollingRiskBudgets(object)
%
%\method{backtestStats}{portfolioBacktesting}(object, FUN = "rollingSigma", \dots)
%
%\method{rollingSigma}{portfolioBacktesting}(object)
%\method{rollingVaR}{portfolioBacktesting}(object)
%\method{rollingCVaR}{portfolioBacktesting}(object)
%\method{rollingDaR}{portfolioBacktesting}(object)
%\method{rollingCDaR}{portfolioBacktesting}(object)
%\method{rollingRiskBudgets}{portfolioBacktesting}(object)
}


\arguments{

    \item{object}{
        a list, returned from running the function 
        \code{portfolioSmoothing}.
        }
   \item{FUN}{
        a character string, specifying the name of the rolling statistics 
        function.
        }
   \item{\dots}{
        optional argument to be passed to the rolling statistics 
        function \code{FUN}.
        }
}


\details{
    
    The function \code{rollingSigma} calculates the portfolio risk, 
    Sigma, over time.
    
    The function \code{rollingVaR} calculates a rolling Value at Risk.
    
    The function \code{rollingCVaR} calculates a rolling Conditional 
    Value at Risk.

    The function \code{rollingDaR} calculates a rolling Drawdowns at 
    Risk.

    The function \code{rollingCDaR} calculates a rolling Conditional 
    Drawdowns at Risk.

    %The function \code{rollingRiskBudgets} calculates a rolling Risk 
    %Budget of the portfolio.

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

## Smoothed Rebalancing -   
   smoothedPortfolios <- portfolioSmoothing(
     object = portfolios, 
     backtest = swxBacktest)  
     
## Rolling Variance Sigma - 
   series <- rollingSigma(smoothedPortfolios)
   plot(series, main = "Rolling Variance")

## Rolling Value at Risk -
   series <- rollingVaR(smoothedPortfolios)
   plot(series, main = "Rolling VaR")

## Rolling Conditional Value at Risk -
   series <- rollingCVaR(smoothedPortfolios)
   plot(series, main = "Rolling CVaR")

## Rolling Drawdown at Risk -
   series <- rollingDaR(smoothedPortfolios)
   plot(series, main = "Rolling DaR")

## Rolling Conditional Drawdown at Risk -
   # series <- rollingCDaR(smoothedPortfolios)
   # plot(series, main = "Rolling CDaR")
%
%## Rolling Risk Budgets -
%   # series <- rollingRiskBudgets(smoothedPortfolios)
%   # plot(series, main = "Rolling Risk Budgets")
}


\keyword{models}

