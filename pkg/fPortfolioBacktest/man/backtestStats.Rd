\name{backtestStats}


\alias{backtestStats}

\alias{rollingSigma}
\alias{rollingVaR}
\alias{rollingCVaR}
\alias{rollingDaR}
\alias{rollingCDaR}
\alias{rollingRiskBudgets}



\title{Rolling Portfolio Backtest Statistics}


\description{

    Computes rolling statistics for backtest analysis

}


\usage{   
backtestStats(object, stats = "rollingSigma", ...)

rollingSigma(portfolios)
rollingVaR(portfolios, alpha = 0.05)
rollingCVaR(portfolios, alpha = 0.05)
rollingDaR(portfolios, alpha = 0.05)
rollingCDaR(portfolios, alpha = 0.05)
rollingRiskBudgets(portfolios)


}


\arguments{

    \item{object}{
        a list, returned from running the function 
        \code{portfolioSmoothing}.
        }
   \item{stats}{
        a character string, specifying the name of the rolling statistics 
        function.
        }
   \item{portfolios}{
	    a list, portfolio objects of class fPORTFOLIO ordered by time.
	    }
   \item{alpha}{
	    a numeric value, specifying the quantile level
	    }
    \item{\dots}{
        additional arguments passed to the \code{stats} function
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

    The function \code{rollingRiskBudgets} calculates a rolling Risk 
    Budget of the portfolio.


}


\references{

Wuertz, D., Chalabi, Y., Chen W., Ellis A. (2009);
    \emph{Portfolio Optimization with R/Rmetrics}, 
    Rmetrics eBook, Rmetrics Association and Finance Online, Zurich.
    
}


\keyword{models}

