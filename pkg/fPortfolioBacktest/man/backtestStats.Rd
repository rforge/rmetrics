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
backtestStats(object, FUN = "rollingSigma", \dots)

rollingSigma(object)
rollingVaR(object)
rollingCVaR(object)
rollingDaR(object)
rollingCDaR(object)
rollingRiskBudgets(object)
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

    The function \code{rollingRiskBudgets} calculates a rolling Risk 
    Budget of the portfolio.


}


\references{

Wuertz, D., Chalabi, Y., Chen W., Ellis A. (2009);
    \emph{Portfolio Optimization with R/Rmetrics}, 
    Rmetrics eBook, Rmetrics Association and Finance Online, Zurich.
    
}


\examples{
##  -   
}


\keyword{models}

