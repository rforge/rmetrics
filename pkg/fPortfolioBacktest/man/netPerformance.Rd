\name{netPerformance}


\alias{netPerformance}


\title{Net performance of the portfolio}


\description{

    Displays plot of rebased portfolio performance and summary 
    statistics.
    
}


\usage{   
netPerformance(object, format = "\%Y-\%m-\%d")
}


\arguments{

    \item{object}{
        a list, returned from running the function \code{portfolioSmoothing}.
        }
    \item{format}{
        a character string of the date format used
       }
    
}


\value{

    A plot of rebased portfolio returns and tables summarising portfolio 
    performance over time.
    
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
     
## Net Peformance - 
   netPerformance(smoothedPortfolios)
}


\keyword{models}

