\name{fPortfolioBacktest-package}


\alias{fPortfolioBacktest-package}
\alias{fPortfolioBacktest}


\docType{package}


\title{Package for Portfolio Backtesting}


\description{

    \code{fPortfolioBacktest} is a portfolio backtesting package 
    which allows users to personally define functions which calibrates 
    the backtesting procedure.  
    These functions are refered to as "user" functions. The
    package comes with pre-defined functions that defines the rolling 
    windows, portfolio strategy and smoothing style.
    
    The main backtest functions are \code{portfolioBacktesting} and 
    \code{portfolioSmoothing}, and also a new S4 class is introduced 
    in this package, 'fPFOLIOBACKTEST' which handles backtest 
    specifications.

}


\details{

	\tabular{ll}{
		Package: \tab fPortfolioBacktest\cr
		Type: \tab Package\cr
		Version: \tab 1.0\cr
		Date: \tab 2008-09-17\cr
		License: \tab GPL Version 2 or later\cr}

}


\references{

W\"urtz, D., Chalabi, Y., Chen W., Ellis A. (2009);
    \emph{Portfolio Optimization with R/Rmetrics}, 
    Rmetrics eBook, Rmetrics Association and Finance Online, Zurich.
    
}

