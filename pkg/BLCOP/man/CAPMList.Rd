\name{CAPMList}
\alias{CAPMList}
\title{Compute CAPM alphas for a set of assets}
\description{
    \code{CAPMList} is a helper function that computes the "alphas" and "betas" in the sense of the CAPM for 
    series of asset returns.  It is meant to be used for computing "prior" means for the Black-Litterman model.
}
\usage{
CAPMList(returns, marketIndex, riskFree = NULL, regFunc = BLCOPOptions("regFunc"), coeffExtractFunc = NULL, ...)
}

\arguments{
  \item{returns}{A matrix or data.frame of asset returns, with different columns corresponding to different assets}
  \item{marketIndex}{A time series of returns for some market index (e.g. SP500)}
  \item{riskFree}{Risk-free rate of return}
  \item{regFunc}{The name of the function to used to regress the asset return series against the market index.  This is set in the BLCOP options,
   and is \code{lm} by default.}
  \item{coeffExtractFunc}{A function that extracts the intercept (alpha) and coefficient of the market index (beta) from the results of a 
      call to the regression function. It should return a vector containing these two elements.}
  \item{\dots}{Additional arguments to the regression function }
}
\details{
    \code{coeffExtractFun} is needed because some regression functions such as \code{gls} from the \code{nlme} package
    don't return their results in the same format as \code{lm} does.  If it is not supplied, a default that works with \code{lm} results is used.
}
\value{
    A \code{data.frame} with one column for the "alphas" and another for the "betas"
}

\author{Francisco Gochez <fgochez@mango-solutions.com>}

\examples{
    CAPMList(monthlyReturns, marketIndex = sp500Returns, riskFree = US13wTB, regFunc = "rlm")
}
\keyword{utilities}
\keyword{math}