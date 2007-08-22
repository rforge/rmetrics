\name{ValueAtRisk}

\alias{ValueAtRisk}

\alias{VaR}
\alias{CVaR}


\title{Value-at-Risk}


\description{
  
    A collection and description of functions to compute
    Value-at-Risk and conditional Value-at-Risk
    \cr
    
    The functiona are:
    
    \tabular{ll}{
    \code{VaR} \tab Computes Value-at-Risk, \cr
    \code{CVaR} \tab Computes conditional Value-at-Risk. }
    
}


\usage{
VaR(x, alpha = 0.05, type = "sample", tail = c("lower", "upper"))
CVaR(x, alpha = 0.05, type = "sample", tail = c("lower", "upper"))
}


\arguments{

    \item{x}{
        an uni- or multivariate timeSeries object
        }
    \item{alpha}{
        a numeric value, the confidence interval.
    \item{type}{
        a character string, the type to calculate the value-at-risk.
    \item{tail}{
        a character string denoting which tail will be
        considered, either \code{"lower"} or \code{"upper"}.
        If \code{tail="lower"}, then alpha will be converted to
        \code{alpha=1-alpha}.

}


\value{
  
    \code{VaR}\cr
    \code{CVaR}\cr
    \cr
    returns a numeric vector or value with the (conditional) value-at-risk
    for each time series column.
    
}


\seealso{
  
    \code{hillPlot},
    \code{gevFit}. 
  
}


\author{

    Diethelm Wuertz for this \R-port.
    
}


\keyword{models}

