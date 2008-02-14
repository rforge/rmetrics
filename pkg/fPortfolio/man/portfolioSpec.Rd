\name{portfolioSpec}


\alias{portfolioSpec}


\title{Specification of Portfolios}


\description{
  
    Specifies a portfolio from scratch.
    
}


\usage{
portfolioSpec(
    model = list(type = "MV", optimize = "minRisk",
        estimator = "covEstimator", tailRisk = list(), 
        params = list(alpha = 0.05, a = 1)), 
    portfolio = list(weights = NULL, targetReturn = NULL, 
        targetRisk = NULL, riskFreeRate = 0, nFrontierPoints = 50, 
        status = 0),
    optim = list(solver = "solveRquadprog", trace = FALSE))

}


\arguments{


    \item{model}{
        a list, containing different arguments: type, estimator, params.
        See these arguments for further explanation.
        }
    \item{portfolio}{
        a list, containing different arguments: weights, targetReturn,
        riskFreeRate, nFrontierPoints. See these
        arguments for further explanation.
        }
    \item{optim}{
        a list with two entries, a character string \code{solver} denoting 
        the type of the solver to be used, and a logical flag, \code{trace}
        denoting if the optimization should be traced.
        }

}       


\details{

    
    The function \code{portfolioSpec} sets the values for the default 
    specification structure. The default settings can be modified using
    the assignment functions for the specification structure.
    \cr
     
    \bold{Model:}
    \cr\cr
    \emph{Type:} The risk measure in use is determined by the argument 
    \code{model$type}. Available measures are mean-variance/Markowitz 
    \code{"MV"}, later we will add lower partial moments \code{LPM} and 
    conditional value at risk \code{"CVaR"}.
    \cr
    \emph{Optimzer:} Tells us what should be optimized. Two options are
    allowed: \code{"minRisk"} to minimize the risk, which is the default
    value, and \code{maxReturns"} to maximize the Return. 
    \cr
    \emph{Estimators:} The estimator(s) for the mean vector and covariance 
    matrix are determined by the argument \code{model$type}, which is a 
    character vector with two elements, the first for the mean and the 
    second for the covariance matrix. Valid pairs for example are 
    \code{c("mean","cov")}, \code{c("mean","shrink")}, or 
    \code{c("mean","mcd")}. The latter two covariance estimators are robust 
    covariance estimators which are recommended to be used for portfolios 
    with many assets and few number of data records. 
    \cr
    
    \bold{Portfolio:}
    \cr\cr
    This slot sets portfolio parameters including weights, target return,
    risk free rate, and number of frontier points.
    Note, only one of \code{portfolio$weights} or \code{targetReturn} can
    be set exclusively. If one of both is set, the other will be set to
    \code{NULL}.
    \cr
    
    \bold{Solver:}
    \cr\cr
    The default solver is the quadratic programming solver from the 
    contributes R package \code{quadprog}, Optionally, one can use 
    the sequential quadratic programming algorithm from Spelucci as
    implemented in the contributed R package \code{Rdonlp2}. Note,
    this package is not part of the Rmetrics software and has to be 
    installed separately. In the case of non-linear risk budget 
    constraints which can can be handled by \code{quadprog}, one has
    to use \code{Rdonlp2}.
 
}


\value{
  
    \code{portfolioSpec}\cr
    \cr
    returns an S4 object of class \code{"fPFOLIOSPEC"}.

}


%\references{}


\examples{
## portfolioSpec -
   
   # Show Default Portfolio Specifications:
   Spec = portfolioSpec()
   
   # Change Risk Free Rate
   setRiskFreeRate(Spec) = 3
   Spec
}


\keyword{models}

