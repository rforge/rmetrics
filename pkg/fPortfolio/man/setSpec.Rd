\name{setSpec}


\alias{setSpec}

\alias{setType<-}
\alias{setOptimize<-}
\alias{setEstimator<-}
\alias{setTailRisk<-}
\alias{setParams<-}
\alias{setAlpha<-}

\alias{setWeights<-}
\alias{setTargetReturn<-}
\alias{setTargetRisk<-}
\alias{setRiskFreeRate<-}
\alias{setNFrontierPoints<-}
\alias{setStatus<-}

\alias{setSolver<-}
\alias{setObjective<-}
\alias{setTrace<-}


\title{Specification of Portfolios}


\description{
  
    Functions to set specifications for a portfolio.
    \cr
    
    The functions are:
    
    \tabular{ll}{
    \code{setType} \tab Sets type of portfolio optimization, \cr
    \code{setOptimize} \tab Sets what to optimize, min risk or max return, \cr
    \code{setEstimator} \tab Sets names of mean and covariance estimators, \cr
    \code{setParams} \tab Sets optional model parameters, \cr
    
    \code{setWeights} \tab Sets weights vector, \cr
    \code{setTargetReturn} \tab Sets target return value, \cr
    \code{setTargetRisk} \tab Sets target risk value, \cr
    \code{setTargetAlpha} \tab Sets CVaR target alpha value, \cr
    \code{setRiskFreeRate} \tab Sets risk-free rate value, \cr
    \code{setNFrontierPoints} \tab Sets number of frontier points, \cr
    \code{setStatus} \tab Sets status value, \cr
    
    \code{setSolver} \tab Sets the type of solver to be used, \cr
    \code{setObjective} \tab Sets objective function name to be used, \cr
    \code{setTrace} \tab Sets the logical trace flag. }
    
}


\usage{
setType(spec) <- value
setOptimize(spec) <- value
setEstimator(spec) <- value
setTailRisk(spec) <- value
setParams(spec) <- value
setAlpha(spec) <- value

setWeights(spec) <- value
setTargetReturn(spec) <- value
setTargetRisk(spec) <- value
setRiskFreeRate(spec) <- value
setNFrontierPoints(spec) <- value
setStatus(spec) <- value

setSolver(spec) <- value
setObjective(spec) <- value
setTrace(spec) <- value
}


\arguments{

    \item{spec}{
        an S4 object of class \code{fPFOLIOSPEC}, the specification to be 
        modified, by default the default of the function \code{portfolioSpec()}.
        }
    \item{value}{
        a value for that component of \code{spec} to be set.
        }

}       


\details{
    
    The function \code{portfolioSpec()} allows to set the values for the 
    specification structure from scratch. 
    
    To  modify individual settings one can use the set functions.
 
}


\value{
  
    \code{setType}\cr
    \code{setOptimize}\cr
    \code{setEstimator}\cr 
    \code{setParam}\cr
    \cr
    \emph{Model Settings:} just modify the model settings including the 
    portfolio type, the mean/covariance estimator, and optional parameters 
    of an existing portfolio structure.
    \cr
    
    \code{setWeights}\cr
    \code{setTargetReturn}\cr
    \code{setTargetRisk}\cr
    \code{setTargetAlpha}\cr
    \code{setRiskFreeRate}\cr
    \code{setNFrontierPoints}\cr
    \code{setStatus}\cr
    \cr
    \emph{Portfolio Settings:} just modify the portfolio settings including 
    predefined weights, the target return, the risk free rate, the number of 
    frontier points, and the return and risk range of an existing portfolio 
    structure.
    \cr
    
    \code{setSolver}\cr
    \code{setObjective}\cr
    \code{setTrace}\cr
    \cr
    \emph{Optim Settings:} just modifies the solver setting, i.e. the type 
    of solver to be used for portfolio optimization.

}


%\references{}


\examples{
## portfolioSpec -
   # Show Default Portfolio Specifications:
   Spec = portfolioSpec()
   
## setRiskFreeRate - 
   # Change Risk Free Rate
   setRiskFreeRate(Spec) = 3
   Spec
}


\keyword{models}

