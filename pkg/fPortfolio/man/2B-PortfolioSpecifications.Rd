\name{PortfolioSpecification}

\alias{PortfolioSpecification}

\alias{fPFOLIOSPEC}
\alias{fPFOLIOSPEC-class}

\alias{show.fPFOLIOSPEC}
\alias{show,fPFOLIOSPEC-method}

\alias{portfolioSpec}

\alias{setType}
\alias{setEstimator}

\alias{setWeights}
\alias{setTargetReturn}
\alias{setRiskFreeRate}
\alias{setNFrontierPoints}
\alias{setReturnRange}
\alias{setRiskRange}

\alias{portfolioStatistics}


\title{Specification of Portfolios}


\description{
  
    A collection and description of xxx.
    \cr
    
    The functions are:
    
    \tabular{ll}{
    \code{portfolioSpec} \tab Specifies a portfolio, \cr
    \code{print|show} \tab Print method for 'fPFOLIOSPEC' objects, \cr
    \code{setType} \tab Sets type of portfolio optimization, \cr
    \code{setEstimator} \tab Sets names of mean and covariance estimators, \cr
    \code{setWeights} \tab Sets weights vector, \cr
    \code{setTargetReturn} \tab Sets target return value, \cr
    \code{setRiskFreeRate} \tab Sets risk-free rate value, \cr
    \code{setNFrontierPoints} \tab Sets number of frontier points, \cr
    \code{setReturnRange} \tab Sets range of target returns, \cr
    \code{setRiskRange} \tab Sets range of target risks, \cr
    \code{portfolioStatistics} \tab Estimates 'mu' and 'Sigma' statistics. }
    
}


\usage{
portfolioSpec(model = list(type = "MV", estimator = c("mean", "cov")), 
    portfolio = list(weights = NULL, targetReturn = NULL, riskFreeRate = 0, 
    nFrontierPoints = 50, returnRange = NULL, riskRange = NULL),
    title = NULL, description = NULL)
show.fPFOLIOSPEC(object)

setType(spec = portfolioSpec(), type = c("MV", "CVaR"))
setEstimator(spec = portfolioSpec(), estimator = c("mean", "cov"))

setWeights(spec = portfolioSpec(), weights = NULL)
setTargetReturn(spec = portfolioSpec(), targetReturn = NULL)
setRiskFreeRate(spec = portfolioSpec(), riskFreeRate = 0)
setNFrontierPoints(spec = portfolioSpec(), nFrontierPoints = 100)
setReturnRange(spec = portfolioSpec(), returnRange = NULL)
setRiskRange(spec = portfolioSpec(), riskRange = NULL)

portfolioStatistics(data, spec = portfolioSpec())
}


\details{

    \bold{Portfolio Specification:}
    \cr\cr
    Describe it ...

}


\arguments{

    \item{data}{
        [portfolioStatistics] - \cr
        xxx
        }
    \item{description}{
        [portfolioSpec] - \cr
        xxx
        }
    \item{estimator}{
        [setEstimator] - \cr
        xxx
        }
    \item{model}{
        [setModel] - \cr
        xxx
        }
    \item{nFrontierPoints}{
        [setNFrontierPoints] - \cr
        xxx
        }
    \item{object}{
        [show] - \cr
        xxx
        }
    \item{portfolio}{
        [portfolioSpec] - \cr
        xxx
        }
    \item{returnRange, riskRange}{
        [set*Range] - \cr
        ...
        }
    \item{riskFreeRate}{
        [setRiskFreeRate] - \cr
        xxx
        }
    \item{spec}{
        xxx
        }
    \item{targetReturn}{
        [setTargetReturn] - \cr
        xxx
        }
    \item{title}{
        [portfolioSpec] - \cr
        xxx
        }
    \item{type}{
        [setType] - \cr
        xxx
        }
    \item{weights} {
        [setWeights] - \cr
        xxx
        }

}


\value{
  
    \code{portfolioSpec} 
    \cr
    returns an S4 object of class \code{"fPFOLIOSPEC"}, with the following 
    slots:

    \item{call}{
        a call, returning the matched function call.
        }
    \item{description}{
        a character string allowing for a brief project description.
        }
    \item{model}{
        a list, containing with two named elements:\cr
        \code{type="MV"} xxx \cr
        \code{estimator=c("mean","cov")} xxx
        }     
    \item{portfolio}{
        a list, containing parameter specifications for the portfolio:\cr 
        \code{weights=NULL} a numeric vector specifying the portfolio 
            weights. xxx
            \cr
        \code{targetReturn=NULL} a numeric value specifying the target return.
            The default value set the target return xxx
            \cr
        \code{riskFreeRate=0} a numeric value specifying the risk free rate. 
            \cr  
        \code{nFrontierPoints=50} a numeric calue determining the number
            of points on the efficient frontier.
        \code{returnRange=NULL} a numeric vector of length two, giving the
            smallest and largest values for the range of returns.
            \cr
        \code{riskRange=NULL} a numeric vector of length two, giving the
            smallest and largest values for the range of risks.
        }
    \item{title}{
        a title string.
        }
    \cr
           
    \code{setType}\cr
    \code{setEstimator}\cr 
    return the model specifications from an object of class 
    \code{fPFOLIOSPEC"}
    xxx
    \cr
    
    \code{setWeights}\cr
    \code{setTargetReturn}\cr
    \code{setRiskFreeRate}\cr
    \code{setNFrontierPoints}\cr
    \code{setReturnRange}\cr
    \code{setRiskRange}\cr
    return the portfolio specifications from an object of class 
    \code{fPFOLIOSPEC"}
    xxx
    \cr
       
    \code{portfolioStatistics}\cr
    returns the xxx
}


%\references{}


\seealso{

    \code{xxx}.

}


\author{

    Diethelm Wuertz and Oliver Greshake for the Rmetrics port.
    
}


\examples{

## portfolioSpec -
   # Show Default Portfolio Specifications
   # portfolioSpec() # requires getTitle()
 
}


\keyword{models}

