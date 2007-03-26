\name{PortfolioSpecification}

\alias{PortfolioSpecification}

\alias{fPFOLIOSPEC}
\alias{fPFOLIOSPEC-class}
\alias{fPfoliospec}

\alias{show.fPFOLIOSPEC}
\alias{show,fPFOLIOSPEC-method}

\alias{portfolioSpec}

\alias{setType}
\alias{setEstimator}

\alias{setWeights}
\alias{setWeights<-}
\alias{setTargetReturn}
\alias{setTargetReturn<-}
\alias{setRiskFreeRate}
\alias{setRiskFreeRate<-}
\alias{setNFrontierPoints}
\alias{setReturnRange}
\alias{setRiskRange}

\alias{portfolioStatistics}


\title{Specification of Portfolios}


\description{
  
    A collection and description of functions helping to set the specifications
    of the portfolio. Typical specifications are desired target risk, estimation
    model of covariance matrix and type of risk measure. See Notes for a complete
    list of specifications and Arguments for description. 
    \cr
    
    The functions are:
    
    \tabular{ll}{
    \code{portfolioSpec} \tab Specifies a portfolio, \cr
    \code{print|show} \tab Print method for \code{fPFOLIOSPEC} objects, \cr
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
portfolioSpec(
    model = list(type = "MV", estimator = c("mean", "cov"), params = list()), 
    portfolio = list(weights = NULL, targetReturn = NULL, riskFreeRate = 0, 
        nFrontierPoints = 50, returnRange = NULL, riskRange = NULL),
    solver = list(type = c("RQuadprog", "RDonlp2")), 
    title = NULL, description = NULL)
show.fPFOLIOSPEC(object)

setType(spec = portfolioSpec(), type = c("MV", "LPM", "CVaR"))
setEstimator(spec = portfolioSpec(), estimator = c("mean", "cov"))

setWeights(spec = portfolioSpec(), weights = NULL)
setWeights(spec) <- value
setTargetReturn(spec = portfolioSpec(), targetReturn = NULL)
setTargetReturn(spec) <- value
setRiskFreeRate(spec = portfolioSpec(), riskFreeRate = 0)
setRiskFreeRate(spec) <- value
setNFrontierPoints(spec = portfolioSpec(), nFrontierPoints = 100)
setReturnRange(spec = portfolioSpec(), returnRange = NULL)
setRiskRange(spec = portfolioSpec(), riskRange = NULL)

portfolioStatistics(data, spec = portfolioSpec())
}


\arguments{

    \item{data}{
        [portfolioStatistics] - \cr
        a time series or a named list, containing either a series of returns or
        named entries 'mu' and 'Sigma' being mean and covariance matrix.
        }
    \item{description}{
        [portfolioSpec] - \cr
        a character string, allowing for a brief project description, by default
        NULL, i.e. Date and User.
        }
    \item{estimator}{
        [setEstimator] - \cr
        a character string vector, determining which estimation type should be
        used calculating mean and covariance matrix, by default "mean" and "cov"
        meaning functions mean() and cov(). Other estimators are "mcd", using 
        an mcd estimator, and "shrink", using an shrinkage estimator.
        See section details for further information.
        }
    \item{model}{
        [portfolioSpec] - \cr
        a list, containing different arguments: type, estimator, params.
        See these arguments for further explanation.
        }
    \item{nFrontierPoints}{
        [setNFrontierPoints] - \cr
        a numeric, determining the number of equidistant steps on the frontier,
        by default 50.
        }
    \item{object}{
        [show] - \cr
        an S4 object of class \code{fPFOLIOSPEC}, having slots call, model,
        portfolio, title, description. See \code{value} for slot description.
        }
    \item{portfolio}{
        [portfolioSpec] - \cr
        a list, containing different arguments: weights, targetReturn,
        riskFreeRate, nFrontierPoints, returnRange, riskRange. See these
        arguments for further explanation.
        }
    \item{returnRange}{
        [setReturnRange] - \cr
        a numerical vector, setting return range for calculation of efficient
        frontier, by default NULL, i.e. range of returns from input data is taken.
        }
    \item{riskFreeRate}{
        [setRiskFreeRate] - \cr
        a integer value, determining the percentage of the yearly return for a
        risk free asset.
        }
    \item{riskRange}{
        [setRiskRange] - \cr
        a numerical vector, setting risk range for calculation of efficient
        frontier, by default NULL, i.e. range of risk from input data is taken.
        }
    \item{solver}{
        a character string denoting the type of the solver to be used.
        }
    \item{spec}{
        an S4 object of class \code{fPFOLIOSPEC}, the specification to be modified,
        by default the default of the function \code{portfolioSpec()}.
        }
    \item{targetReturn}{
        [setTargetReturn] - \cr
        a numerical value, setting the desired portfolio return,
        by default return of the tangency portfolio.
        }
    \item{title}{
        [portfolioSpec] - \cr
        a character string, containing the title for the object,
        by default NULL, i.e. "Portfolio Specification".
        }
    \item{type}{
        [setType] - \cr
        a character string, determining which risk measure should be taken,
        by default "MV" meaning mean-variance. Other types are "CVaR" and "LPM".
        }
    \item{value}{
        a value for that component of \code{spec} to be set.
        }
    \item{weights}{
        [setWeights] - \cr
        a numerical vector, containing the weights of the desired optimal portfolio.
        Note that the program stops if desired return and weights are set.
        }

}


\details{

    \bold{Portfolio Specification:}
    \cr\cr
    The S4 class \code{fPFOLIOSPEC} contains all information about the
    specifications of the portfolio. For a full slot description see \code{value}.
    \cr
    
    \bold{Estimators:}
    \cr\cr
    The estimator for mean vector and covariance matrix are determined by the
    argument \code{type}. Estimator are "mean" (only for mean vector), "cov"
    (only for covariance matrix), "mcd" and "shrink". Especially for portfolios
    with a lot of assets and minor number of data points, MCD and shrinkage
    improve the estimation covariance matrix. The estimator effects only the
    mean-variance/Markowitz framework.
    \cr
    
    \bold{Risk Measures:}
    \cr\cr
    The risk measures is determined by the argument \code{type}. Available
    measures are mean-variance/Markowitz, lower partial moments (LPM) and CVaR.
    Markowitz framework is only useful along with multivariate normally
    distributed assets. Where CVaR framework is independent from assets
    distribution.
    \cr\cr
    
}


\note{

    Using Spelucci's solver "donlp2" requires to load the contributed
    R package \code{Rdonlp2}.
    
}


\value{
  
    \code{portfolioSpec}
    \code{setType}\cr
    \code{setEstimator}\cr 
    \code{setRiskFreeRate}\cr
    \code{setWeights}\cr
    \code{setTargetReturn}\cr
    \code{setReturnRange}\cr
    \code{setRiskRange}\cr 
    return all an S4 object of class \code{"fPFOLIOSPEC"}, with the following 
    slots:

    \item{call}{
        a call, returning the matched function call.
        }
    \item{description}{
        a character string, allowing for a brief project description.
        }
    \item{model}{
        a list, containing two named elements being the type and the 
        estimator\cr
        \code{type="MV"} 
            \cr
        \code{estimator=c("mean","cov")} 
            }     
    \item{portfolio}{
        a list, containing parameter specifications for the portfolio:\cr 
        \code{weights=NULL} a numeric vector specifying the portfolio 
            weights.\cr
        \code{targetReturn=NULL} a numeric value specifying the target 
            return. The default value set the target return.\cr
        \code{riskFreeRate=0} a numeric value specifying the risk free 
            rate.\cr  
        \code{nFrontierPoints=50} a numeric value determining the number
            of points on the efficient frontier.\cr
        \code{returnRange=NULL} a numeric vector of length two, giving the
            smallest and largest values for the range of returns.\cr
        \code{riskRange=NULL} a numeric vector of length two, giving the
            smallest and largest values for the range of risks.
        }
    \item{title}{
        a title string.\cr
        }
    %\cr move inside \item!
          
 
    \code{portfolioStatistics}\cr
    returns a named list of estimated mu and Sigma statistics, from a time 
    series.\cr

    Note:\cr
    In the following all specification elements of the portfolio are listed.
    data, estimator, model, nFrontierPoints, returnRange, riskFreeRate, 
    riskRange, targetReturn, type, weights.
}


%\references{}


\seealso{

   \code{\link{PortfolioData}},
   \code{\link{PortfolioConstraints}},
   \code{\link{fPORTFOLIO}},
   \code{\link{PortfolioExtractors}},\cr
   \code{\link{PortfolioPlots}}.

}


\author{

    Diethelm Wuertz and Oliver Greshake for the Rmetrics port.
    
}


\examples{
## portfolioSpec -
   # Show Default Portfolio Specifications
   portfolioSpec() # requires getTitle()
   
   # Modification of Specifications
   # Setting the Number of Frontier Points
   Spec = portfolioSpec(portfolio = list(nFrontierPoints = 150))
   # Setting the Risk Free Rate
   setRiskFreeRate(Spec) = 3
   Spec

## portfolioStatistics -
   # Getting Data, i.e. a List of returns
   Data = usPortfolioData()
   # Estimating Mean and Covariance with mean()and cov()
   portfolioStatistics(Data)
   # Setting Estimators to MCD Method
   Spec = setEstimator(estimator = c("mcd", "mcd"))
   # Estimating Mean and Covariance MCD
   portfolioStatistics(Data, Spec)
}


\keyword{models}

