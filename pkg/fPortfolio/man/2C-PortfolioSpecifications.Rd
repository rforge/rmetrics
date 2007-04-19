\name{PortfolioSpecification}

\alias{PortfolioSpecification}

\alias{fPFOLIOSPEC}
\alias{fPFOLIOSPEC-class}
\alias{fPfoliospec}

\alias{show.fPFOLIOSPEC}
\alias{show,fPFOLIOSPEC-method}

\alias{portfolioSpec}

\alias{setType}
\alias{setType<-}
\alias{setEstimator}
\alias{setEstimator<-}
\alias{setParams}
\alias{setParams<-}

\alias{setWeights}
\alias{setWeights<-}
\alias{setTargetReturn}
\alias{setTargetReturn<-}
\alias{setTargetAlpha}
\alias{setTargetAlpha<-}
\alias{setRiskFreeRate}
\alias{setRiskFreeRate<-}
\alias{setNFrontierPoints}
\alias{setNFrontierPoints<-}
\alias{setReturnRange}
\alias{setReturnRange<-}
\alias{setRiskRange}
\alias{setRiskRange<-}

\alias{setSolver}
\alias{setSolver<-}

\alias{portfolioStatistics}
\alias{portfolioData}


\title{Specification of Portfolios}


\description{
  
    A collection and description of functions to 
    specify a portfolio. Typical specifications 
    include model settings, portfolio parameters, 
    and selection of the type of solver. 
    \cr
    
    The functions are:
    
    \tabular{ll}{
    \code{portfolioSpec} \tab Specifies a portfolio, \cr
    \code{setType} \tab Sets type of portfolio optimization, \cr
    \code{setEstimator} \tab Sets names of mean and covariance estimators, \cr
    \code{setParams} \tab Sets optional model parameters, \cr
    \code{setWeights} \tab Sets weights vector, \cr
    \code{setTargetReturn} \tab Sets target return value, \cr
    \code{setTargetAlpha} \tab Sets CVaR target alpha value, \cr
    \code{setRiskFreeRate} \tab Sets risk-free rate value, \cr
    \code{setNFrontierPoints} \tab Sets number of frontier points, \cr
    \code{setReturnRange} \tab Sets range of target returns, \cr
    \code{setRiskRange} \tab Sets range of target risks, \cr
    \code{portfolioStatistics} \tab Estimates 'mu' and 'Sigma' statistics, \cr
    \code{print|show} \tab Print method for \code{fPFOLIOSPEC} objects. }
    
}


\usage{
portfolioSpec(
    model = list(type = "MV", estimator = c("mean", "cov"), params = list()), 
    portfolio = list(weights = NULL, targetReturn = NULL, targetAlpha = NULL,
        riskFreeRate = 0, nFrontierPoints = 50, returnRange = NULL, 
        riskRange = NULL),
    solver = list(type = c("RQuadprog", "RDonlp2"), trace = FALSE), 
    title = NULL, description = NULL)
show.fPFOLIOSPEC(object)

setType(spec = portfolioSpec(), type = c("MV", "LPM", "CVaR"))
setType(spec) <- value
setEstimator(spec = portfolioSpec(), estimator = c("mean", "cov"))
setEstimator(spec) <- value
setParams(spec = portfolioSpec(), params = list())
setParams(spec) <- value

setWeights(spec = portfolioSpec(), weights = NULL)
setWeights(spec) <- value
setTargetReturn(spec = portfolioSpec(), targetReturn = NULL)
setTargetReturn(spec) <- value
setTargetAlpha(spec = portfolioSpec(), targetAlpha = NULL)
setTargetAlpha(spec) <- value
setRiskFreeRate(spec = portfolioSpec(), riskFreeRate = 0)
setRiskFreeRate(spec) <- value
setNFrontierPoints(spec = portfolioSpec(), nFrontierPoints = 100)
setNFrontierPoints(spec) <- value
setReturnRange(spec = portfolioSpec(), returnRange = NULL)
setReturnRange(spec) <- value
setRiskRange(spec = portfolioSpec(), riskRange = NULL)
setRiskRange(spec) <- value

setSolver(spec = portfolioSpec(), solver = c("RQuadprog", "Rdonlp2"))
setSolver(spec) <- value

portfolioStatistics(data, spec = portfolioSpec())
portfolioData(data, spec = portfolioSpec())
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
    \item{params}{
        [setParams] - \cr
        a list of optional model parameters, currently not used.
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
    \item{targetAlpha}{
        [setTargetAlpha] - \cr
        a numerical value, setting the desired portfolio CVaR alpha.
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

    \bold{Portfolio Specifcation Structure:}
    \cr\cr
    
    The S4 class \code{fPFOLIOSPEC} specifies the portfolio. The slots 
    are:\cr
    
    \describe{
        \item{@call}{
            a call, returning the matched function call.
            }
        \item{@model}{
            a list, setting the \code{type} of portfolio to be optimized, 
            and the mean/covariance \code{estimator} to be applied:\cr
            \code{type="MV"} a character string denoting the type of
                portfolio, currently the only implemented type is the
                Mean-Variance Markowitz Portfolio.
                \cr
            \code{estimator=c("mean","cov")} a vector of two character
                strings, the first denoting the mean estimator, and the 
                second the covariance estimator. Additional meaningful 
                selections include robust covariance estimators, e.g. 
                \code{c("mean","mcd")}, or \code{c("mean","shrink")}.
            \code{params=list()} a list of optional model parameters,
                currently not used.
            }     
        \item{@portfolio}{
            a list, settings portfolio parameters including predefined
            weights, target return, risk free rate, number of frontier
            points, and the return and risk range:\cr 
            \code{weights=NULL} a numeric vector specifying the portfolio 
                weights.
                \cr
            \code{targetReturn=NULL} a numeric value specifying the target 
                return. The default value set the target return.\cr
            \code{riskFreeRate=0} a numeric value specifying the risk free 
                rate.
                \cr  
            \code{nFrontierPoints=50} a numeric value determining the number
                of points on the efficient frontier.
                \cr
            \code{returnRange=NULL} a numeric vector of length two, giving 
                the smallest and largest values for the range of returns.
                \cr
            \code{riskRange=NULL} a numeric vector of length two, giving the
                smallest and largest values for the range of risks.
            }
        \item{@solver}{
            a list, setting the type of solver to be used for portfolio
            optimization:
            \cr 
            \code{type=c("RQuadprog", "RDonlp2")}
            }
        \item{@title}{
            a title string, allowing to set a project title. 
            }
        \item{@description}{
            a character string, allowing for a brief project description.
            }
    }
    
    The function \code{portfolioSpec} sets the values for the default 
    specification structure or allows to create a specification structure
    from scratch.
    \cr
     
    \bold{Model:}
    \cr\cr
    \emph{Type:} The risk measure in use is determined by the argument 
    \code{model$type}. Available measures are mean-variance/Markowitz 
    \code{"MV"}, later we will add lower partial moments \code{LPM} and 
    conditional value at risk \code{"CVaR"}.
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
    risk free rate, number of frontier points, and target range and return.
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


\note{

    Using Spelucci's solver "donlp2" requires to load the contributed
    R package \code{Rdonlp2}.
    
}


\value{
  
    \code{portfolioSpec}\cr
    \cr
    returns an S4 object of class \code{"fPFOLIOSPEC"}.

    \code{setType}\cr
    \code{setEstimator}\cr 
    %\code{setParam}\cr
    \cr
    \emph{Model Settings:} just modify the model settings including the 
    portfolio type, the mean/covariance estimator, and optional parameters 
    of an existing portfolio structure.
    \cr
    
    \code{setWeights}\cr
    \code{setTargetReturn}\cr
    \code{setRiskFreeRate}\cr
    \code{setNFrontierPoints}\cr
    \code{setReturnRange}\cr
    \code{setRiskRange}\cr 
    \cr
    \emph{Portfolio Settings:} just modify the portfolio settings including 
    predefined weights, the target return, the risk free rate, the number of 
    frontier points, and the return and risk range of an existing portfolio 
    structure.
    \cr
    
    \code{setSolver}\cr
    \cr
    \emph{Solver Settings:} just modifies the solver setting, i.e. the type 
    of solver to be used for portfolio optimization.
    \cr
        
    \code{portfolioStatistics}\cr
    returns a named list of estimated mean \code{$mu} and covariance 
    \code{$Sigma} statistics, from a multivariate time series of assets.\cr

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
   # Show Default Portfolio Specifications:
   portfolioSpec()
   # Modify Specifications - Set the Number of Frontier Points:
   Spec = portfolioSpec(portfolio = list(nFrontierPoints = 150))
   # Change Risk Free Rate
   setRiskFreeRate(Spec) = 3
   Spec

## portfolioStatistics -
   # Load Data, i.e. a List of Returns:
   Data = as.timeSeries(data(smallcap.ts))
   Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
   Data
   # Estimating Mean and Covariance with mean()and cov()
   portfolioStatistics(Data)
   # Set Estimators to "mcd" Estimator:
   Spec = setEstimator(estimator = c("mean", "shrink"))
   # Estimating Mean and Covariance MCD
   portfolioStatistics(Data, Spec)
}


\keyword{models}

