\name{PortfolioSpec}

\alias{PortfolioSpec}

\alias{fPFOLIOSPEC}
\alias{fPFOLIOSPEC-class}
\alias{fPfoliospec}

\alias{show.fPFOLIOSPEC}
\alias{show,fPFOLIOSPEC-method}

\alias{portfolioSpec}

\alias{setType<-}
\alias{setEstimator<-}
\alias{setTailRisk<-}
\alias{setParams<-}

\alias{setWeights<-}
\alias{setTargetReturn<-}
\alias{setTargetRisk<-}
\alias{setTargetAlpha<-}
\alias{setRiskFreeRate<-}
\alias{setNFrontierPoints<-}

\alias{setSolver<-}
\alias{setTrace<-}


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
    \code{setTargetRisk} \tab Sets target risk value, \cr
    \code{setTargetAlpha} \tab Sets CVaR target alpha value, \cr
    \code{setRiskFreeRate} \tab Sets risk-free rate value, \cr
    \code{setNFrontierPoints} \tab Sets number of frontier points, \cr
    \code{setNFrontierPoints} \tab Sets tail dependence matrix, \cr
    \code{portfolioStatistics} \tab Estimates 'mu' and 'Sigma' statistics, \cr
    \code{print|show} \tab Print method for \code{fPFOLIOSPEC} objects. }
    
}


\usage{
portfolioSpec(
    model = list(type = "MV", estimator = "covEstimator", 
        tailRisk = list(), params = list()), 
    portfolio = list(weights = NULL, targetReturn = NULL, targetRisk = NULL, 
        targetAlpha = 0.05, riskFreeRate = 0, nFrontierPoints = 50, status = 0),
    optim = list(solver = "solveRquadprog", trace = FALSE))
        
show.fPFOLIOSPEC(object)

setType(spec) <- value
setEstimator(spec) <- value
setTailRisk(spec) <- value
setParams(spec) <- value

setWeights(spec) <- value
setTargetReturn(spec) <- value
setTargetRisk(spec) <- value
setTargetAlpha(spec) <- value
setRiskFreeRate(spec) <- value
setNFrontierPoints(spec) <- value

setSolver(spec) <- value
setTrace(spec) <- value
}


\arguments{

    \item{estimator}{
        [setEstimator] - \cr
        a character string vector, determining which type of estimator 
        should be used calculating the mean vector and the covariance 
        matrix. By default \code{"covEstimator"} which uses the standard
        mean and covariance estimators, i.e. calling the functions mean() 
        and cov(). Alternatives include among others robust and shrinkage
        estimators.
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
        riskFreeRate, nFrontierPoints. See these
        arguments for further explanation.
        }
    \item{riskFreeRate}{
        [setRiskFreeRate] - \cr
        a integer value, determining the percentage of the yearly return for a
        risk free asset.
        }
    \item{optim}{
        a list with two entries, a character string \code{solver} denoting 
        the type of the solver to be used, and a logical flag, \code{trace}
        denoting if the optimization should be traced.
        }
    \item{spec}{
        an S4 object of class \code{fPFOLIOSPEC}, the specification to be 
        modified, by default the default of the function \code{portfolioSpec()}.
        }
    \item{tailRisk}{
        a list with two matrices for the upper and lower tail dependence
        coefficients.
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
    \item{targetRisk}{
        [setTargetRisk] - \cr
        a numerical value, setting the desired portfolio risk,
        only for the dual portfolio calculation.
        }
    \item{type}{
        [setType] - \cr
        a character string, determining which risk measure should be taken,
        by default \code{"MV"} meaning mean-variance. 
        Other types are conditional value-at-risk \code{"CVaR"}, 
        and lower parttial moments \code{"LPM"}.
        }
    \item{value}{
        a value for that component of \code{spec} to be set.
        }
    \item{weights}{
        [setWeights] - \cr
        a numerical vector, containing the weights of the desired optimal 
        portfolio. Note that the program stops if desired return and weights 
        are set.
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
            and the mean/covariance \code{estimator} to be applied:
            \cr
            \code{type=c("MV","CVaR")} a character string denoting the type of
                portfolio, the implemented types are the
                Mean-Variance Markowitz Portfolio, \code{"MV"}, and the
                Mean-CVaR Portfolio, \code{"CVaR"}.
                \cr
            \code{estimator=c("mean","cov")} a vector of two character
                strings, the first denoting the mean estimator, and the 
                second the covariance estimator. Additional meaningful 
                selections include robust covariance estimators, e.g. 
                \code{c("mean","mcd")}, or \code{c("mean","shrink")}.
                \cr
            \code{tailRisk=list()} a list of optional tail risk information,
                currently not used.\cr
            \code{params=list()} a list of optional model parameters,
                currently not used.
            }     
        \item{@portfolio}{
            a list, settings portfolio parameters including predefined
            weights, target return, risk free rate, number of frontier
            points:
            \cr 
            \code{weights=NULL} a numeric vector specifying the portfolio 
                weights.
                \cr
            \code{targetReturn=NULL} a numeric value specifying the target 
                return. The default value sets the target return.
                \cr
            \code{targetRisk=NULL} a numeric value specifying the target 
                risk.\cr
            \code{targetAlpha=NULL} a numeric value specifying the target 
                alpha confidence level for CVaR portfolio optimization. 
                The default value sets the target return.
                \cr
            \code{riskFreeRate=0} a numeric value specifying the risk free 
                rate.
                \cr  
            \code{nFrontierPoints=50} a numeric value determining the number
                of points on the efficient frontier.
            }
        \item{@solver}{
            a list, setting the type of solver to be used for portfolio
            optimization:
            \cr 
            \code{type=c("quadprog", "Rdonlp2", "lpSolve")} a character
                string specifying the name of the solver to be used.\cr
            \code{trace=FALSE} a logical flag, should the optimization be
                traced?
            }
        \item{@title}{
            a title string, with a default project title. 
            }
        \item{@description}{
            a character string, with a default project description.
            }
    }
    
    The function \code{portfolioSpec()} sets the values for the default 
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

}


%\references{}


\seealso{

   \code{\link{PortfolioData}},
   \code{\link{PortfolioConstraints}},
   \code{\link{fPORTFOLIO}},
   \code{\link{PortfolioPlots}}.

}


\author{

    Diethelm Wuertz for the Rmetrics port,\cr
    Oliver Greshake for addons.
    
}


\examples{
## portfolioSpec -
   # Show Default Portfolio Specifications:
   Spec = portfolioSpec()
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
   # Set Covariance Estimator to "shrink" Estimator:
   setEstimator(Spec) = c("mean", "shrink")
   # Estimating Mean and Covariance MCD
   portfolioStatistics(Data, Spec)
}


\keyword{models}

