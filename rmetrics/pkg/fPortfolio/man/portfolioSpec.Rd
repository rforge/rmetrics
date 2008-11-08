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
    optim = list(solver = "solveRquadprog", objective = NULL,
        params = list(meq = 2), control = list(), trace = FALSE))
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
        a list with four entries, a character string \code{solver} denoting 
        the type of the solver to be used, a \code{params} list to pass further
        arguments to the objective function to optimize, a \code{control} list
        for all control settings of the solver, and a logical flag, \code{trace}
        denoting if the optimization should be traced.
        }

}       


\details{

    To optimize a portfolio of assets we first have to specify it. All
    settings which specify a portfolio of assets are respresented by a
    S4 class named \code{fPFOLIOSPEC}.
        
    \preformatted{
    setClass("fPFOLIOSPEC", 
      representation(
        model = "list",
        portfolio = "list",
        optim = "list") )
    }
        
    An object of class \code{fPFOLIOSPEC} has three slots, named
    \code{@model}, \code{@portfolio}, and \code{@optim}. The first 
    slot \code{@model} holds the model information, the second
    slot \code{@portfolio} the portfolio information, and the last 
    slot \code{@optim} the information about the solver used for 
    optimization.
    
    The default settings are as follows:
        
    \preformatted{
    model = list(
        type = "MV",
        optimize = "minRisk",    
        estimator = "covEstimator",     
        tailRisk = list(),               
        params = list(alpha = 0.05, a = 2)),
    portfolio = list(
        weights = NULL, 
        targetReturn = NULL, 
        targetRisk = NULL,
        riskFreeRate = 0, 
        nFrontierPoints = 50,
        status = NA),
    optim = list(
        solver = "solveRquadprog",    
        objective = NULL,  
        parames = list(),
        control = list(meq = 2),   
        trace = FALSE)
    }
        
        
    \bold{Model Slot:}
    
    
    \emph{Type of Model:}\cr
    The list entry \code{type} from the \code{@model} slot describes 
    the type of the desired portfolio. The current implementation 
    supports three types of portfolios. This may be 
    a Markowitz mean -- variance portfolio named \code{"MV"}, 
    a mean -- lower partial moment portfolio named \code{"LPM"}, or 
    a mean -- CVaR condititional value-at-risk portfolio named \code{"CVaR"}. 
    One can use the function \code{getType} to retrieve the current
    setting and the function \code{setType} to modify this selection.
     
    
    \emph{What to optimize?}\cr
    The list entry \code{optimize} from the \code{@model} slot describes 
    what should be optimized. Two choices are psssible. Either
    
    \preformatted{
        \code{"minRisk"}
        } 
        
    which minimizes the risk if the target returns is given, or 
    
    \preformatted{
        \code{"maxReturn"}
        } 
        
    which maximizes the return if the target risk is given.
    One can use the function \code{getOptimize} to retrieve the current
    setting and the function \code{setOptimize} to modify this selection.
    
    
    \emph{How to estimate mean and covariance?}\cr
    The list entry \code{estimator} from the \code{@model} slot requests 
    for a string that denotes the function name of the covariance 
    estimator which should be used for the estimation of risk. 
    
    In Markowitz' mean-variance portfolio model, \code{type="MV"}, the 
    default function 
    
    \preformatted{
        \code{"covEstimator"} 
        }
        
    is used which computes the standard column means 
    of the multivariate assets data series and the standard covariance 
    matrix. Alternative robust estimators include 
    
    \preformatted{
        \code{"covMcdEstimator"}
        \code{"covOGKEstimator"}
        \code{"mveEstimator"}
        \code{"nnveEstimator"}
        \code{"mcdEstimator"}
        }
        
    In addition a shrinkage covariance estimator named
        
    \preformatted{
        \code{"shrinkEstimator"},
        }
        
    and a bagged covariance estimator named
     
    \preformatted{   
        \code{"baggedEstimator"} 
        }
    
    are also available.
    Note, the experienced user can add his own function to estimate in any
    alternative way the mean and the covariance of the multivariate assets
    data series. In this case (s)he has to write a function, e.g. named
     
    \preformatted{   
        \code{myEstimator=function(x,spec=NULL,...)}
        }
        
    where \code{x} is a multivariate time series, \code{spec} optionally 
    the portfolio specification, if rquired, and \code{...} additional
    arguments passed to the users code. Note, \code{myEstimator} must
    a return a named list, with at least the following two entries
    \code{\$mu} and \code{\$Sigma}, which represent estimators for the
    mean and covariance, respectively. 
    
    In the case of the Mean -- Lower-Partial-Moment portfolio, 
    \code{type="LPM"} we make use of the equivalence to Markowitz' 
    mean-variance portfolio with a modified covariance estimator, i.e. 
    
    \preformatted{
        \code{"lpmEstimator"},
        }
        
    Note, in this case the setting of \code{type="LPM"} 
    changes the covariance estimator function name 
    from any selection previously made to the function automatically
    to \code{"lpmEstimator"} which returns the LPM mean and covariance 
    estimates.   

    One can use the function \code{getEstimator} to retrieve the current
    setting and the function \code{setEstimator} to modify this selection.
     
     
    \emph{Tail Risk List:}\cr
    The list entry \code{tailRisk} from the \code{@model} slot is an empty 
    list. It can be used to add tail risk budget constrains to the 
    optimization. In this case a square matrix of the size of the 
    number of assets is expected as list entry, which contains bivariate 
    tail risk measures, i.e. the tail dependence coefficients estaimated 
    via a copulae approach.   
    Use the function \code{setType} to modify this selection.
    
    
    The list entry \code{parameters} from the \code{@model} slot is a 
    list with additional parameters used in different situations. It can 
    be ebhanced by the user if needed. 
    By default it contains the exponent \code{a=2}, the parameter needed 
        for "LPM" portfolio optimization,
    and it contains the \code{targetAlpha=0.05}, the confidence level
        for "CVaR" portfolio optimization.
    Use the function \code{setParams} to modify this selection.
    
    
    \bold{Portfolio Slot:}
    
    
    The values \code{weights}, \code{targetReturn}, and \code{targetRisk}
    from the \code{portfolio} slot have to be considered in common. By
    default all three are set to \code{NULL}. If this is the case, then 
    it is assumed that an equal weight portfolio should be calculated.
    If only one of the three values is different from \code{NULL} then
    the following procedure will be startet. 
    If the weights are specified then it is assumed that a feasible 
        portfolio should be considered.
    If the target return is fixed then it is assumed that the efficient 
        portfolio with the minimal risk will be considered. 
    And finally if the risk is fixed, then the return should be maximized.
    Use the functions \code{setWeights}, \code{setTargetReturn}, and
    \code{setTargetRisk} to modify this selection.Note, the change in of
    the three functions will influence the settings of the other two.
    
    
    The \code{riskFreeRate=0} is also stored in the \code{portfolio} slot.
    Its value defaults to zero. It can be changed by the user.
    Use the function \code{setRiskFreeRate} to modify this selection.
    
    
    The number of frontier points reqauired by the calculation of
    the \code{portfolioFrontier} is obtained from the value of
    \code{nFrontierPoints=50} hold in the \code{portfolio} slot.
    Its value defaults to 50. It can be changed by the user.
    Use the function \code{setNFrontierPoints} to modify this selection.
    
    
    The final \code{status} of portfolio optimization is returned 
    and stored in the \code{portfolio} slot. Before optimization the
    value is unset to \code{NA}, after optimization a value of 
    \code{status=0} means a successful termination. For other
    values we recommend to inspect the help page of the selected
    solver, the name of the solver can be returned by the function
    \code{getSolver}.
    Use the function \code{setSolver} to reset the value to \code{NA}
    if it should be required.
    
    
    \bold{Optim Slot:}
    
    
    The name of the default solver used for optimization can be retrieved
    calling the function \code{getSolver}. The default value for the
    value \code{solver} in the specification is set to \code{NULL} 
    which means that the best solver availalbe will be autoselected 
    and used. Before optimization the user can change the setting
    to another solver. Be aware, that a possible personal change will 
    be overwritten by the function \code{setType}, so call 
    \code{setSolver} after setting the type of the portfolio.
    
    
    The logical flag \code{trace} in the slot \code{optim} allows
    to trace optionally the portfolio optimization process. By 
    default this will not be the case since the default value is 
    \code{trace=FALSE}. Use the fanction \code{setTrace} to modify
    the selection.
        
    
    \bold{Retrieving and Modifying Specification Settings:}
    
             
    Information about the current portfolio specification can be retrieved 
    by \code{"get"} functions. These include:
    
    \tabular{ll}{
    \code{getType} \tab Extracts portfolio type from specification, \cr
    \code{getOptimize} \tab Extracts what to optimize from specification, \cr
    \code{getEstimator} \tab Extracts type of covariance estimator, \cr
    \code{getTailRisk} \tab Extracts list of tail dependency risk matrixes, \cr
    \code{getParams} \tab Extracts parameters from specification, \cr
    \code{getWeights} \tab Extracts weights from a portfolio object, \cr
    \code{getTargetReturn} \tab Extracts target return from specification, \cr
    \code{getTargetRisk} \tab Extracts target riks from specification, \cr
    \code{getAlpha} \tab Extracts target VaR-alpha specification, \cr
    \code{getRiskFreeRate} \tab Extracts risk free rate from specification, \cr
    \code{getNFrontierPoints} \tab Extracts number of frontier points, \cr 
    \code{getStatus} \tab Extracts the status of optimization, \cr 
    \code{getSolver} \tab Extracts solver from specification, \cr
    \code{getTrace} \tab Extracts solver's trace flag. }
    
    For details we refer to \code{link{getSpec}}.
    
    To modify the setting from a portfolio specification use the 
    \code{"set"} functions:
    
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
    \code{setTrace} \tab Sets the logical trace flag. }
    
    
    For details we refer to \code{link{setSpec}}.
      
    \bold{Printing Specification Settings:}
    
    There is a generic print function to print information from
    specification. What is printed depends on the values of the
    settings. For example \code{print(portfolioSpec())} returns
    the type of portfolio, the name of the covariance estimator,
    the portfolios risk free rate, and the desired solver.
    
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
   spec = portfolioSpec()
   
## getRiskFreeRate -
   # Get Risk Free Rate
   getRiskFreeRate(spec) 
   spec
   
## setRiskFreeRate -
   # Modify Risk Free Rate
   setRiskFreeRate(spec) <- 2.5

}


\keyword{models}

