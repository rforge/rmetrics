\name{feasibleSet}


\alias{feasibleHull}
\alias{feasibleSet}
\alias{feasibleSetPlot}


\title{Feasible Set of a Portfolio}


\description{
  
    Explores the feasible set of a portfolio.
    
    The functions are:

    \tabular{ll}{
    \code{feasibleHull} \tab Returns the convex Hull of the feasible set, \cr
    \code{feasibleSet} \tab computes feasible portfolios on a Risk/Return grid, \cr
    \code{feasibleSetPlot} \tab underlies a frontier with an image and contour plot. }
    
}
    

\usage{
feasibleHull(data, spec = portfolioSpec(), constraints = "LongOnly", 
    trace = TRUE, doplot = TRUE)

feasibleSet(data, spec = portfolioSpec(), constraints = "LongOnly",  
    trace = TRUE, doplot = TRUE, locator = FALSE)
feasibleSetPlot(object, FUN = cvarRisk, ...)
}



\arguments{

    \item{data}{
        a multivariate time series described by an S4 object of class
        \code{timeSeries}. If your timeSerie is not a \code{timeSeries}
        object, consult the generic function \code{as.timeSeries} to 
        convert your time series.
        }
    \item{constraints}{
        a character string vector, containing the constraints of the form\cr
        \code{"minW[asset]=percentage"} for box constraints resp. \cr
        \code{"maxsumW[assets]=percentage"} for sector constraints.
        }
    \item{spec}{
        an S4 object of class \code{fPFOLIOSPEC} as returned by the function
        \code{portfolioSpec}.
        }
    \item{object}{
        a feasible set, an object as returned by the function 
        \code{feasibleSet}.
        }
    \item{trace}{
        a logical value, should the results of the computation be traced?
        }
    \item{doplot}{
        a logical value, should the results of the computation be plotted?
        }
    \item{locator}{
        a logical value, allows to locate the weights for the nearest 
        available point to the locator.
        }
    \item{FUN}{
        a function name for the function to be displayed as the background
        image for the feasible set.
        }
    \item{...}{
        optional arguments passed to functopn \code{FUN}.
        }

}


\value{
    
    \code{feasibleHull}\cr
    a numeric matrix with three columns, the target returns, the minimal
    and maximal target risks along the hull.
    \cr
    
    \code{feasibleSet}\cr
    returns a list with the following entries: \code{data}, \code{weights},
    \code{coords}, \code{positions}, \code{targetReturns}, \code{targetRisk},
    and \code{hull}.
    \cr
    
    \code{feasibleSetPlot}\cr
    generates a plot, returning \code{invisible()}.
    
}


\references{

Wuertz, D., Chalabi, Y., Ellis A., Theussl S. (2009);
    \emph{Advanced Portfolio Optimization with R/Rmetrics}, 
    Rmetrics eBook, Rmetrics Association and Finance Online, Zurich.
    
}


\examples{
## feasibleHull -
   data = 100*LPP2005.RET[, 1:6]
   feasibleHull(data) 
   
## feasibleSet -
   # be patient, that takes some time ...
   set = feasibleSet(data, trace = FALSE)
   
## feasibleSetPlot -
   # CVaR:
   feasibleSetPlot(set, FUN = cvarRisk, alpha = 0.10)  
   title(main = "CVaR - LPP2005 Pension Fund Portfolio")
   # Max Risk Budgets:
   maxRiskBudget <- function(data, weights)
     max(riskBudgets(data, weights))
   feasibleSetPlot(set, maxRiskBudget)
   title(main = "Max Risk Budget - LPP2005")
   # Sharpe Ratio:
   sharpeRatio <- function(data, weights) {
     return = mean( data \%*\% weights )
     risk = sqrt( t(weights) \%*\% cov(data) \%*\% weights )[[1]] 
     return / risk }
   feasibleSetPlot(set, sharpeRatio)
   title(main = "Sharpe Ratio - LPP2005")
}


\keyword{models}

