\name{Extractors}

\alias{Extractors}


\alias{getConstraints}
\alias{getData}
\alias{getEstimator}
\alias{getFrontier}
\alias{getMu}
\alias{getNames}
\alias{getNumberOfAssets}
\alias{getNFrontierPoints}
\alias{getPortfolio}
\alias{getParams}
\alias{getRiskBudgets} 
\alias{getRiskFreeRate} 
\alias{getSeries}
\alias{getSigma}
\alias{getSolver}
\alias{getSpec}
\alias{getStatistics}
\alias{getTargetAlpha}
\alias{getTailRisk}
\alias{getTailRiskBudgets} 
\alias{getTargetReturn}
\alias{getTargetRisk}
\alias{getTrace}
\alias{getType}
\alias{getWeights}


\title{Extractors}


\description{
  
    A collection and description of functions 
    allowing to get information about objects
    of class fPFOLIODATA, FPFOLIOSPEC, and 
    fPORTFOLIO. 
    \cr
    
    The functions are:
    
}
    

\usage{
getConstraints(object)
getData(object)
getEstimator(object)
getFrontier(object, \dots)
getMu(object)
getNames(object)
getNumberOfAssets(object)
getNFrontierPoints(object)
getPortfolio(object)
getParams(object)
getRiskBudgets(object)
getRiskFreeRate(object)
getSeries(object)
getSigma(object)
getSolver(object)
getSpec(object)
getStatistics(object)
getTargetAlpha(object)
getTailRisk(object)
getTailRiskBudgets(object) 
getTargetReturn(object)
getTargetRisk(object)
getTrace(object)
getType(object)
getWeights(object)
}



\arguments{

    \item{object}{
        an object of class \code{fPFOLIODATA}, \code{fPFOLIOSPEC} or
        \code{fPORTFOLIO}.
        }
    \item{\dots}{
        [getPortfolio, getFrontier, getWeights] - \cr
        optional arguments to be passed.\cr
        }

}


\author{

    Diethelm Wuertz and Oliver Greshake for the Rmetrics port.
    
}


\examples{
## 
}


\keyword{models}

