\name{PortfolioClassExtractors}

\alias{PortfolioClassExtractors}

\alias{getData.fPORTFOLIO}
\alias{getSeries.fPORTFOLIO}
\alias{getStatistics.fPORTFOLIO}
\alias{getNumberOfAssets.fPORTFOLIO}

\alias{getSpec.fPORTFOLIO}
\alias{getType.fPORTFOLIO}
\alias{getEstimator.fPORTFOLIO}
\alias{getParams.fPORTFOLIO}
\alias{getSolver.fPORTFOLIO}
\alias{getTrace.fPORTFOLIO}

\alias{getConstraints.fPORTFOLIO}

\alias{getPortfolio.fPORTFOLIO}
\alias{getWeights.fPORTFOLIO}
\alias{getTargetReturn.fPORTFOLIO}
\alias{getTargetRisk.fPORTFOLIO}
\alias{getTargetAlpha.fPORTFOLIO}
\alias{getRiskFreeRate.fPORTFOLIO}
\alias{getNFrontierPoints.fPORTFOLIO}
\alias{getStatus.fPORTFOLIO}

\alias{getFrontier.fPORTFOLIO}
\alias{getRiskBudgets.fPORTFOLIO}
\alias{getTailRiskBudgets.fPORTFOLIO}


\title{Portfolio Class Extractors}


\description{
  
    A collection and description of functions 
    allowing to get information about an object
    of class fPORTFOLIO. 
    \cr
    
    The functions are:
    
    \tabular{ll}{
    \code{getData} \tab Extracts ..., \cr
    \code{getSeries} \tab Extracts ..., \cr
    \code{getStatistics} \tab Extracts ..., \cr
    \code{getNumberOfAssets} \tab Extracts ..., \cr
    \code{getSpec} \tab Extracts ..., \cr
    \code{getType} \tab Extracts ..., \cr
    \code{getEstimator} \tab Extracts ..., \cr
    \code{getParams} \tab Extracts ..., \cr
    \code{getSolver} \tab Extracts ..., \cr
    \code{getTrace} \tab Extracts ..., \cr
    \code{getConstraints} \tab Extracts ..., \cr
    \code{getPortfolio} \tab Extracts ..., \cr
    \code{getWeights} \tab Extracts ..., \cr
    \code{getTargetReturn} \tab Extracts ..., \cr
    \code{getTargetRisk} \tab Extracts ..., \cr
    \code{getTargetAlpha} \tab Extracts ..., \cr
    \code{getRiskFreeRate} \tab Extracts ..., \cr
    \code{getNFrontierPoints} \tab Extracts ..., \cr
    \code{getStatus} \tab Extracts ..., \cr
    \code{getFrontier} \tab Extracts ..., \cr
    \code{getRiskBudgets} \tab Extracts ..., \cr
    \code{getTailRiskBudgets} \tab Extracts ... . }
    
}
    

\usage{
\method{getData}{fPORTFOLIO}(object)
\method{getSeries}{fPORTFOLIO}(object)
\method{getStatistics}{fPORTFOLIO}(object)
\method{getNumberOfAssets}{fPORTFOLIO}(object)

\method{getSpec}{fPORTFOLIO}(object)
\method{getType}{fPORTFOLIO}(object)
\method{getEstimator}{fPORTFOLIO}(object)
\method{getParams}{fPORTFOLIO}(object)
\method{getSolver}{fPORTFOLIO}(object)
\method{getTrace}{fPORTFOLIO}(object)

\method{getConstraints}{fPORTFOLIO}(object)

\method{getPortfolio}{fPORTFOLIO}(object)
\method{getWeights}{fPORTFOLIO}(object)
\method{getTargetReturn}{fPORTFOLIO}(object)
\method{getTargetRisk}{fPORTFOLIO}(object)
\method{getTargetAlpha}{fPORTFOLIO}(object)
\method{getRiskFreeRate}{fPORTFOLIO}(object)
\method{getNFrontierPoints}{fPORTFOLIO}(object)
\method{getStatus}{fPORTFOLIO}(object)

\method{getFrontier}{fPORTFOLIO}(object, frontier = c("both", "lower", "upper"), 
    doplot = FALSE, \dots)
\method{getRiskBudgets}{fPORTFOLIO}(object)
\method{getTailRiskBudgets}{fPORTFOLIO}(object)
}



\arguments{

    \item{doplot}{
        [getPortfolio, getFrontier, getWeights] - \cr
        a logical value, determining whether the extracted data should be plotted,
        by default\cr
        FALSE.
        }
    \item{frontier}{
        [getFrontier] - \cr
        a character string, determining which part of the frontier should be
        extracted. \code{"both"} stands for the full hyperbola, \code{"lower"}
        for all points below the minimum variance return and \code{"upper"} for
        the actual efficient frontier, by default "both".
        }
    \item{object}{
        an object of class \code{fPORTFOLIO}, containing slots call, data,
        specification, constraints, portfolio, title, description.
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

