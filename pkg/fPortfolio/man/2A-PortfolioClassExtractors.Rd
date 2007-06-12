\name{PortfolioClassExtractors}

\alias{PortfolioClassExtractors}

\alias{getData}
\alias{getSpec}
\alias{getConstraints}
\alias{getPortfolio}
\alias{getWeights}
\alias{getTargetReturn}
\alias{getTargetRisk}
\alias{getTargetAlpha}
\alias{getStatus}

\alias{getFrontier}
\alias{getRiskBudgets}
\alias{getTailRiskBudgets}


\title{Portfolio Class Extractors}


\description{
  
    A collection and description of functions 
    allowing to get information about an object
    of class fPORTFOLIO. 
    \cr
    
    The functions are:
    
    \tabular{ll}{
    \code{getData} \tab Extracts ..., \cr
    \code{getSpec} \tab Extracts ..., \cr
    \code{getConstraints} \tab Extracts ..., \cr
    \code{getPortfolio} \tab Extracts ..., \cr
    \code{getWeights} \tab Extracts ..., \cr
    \code{getTargetReturn} \tab Extracts ..., \cr
    \code{getTargetRisk} \tab Extracts ..., \cr
    \code{getTargetAlpha} \tab Extracts ..., \cr
    \code{getStatus} \tab Extracts ..., \cr
    \code{getFrontier} \tab Extracts ..., \cr
    \code{getRiskBudgets} \tab Extracts ..., \cr
    \code{getTailRiskBudgets} \tab Extracts ... . }
    
}
    

\usage{
\method{getData}{fPORTFOLIO}(object)
\method{getSpec}{fPORTFOLIO}(object)
\method{getConstraints}{fPORTFOLIO}(object)
\method{getPortfolio}{fPORTFOLIO}(object)
\method{getWeights}{fPORTFOLIO}(object)
\method{getTargetReturn}{fPORTFOLIO}(object)
\method{getTargetRisk}{fPORTFOLIO}(object)
\method{getTargetAlpha}{fPORTFOLIO}(object)
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

