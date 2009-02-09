\name{getDefault}

\alias{getDefault}


\alias{getConstraints}
\alias{getControl}
\alias{getCov}
\alias{getCovRiskBudgets}
\alias{getData}
\alias{getEstimator}
\alias{getMean}
\alias{getMu}
\alias{getNames}
\alias{getNAssets}
\alias{getNFrontierPoints}
\alias{getObjective}
\alias{getOptim}
\alias{getOptions}
\alias{getOptimize}
\alias{getPortfolio}
\alias{getParams} 
\alias{getRiskFreeRate} 
\alias{getSeries}
\alias{getSigma}
\alias{getSolver}
\alias{getSpec}
\alias{getStatistics}
\alias{getStatus}
\alias{getAlpha}
\alias{getTailRisk}
\alias{getTailRiskBudgets} 
\alias{getTargetReturn}
\alias{getTargetRisk}
\alias{getTrace}
\alias{getType}
\alias{getWeights}


\title{Extractor Functions}


\description{
  
    Extractor functions to get information from objects
    of class fPFOLIODATA, fPFOLIOSPEC, fPFOLIODATA, fPFOLIOVAL,
    and fPORTFOLIO.
    
}
    

\usage{
getConstraints(object)
getControl(object)
getCov(object)
getCovRiskBudgets(object)
getData(object)
getEstimator(object)
getMean(object)
getMu(object)
getNames(object)
getNAssets(object)
getNFrontierPoints(object)
getObjective(object)
getOptim(object)
getOptions(object)
getOptimize(object)
getPortfolio(object)
getParams(object)
getRiskFreeRate(object)
getSeries(object)
getSigma(object)
getSolver(object)
getSpec(object)
getStatistics(object)
getStatus(object)
getAlpha(object)
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
        optional arguments to be passed.\cr
        }

}


\references{

Wuertz, D., Chalabi, Y., Chen W., Ellis A. (2009);
    \emph{Portfolio Optimization with R/Rmetrics}, 
    Rmetrics eBook, Rmetrics Association and Finance Online, Zurich.
    
}


\examples{
## getModel -
   getModel(portfolioSpec())
   getType(portfolioSpec())
}


\keyword{models}

