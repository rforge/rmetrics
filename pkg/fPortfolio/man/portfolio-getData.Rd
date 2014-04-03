\name{portfolio-getData}


%%\alias{getData} %%

\alias{getData.fPFOLIODATA}
\alias{getSeries.fPFOLIODATA}
\alias{getNAssets.fPFOLIODATA}
\alias{getUnits.fPFOLIODATA}

\alias{getStatistics.fPFOLIODATA}
\alias{getMean.fPFOLIODATA}
\alias{getCov.fPFOLIODATA}
\alias{getMu.fPFOLIODATA}
\alias{getSigma.fPFOLIODATA}
\alias{getEstimator.fPFOLIODATA}

\alias{getTailRisk.fPFOLIODATA}


\title{Portfolio Data Extractor Functions}


\description{
  
    Extracts information from an object of class fPFOLIODATA. 
    
}
    

\usage{
\method{getData}{fPFOLIODATA}(object)
\method{getSeries}{fPFOLIODATA}(object)
\method{getNAssets}{fPFOLIODATA}(object)
\method{getUnits}{fPFOLIODATA}(object)

\method{getStatistics}{fPFOLIODATA}(object)
\method{getMean}{fPFOLIODATA}(object)
\method{getCov}{fPFOLIODATA}(object)
\method{getMu}{fPFOLIODATA}(object)
\method{getSigma}{fPFOLIODATA}(object)
\method{getEstimator}{fPFOLIODATA}(object)

\method{getTailRisk}{fPFOLIODATA}(object)
}


\arguments{

    \item{object}{
        an object of class \code{fPFOLIODATA}.
        }

}


\details{

    \tabular{ll}{
    \code{getData} \tab Extracts data slot, \cr
    \code{getSeries} \tab Extracts assets series, \cr
    \code{getNAssets} \tab Extracts number of assets, \cr
    \code{getUnits} \tab Extracts names of assets, \cr
    
    \code{getStatistics} \tab Extracts statistics slot, \cr
    \code{getMean} \tab Extracs mean vector, \cr
    \code{getCov} \tab Extracs covariance matrix, \cr
    \code{getMu} \tab Extracs mu vector, \cr
    \code{getSigma} \tab Extracs Sigma matrix, \cr
    \code{getEstimator} \tab Extracs Sigma matrix, \cr
    
    \code{getTailRisk} \tab Extracts tail risk slot. }  
}


\references{

Wuertz, D., Chalabi, Y., Chen W., Ellis A. (2009);
    \emph{Portfolio Optimization with R/Rmetrics}, 
    Rmetrics eBook, Rmetrics Association and Finance Online, Zurich.
    
}


\keyword{models}

