\name{PortfolioDataExtractors}

\alias{PortfolioDataExtractors}

\alias{getData}
\alias{getSeries}
\alias{getNumberOfAssets}

\alias{getStatistics}
\alias{getMu}
\alias{getSigma}

\alias{getTailrisk}


\title{Portfolio Data Extractors}


\description{
  
    A collection and description of functions 
    allowing to get information about an object
    of class fPFOLIODATA. 
    \cr
    
    The functions are:
    
    \tabular{ll}{
    \code{getData} \tab Extracts data slot, \cr
    \code{getSeries} \tab Extracts assets series data, \cr
    \code{getNumberOfAssets} \tab Extracts number of assets from statistics, \cr
    \code{getStatistics} \tab Extracts statistics slot, \cr
    \code{getMu} \tab Extracs mean  mu from statistics, \cr
    \code{getSigma} \tab Extracs covariance Sigma from statistics, \cr
    \code{getTailrisk} \tab Extracts tail risk slot. }  
    
}
    

\usage{
\method{getData}{fPFOLIODATA}(object)
\method{getSeries}{fPFOLIODATA}(object)
\method{getNumberOfAssets}{fPFOLIODATA}(object)

\method{getStatistics}{fPFOLIODATA}(object)
\method{getMu}{fPFOLIODATA}(object)
\method{getSigma}{fPFOLIODATA}(object)

\method{getTailrisk}{fPFOLIODATA}(object)
}


\arguments{

    \item{object}{
        an object of class \code{fPFOLIODATA}.
        }

}


%\references{}



\author{

    Diethelm Wuertz and Oliver Greshake for the Rmetrics port.
    
}


\examples{
## ...
}


\keyword{models}

