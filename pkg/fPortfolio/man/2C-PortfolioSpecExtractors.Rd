\name{PortfolioSpecExtractors}

\alias{PortfolioSpecExtractors}


\alias{getType}         
\alias{getEstimator}       
\alias{getParams}       
         
\alias{getWeights}         
\alias{getTargetReturn}    
\alias{getTargetRisk}   
\alias{getTargetAlpha}     
\alias{getRiskFreeRate}    
\alias{getNFrontierPoints} 
         
\alias{getSolver}         
\alias{getTrace}           


\title{Portfolio Specification Extractors}


\description{
  
    A collection and description of functions 
    allowing to get information about an object
    of class fPFOLIOSPEC. 
    \cr
    
    The functions are:
    
    \tabular{ll}{
    \code{getType} \tab Extract portfolio type from specification, \cr
    \code{getEstimator} \tab Extract type of covariance estimator, \cr
    \code{getParams} \tab Extract parameters from specification, \cr
    \code{getWeights} \tab Extracts weights from a portfolio object, \cr
    \code{getTargetReturn} \tab Extracts target return from specification, \cr
    \code{getTargetRisk} \tab Extracts target riks from specification, \cr
    \code{getTargetAlpha} \tab Extracts target VaR-alpha specification, \cr
    \code{getRiskFreeRate} \tab Extracts risk free rate from specification, \cr
    \code{getNFrontierPoints} \tab Extracts number of frontier points, \cr 
    \code{getSolver} \tab Extracts solver from specification, \cr
    \code{getTrace} \tab Extracts solver's trace flag. }
    
}
    

\usage{
\method{getType}{fPFOLIODATA}(object)
\method{getEstimator}{fPFOLIODATA}(object)
\method{getParams}{fPFOLIODATA}(object)

\method{getWeights}{fPFOLIODATA}(object)
\method{getTargetReturn}{fPFOLIODATA}(object)
\method{getTargetRisk}{fPFOLIODATA}(object)
\method{getTargetAlpha}{fPFOLIODATA}(object)
\method{getRiskFreeRate}{fPFOLIODATA}(object)
\method{getNFrontierPoints}{fPFOLIODATA}(object)

\method{getSolver}{fPFOLIODATA}(object)
\method{getTrace}{fPFOLIODATA}(object)
}


\arguments{

    \item{object}{
        an object of class \code{fPFOLIOSPEC}.
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

