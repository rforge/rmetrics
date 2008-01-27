\name{PortfolioSpecExtractors}


\alias{PortfolioSpecExtractors}


\alias{getType.fPFOLIOSPEC}         
\alias{getEstimator.fPFOLIOSPEC} 
\alias{getTailRisk.fPFOLIOSPEC}      
\alias{getParams.fPFOLIOSPEC}       
         
\alias{getWeights.fPFOLIOSPEC}         
\alias{getTargetReturn.fPFOLIOSPEC}    
\alias{getTargetRisk.fPFOLIOSPEC}   
\alias{getTargetAlpha.fPFOLIOSPEC}     
\alias{getRiskFreeRate.fPFOLIOSPEC}    
\alias{getNFrontierPoints.fPFOLIOSPEC} 
\alias{getStatus.fPFOLIOSPEC}

\alias{getSolver.fPFOLIOSPEC}         
\alias{getTrace.fPFOLIOSPEC}           


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
    \code{getTailRisk} \tab Extract list of tail dependency risk matrixes, \cr
    \code{getParams} \tab Extract parameters from specification, \cr
    \code{getWeights} \tab Extracts weights from a portfolio object, \cr
    \code{getTargetReturn} \tab Extracts target return from specification, \cr
    \code{getTargetRisk} \tab Extracts target riks from specification, \cr
    \code{getTargetAlpha} \tab Extracts target VaR-alpha specification, \cr
    \code{getRiskFreeRate} \tab Extracts risk free rate from specification, \cr
    \code{getNFrontierPoints} \tab Extracts number of frontier points, \cr 
    \code{getStatus} \tab Extracts the status of optimization, \cr 
    \code{getSolver} \tab Extracts solver from specification, \cr
    \code{getTrace} \tab Extracts solver's trace flag. }
    
}
    

\usage{
\method{getType}{fPFOLIOSPEC}(object)
\method{getEstimator}{fPFOLIOSPEC}(object)
\method{getTailRisk}{fPFOLIOSPEC}(object)
\method{getParams}{fPFOLIOSPEC}(object)

\method{getWeights}{fPFOLIOSPEC}(object)
\method{getTargetReturn}{fPFOLIOSPEC}(object)
\method{getTargetRisk}{fPFOLIOSPEC}(object)
\method{getTargetAlpha}{fPFOLIOSPEC}(object)
\method{getRiskFreeRate}{fPFOLIOSPEC}(object)
\method{getNFrontierPoints}{fPFOLIOSPEC}(object)
\method{getStatus}{fPFOLIOSPEC}(object)

\method{getSolver}{fPFOLIOSPEC}(object)
\method{getTrace}{fPFOLIOSPEC}(object)
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
## get -
   
   # Create default specification structure:
   Spec = portfolioSpec()
   
   # Get:
   getType(Spec)
   getgetRiskFreeRate(Spec)  
}


\keyword{models}

