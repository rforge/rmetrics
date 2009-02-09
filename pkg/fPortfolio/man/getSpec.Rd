\name{getSpec}


%%\alias{getSpec} %%conflicts with getDefault

\alias{getModel.fPFOLIOSPEC}
\alias{getType.fPFOLIOSPEC}  
\alias{getOptimize.fPFOLIOSPEC}        
\alias{getEstimator.fPFOLIOSPEC} 
\alias{getTailRisk.fPFOLIOSPEC}      
\alias{getParams.fPFOLIOSPEC} 
\alias{getAlpha.fPFOLIOSPEC}  
\alias{getA.fPFOLIOSPEC}      
 
\alias{getPortfolio.fPFOLIOSPEC}        
\alias{getWeights.fPFOLIOSPEC}         
\alias{getTargetReturn.fPFOLIOSPEC}    
\alias{getTargetRisk.fPFOLIOSPEC}       
\alias{getRiskFreeRate.fPFOLIOSPEC}    
\alias{getNFrontierPoints.fPFOLIOSPEC} 
\alias{getStatus.fPFOLIOSPEC}

\alias{getOptim.fPFOLIOSPEC}
\alias{getSolver.fPFOLIOSPEC}   
\alias{getObjective.fPFOLIOSPEC} 
\alias{getOptions.fPFOLIOSPEC}       
\alias{getControl.fPFOLIOSPEC}        
\alias{getTrace.fPFOLIOSPEC}           


\title{Portfolio Specification Extractor Functions}


\description{
  
    Extracts information from an object of class fPFOLIOSPEC. 
    
}
    

\usage{
\method{getModel}{fPFOLIOSPEC}(object)
\method{getType}{fPFOLIOSPEC}(object)
\method{getOptimize}{fPFOLIOSPEC}(object)
\method{getEstimator}{fPFOLIOSPEC}(object)
\method{getTailRisk}{fPFOLIOSPEC}(object)
\method{getParams}{fPFOLIOSPEC}(object)

\method{getPortfolio}{fPFOLIOSPEC}(object)
\method{getWeights}{fPFOLIOSPEC}(object)
\method{getTargetReturn}{fPFOLIOSPEC}(object)
\method{getTargetRisk}{fPFOLIOSPEC}(object)
\method{getAlpha}{fPFOLIOSPEC}(object)
\method{getRiskFreeRate}{fPFOLIOSPEC}(object)
\method{getNFrontierPoints}{fPFOLIOSPEC}(object)
\method{getStatus}{fPFOLIOSPEC}(object)

\method{getOptim}{fPFOLIOSPEC}(object)
\method{getSolver}{fPFOLIOSPEC}(object)
\method{getObjective}{fPFOLIOSPEC}(object)
\method{getOptions}{fPFOLIOSPEC}(object)
\method{getControl}{fPFOLIOSPEC}(object)
\method{getTrace}{fPFOLIOSPEC}(object)
}


\arguments{

    \item{object}{
        an object of class \code{fPFOLIOSPEC}.
        }

}


\details{

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
    \code{getobjective} \tab Extracts name of objective function, \cr
    \code{getTrace} \tab Extracts solver's trace flag. }
    
}


\references{

Wuertz, D., Chalabi, Y., Chen W., Ellis A. (2009);
    \emph{Portfolio Optimization with R/Rmetrics}, 
    Rmetrics eBook, Rmetrics Association and Finance Online, Zurich.
    
}


\examples{
## get -  
   # Create default specification structure:
   Spec = portfolioSpec()
   Spec
   
   # Extract from Model Slot:
   getModel(Spec)
   getType(Spec)
   getOptimize(Spec)
   getEstimator(Spec)
   getTailRisk(Spec)
   getParams(Spec)

   # Extract from Portfolio Slot:
   getPortfolio(Spec)
   getWeights(Spec)
   getTargetReturn(Spec)
   getTargetRisk(Spec)
   getAlpha(Spec)
   getRiskFreeRate(Spec)
   getNFrontierPoints(Spec)
   getStatus(Spec)
   
   # Extract from Optim Slot:
   getOptim(Spec)
   getSolver(Spec)
   getObjective(Spec)
   getOptions(Spec)
   getControl(Spec)
   getTrace(Spec)
}
   
   
\keyword{models}

