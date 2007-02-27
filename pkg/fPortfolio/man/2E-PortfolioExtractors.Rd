\name{PortfolioExtractors}

\alias{PortfolioExtractors}

\alias{getAssets}
\alias{getStatistics}
\alias{getNumberOfAssets}
\alias{getSpecification}   
\alias{getPortfolio}       
\alias{getFrontier}        
\alias{getWeights}       
\alias{getTargetReturn}   
\alias{getTargetRisk}  
\alias{getTargetStdev}    


\title{Portfolio Extractor Functions}


\description{
  
    A collection and description of xxx
    \cr
    
    The data sets are:
    
    \tabular{ll}{
    \code{getAssets} \tab Extracts assets series data, if available, \cr
    \code{getStatistics} \tab Extracts assets statistics, mean and covariance, \cr
    \code{getNumberOfAssets} \tab Extracts number of assets from statistics, \cr
    \code{getSpecification} \tab Extracts @specification slot, \cr
    \code{getPortfolio} \tab Extracts @portfolio slot, \cr
    \code{getFrontier} \tab Extracts the efficient frontier, \cr
    \code{getWeights} \tab Extracts weights from a fPORTFOLIO object, \cr
    \code{getTargetReturn} \tab Extracts target return from a portfolio, \cr
    \code{getTargetRisk} \tab Extracts target riks from a portfolio, \cr
    \code{getTargetStdev} \tab Extracts target standard deviation from a portfolio. }
    
}
    

\usage{
getAssets(object)
getStatistics(object)
getNumberOfAssets(object)
getSpecification(object)   
getPortfolio(object, doplot = FALSE, \dots)     
getFrontier(object, frontier = c("both", "lower", "upper"), doplot = FALSE, \dots)        
getWeights(object, doplot = FALSE, \dots)      
getTargetReturn(object)  
getTargetRisk(object)
getTargetStdev(object)
}


\details{

    \bold{Porfolio Extractor Functions:}
    \cr\cr
    Describe the data set xxx

}


\arguments{

    \item{doplot}{
        xxx
        }
    \item{frontier}{
        xxx
        }
    \item{object}{
        xxx
        }
    \item{\dots}{
        xxx
        }

}


\value{
  
    \code{getAssets}\cr
    \code{getStatistics}\cr
    \code{getNumberOfAssets}\cr
    \code{getSpecification}\cr
    \code{getPortfolio}\cr
    \code{getFrontier}\cr
    \code{getWeights}\cr
    \code{getTargetReturn}\cr
    \code{getTargetRisk}\cr
    \code{getTargetStdev}\cr
    return a xxx
  
}


%\references{}


\seealso{

    \code{xxx}.

}


\author{

    Diethelm Wuertz and Oliver Greshake for the Rmetrics port.
    
}


\examples{

## fun -
   # fun()
 
}


\keyword{models}

