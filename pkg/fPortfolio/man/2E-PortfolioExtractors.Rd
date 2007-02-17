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


\title{Portfolio Extractor Functions}


\description{
  
    A collection and description of ...
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
    \code{getTargetRisk} \tab Extracts target riks from a portfolio. }
    
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
}


\details{

    \bold{Porfolio Extractor Functions:}
    \cr\cr
    Describe the data set ...

}


\arguments{

    \item{doplot}{
        ...
        }
    \item{frontier}{
        ...
        }
    \item{object}{
        ...
        }
    \item{\dots}{
        ...
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
    return a ...
  
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

