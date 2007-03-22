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
  
    A collection of extractors for S4 objects 
    of class \code{fPORTFOLIO}. The aim of 
    these functions is to get an quick access 
    to the desired data.
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
    \code{getTargetRisk} \tab Extracts target risk from a portfolio, \cr
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


\details{

    \bold{Portfolio Extractor Functions:}
    \cr\cr
    The extractor functions allow an quick access to all important information 
    of the optimization problem. The functions \code{getFrontier, getPortfolio,
    getWeights} also provide direct plotting of the data with argument doplot.

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
    all functions return a list or a value of the desired information.
  
}


%\references{}


\seealso{

   \code{\link{PortfolioData}},
   \code{\link{PortfolioSpecification}},
   \code{\link{PortfolioConstraints}},
   \code{\link{fPORTFOLIO}},\cr
   \code{\link{PortfolioPlots}}.

}


\author{

    Diethelm Wuertz and Oliver Greshake for the Rmetrics port.
    
}


\examples{
## getStatistics -
   # Calculation of Long Only Efficient Frontier
   frontier = portfolioFrontier(usPortfolioData())
   # Getting the Statistics
   getStatistics(frontier)

## getStatistics -
   # Getting and Plotting the Efficient Frontier
   getFrontier(frontier, doplot = TRUE)     
}


\keyword{models}

