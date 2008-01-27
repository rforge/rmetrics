\name{portfolioFrontier}


\alias{portfolioFrontier}


\title{Efficient Portfolio Frontier}


\description{
  
    Compoutes the efficient portfolio frontier.
    
}
    

\usage{
portfolioFrontier(data, spec = portfolioSpec(), constraints = "LongOnly")
}



\arguments{

    \item{constraints}{
        a character string vector, containing the constraints of the form\cr
        \code{"minW[asset]=percentage"} for box constraints resp. \cr
        \code{"maxsumW[assets]=percentage"} for sector constraints.
        }
    \item{data}{
        a multivariate time series described by an S4 object of class
        \code{timeSeries}. If your timeSerie is not a \code{timeSeries}
        object, consult the generic function \code{as.timeSeries} to 
        convert your time series.
        }
    \item{spec}{
        an S4 object of class \code{fPFOLIOSPEC}, containing slots call, model,
        portfolio, title, description, see \code{\link{PortfolioSpec}}
        for a full slot description.
        }

}


\details{
    
    \bold{Portfolio Frontier:}
    \cr\cr   
    

    The function \code{portfolioFrontier} calculates the whole efficient 
    frontier. The portfolio information consists of five arguments: data, 
    specifications, constraints, title and description. 
    
    The range of the frontier is determined from the range of the asset
    returns, and the number of equidistant points in the returns, is
    calculated from the number of frontier points hold in the specifrication 
    structure. To extract or to modify the number of frontier points
    use the functions \code{getNFrontierPoints} and \code{setNFrontierPoints}.
   
    
    The \code{frontierPortfolio} function returns the properties of 
    the the efficient frontier as an S4 object of class \code{fPORTFOLIO}.

}


\value{
    
    \code{portfolioFrontier} function returns an S4 object of class 
    \code{"fPORTFOLIO"}.
    
}



%\references{}


\seealso{

   \code{portfolioData},
   \code{portfolioSpec},
   \code{portfolioConstraints},
   \code{feasiblePortfolio},
   \code{efficientFrontier}.

}


\author{

    Diethelm Wuertz and Oliver Greshake for the Rmetrics port.
    
}


\examples{
## portfolioFrontier -
   
   # Load Data and Convert to timeSeries Object:
   Data = as.timeSeries(data(smallcap.ts))
   Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
   Data
   
   # Set Default Specifications:
   Spec = portfolioSpec()
   Spec
   setNFrontierPoints = 10
   
   # Allow for unlimited Short Selling:
   Constraints = "LongOnly"
   
   # Compute 10 points efficient frontier:
   portfolioFrontier(Data, Spec, Constraints)  
}


\keyword{models}

