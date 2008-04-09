\name{frontierPoints}


\alias{frontierPoints}


\title{Get Frontier Points}


\description{
  
    Extracts the risk and return coordinates of
    the efficient frontier.
    
}
    

\usage{
frontierPoints(object, frontier = c("both", "lower", "upper"), 
    return = c("mean", "mu"), risk = c("cov", "sigma", "CVaR", "VaR"))
}



\arguments{

    \item{object}{
        an object of class \code{fPORTFOLIO}.
        }
    \item{frontier}{
        a character string denoting which part of the
        efficient portfolio should be extractacted.
        }
    \item{return}{
        character strings denoting which return measure
        should be plotted. Allowed values for the
        return are either \code{"mean"}, or \code{"mu"}.
        }
    \item{risk}{
        character strings denoting which risk measure
        should be plotted. Allowed values for the
        risk measure are either \code{"cov"}, \code{"sigma"},  
        \code{"VaR"}, or \code{"CVaR"}.
        }

}


\examples{
## portfolioFrontier -
   
   # Load Data and Convert to timeSeries Object:
   Data = as.timeSeries(data(smallcap.ts))
   Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
   
   # Compute Efficient Frontier:
   Frontier = portfolioFrontier(Data) 
   
## frontierPoints:

   # Extract Points and Plot:
   x = frontierPoints(Frontier, risk = "VaR")
   x = frontierPoints(Frontier, risk = "CVaR")

}


\keyword{models}

