\name{frontierPoints}


\alias{frontierPoints}


\title{Get Frontier Points}


\description{
  
    Extracts the risk and return coordinates of
    the efficient frontier.
    
}
    

\usage{
frontierPoints(object, frontier = c("both", "lower", "upper"), 
    return = "mean", risk = "cov")
}



\arguments{

    \item{object}{
        an object of class \code{fPORTFOLIO}.
        }
    \item{frontier}{
        a character string denoting which part of the
        efficient portfolio should be extractacted.
        }
    \item{return, risk}{
        character strings denoting which return and risk 
        has to be plotted. Allowed values are for the
        return "mean", and for the risk either "cov,"
        "VaR", or "CVaR".
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

