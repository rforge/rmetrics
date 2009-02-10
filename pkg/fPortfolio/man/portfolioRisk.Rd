\name{portfolioRisk}


\alias{portfolioRisk}

\alias{covRisk}
\alias{varRisk}
\alias{cvarRisk}


\title{portfolioRisk}


\description{

    Computes portfolio risk.

}


\usage{
covRisk(data, weights)
varRisk(data, weights, alpha = 0.05)
cvarRisk(data, weights, alpha = 0.05)
}


\arguments{

    \item{data}{
        a multivariate time series described by an S4 object of class
        \code{timeSeries}.
        }
    \item{weights}{ 
        a numeric vector of weights. 
        }
    \item{alpha}{
        a numeric value, the confidence level, by default \code{alpha=0.05},
        i.e. 5\%.
        }
}


\references{

Wuertz, D., Chalabi, Y., Chen W., Ellis A. (2009);
    \emph{Portfolio Optimization with R/Rmetrics}, 
    Rmetrics eBook, Rmetrics Association and Finance Online, Zurich.
    
}


\examples{
## data -
   Data = SMALLCAP.RET
   Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
   Data
   
## weights - 
   nAssets = getNAssets(portfolioData(Data))
   Weights <- rep(1/nAssets, times = nAssets)
   
## covRisk -
   covRisk(Data, Weights)

## varRisk -
   varRisk(Data, Weights, alpha = 0.05)

## cvarRisk -
   cvarRisk(Data, Weights, alpha = 0.05)
}


\keyword{models}

