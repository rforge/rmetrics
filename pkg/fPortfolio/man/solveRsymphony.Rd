\name{solveRsymphony}


\alias{solveRsymphony}


\title{Linear and Mixed Integer Programming Solver}


\description{

    Interface to optimize a portfolio using the Rsymphony solver.

}


\usage{
solveRsymphony(data, spec, constraints)
}



\arguments{

    \item{data}{
        a time series or a named list, containing either a series of returns 
        or named entries 'mu' and 'Sigma' being mean and covariance matrix.
        }
    \item{spec}{
        an S4 object of class \code{fPFOLIOSPEC} as returned by the function
        \code{portfolioSpec}.
        }
    \item{constraints}{
        a character string vector, containing the constraints of the form\cr
        \code{"minW[asset]=percentage"} for box constraints resp. \cr
        \code{"maxsumW[assets]=percentage} for sector constraints.
        }
}


\references{
SYMPHONY development home page \url{https://projects.coin-or.org/SYMPHONY/wiki}.
}


\examples{
## data -
   # Load Data:
   Data <- as.timeSeries(data(smallcap.ts))
   Data <- Data[, c("BKE", "GG", "GYMB", "KRON")]
   Data
   
## solveRsymphony - 
   # Portfolio specification and constraints:
   Spec <- portfolioSpec()
   setType(Spec) <- "CVaR"
   setTargetReturn(Spec) <- mean(colMeans(Data))
   Constraints <- "LongOnly"

   # Optimize:
   solveRsymphony(Data, Spec, Constraints)
}


\keyword{models}

