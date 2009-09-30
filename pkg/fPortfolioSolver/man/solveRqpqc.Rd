\name{solveRqpqc}


\alias{solveRqpqc}


\title{Second Order Cone QPQC Optimizer}


\description{

    Second order cpnd programming solver for quadratic objectives
    with quadratic constraints.

}


\usage{
solveRqpqc(data, spec, constraints)
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
        \code{"maxsumW[assets]=percentage"} for sector constraints.
        }
}


%\references{}


\examples{
## data -
   # Load Data:
   Data <- as.timeSeries(data(smallcap.ts))
   Data <- Data[, c("BKE", "GG", "GYMB", "KRON")]
   Data
   
## solveRqpqc - 
   # Portfolio specification and constraints:
   Spec <- portfolioSpec()
   setTargetReturn(Spec) <- mean(colMeans(Data))
   Constraints <- "LongOnly"

## Optimize:
   # solveRqpqc(Data, Spec, Constraints)
}


\keyword{models}

