\name{solveRsocp}


\alias{solveRsocp}


\title{Second Order Cone Programming Solver}


\description{

    Optimizes a portfolio using a second order cone 
    programming solver.

}


\usage{
solveRsocp(data, spec, constraints)
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


%\references{}


\examples{
## data -
   # Load Data:
   Data = as.timeSeries(data(smallcap.ts))
   Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
   Data
   
## solveRsocp - 
   # Portfolio specification and constraints:
   Spec <- portfolioSpec()
   setOptimize(Spec) <- "maxReturn"
   setTargetReturn(Spec) <- NA
   setTargetRisk(Spec) <- sd(Data[,"BKE"])

   Constraints <- "LongOnly"

   # Optimize:
   solveRsocp(Data, Spec, Constraints)
}


\keyword{models}

