\name{solveRipop}


\alias{solveRipop}


\title{Interior Point Optimization}


\description{

    An interior point optimization method to solve quadratic 
    programming problems.

}


\usage{
solveRipop(data, spec, constraints)
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
   Data <- as.timeSeries(data(smallcap.ts))
   Data <- Data[, c("BKE", "GG", "GYMB", "KRON")]
   Data
   
## solveRipop - 
   # Portfolio specification and constraints:
   Spec <- portfolioSpec()
   setTargetReturn(Spec) <- mean(colMeans(Data))
   Constraints <- "LongOnly"

   # Optimize:
   solveRipop(Data, Spec, Constraints)
}


\keyword{models}

