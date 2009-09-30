\name{solveRlpSolve}


\alias{solveRlpSolve}


\title{Linear Programming Solver}


\description{

    Optimizes a portfolio with a linear solver lpSolve.

}


\usage{
solveRlpSolve(data, spec, constraints)
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



\references{

    lp\_solve: \url{http://lpsolve.sourceforge.net/5.5/index.htm}
    
}

\examples{
## data -
   # Load Data:
   Data <- as.timeSeries(data(smallcap.ts))
   Data <- Data[, c("BKE", "GG", "GYMB", "KRON")]
   Data
   
## solveRlpSolve - 
   # Portfolio specification and constraints:
   Spec <- portfolioSpec()
   setType(Spec) <- "CVaR"
   setTargetReturn(Spec) <- mean(colMeans(Data))
   Constraints <- "LongOnly"

   # Optimize:
   solveRlpSolve(Data, Spec, Constraints)
}


\keyword{models}

