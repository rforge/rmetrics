\name{solveRlpSolve}


\alias{solveRlpSolve}
\alias{solveRlp}


\title{Linear Programming Solver}


\description{

    Optimizes a portfolio with a the linear
    programming solver lpSolve.

}


\usage{
solveRlp(data, spec, constraints)
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
        \code{"maxsumW[assets]=percentage} for sector constraints.
        }
}


\note{

    The function \code{solveRlp} uses the R function interface from 
    the CRAN contributed R package \code{lpSolve} and the function 
    \code{solveRlp} uses the R function interface from the R-Forge 
    contributed R package \code{lpSolveAPI}.
    
}


%\references{}


\examples{
## data - 
   # Load Data:
   Data = as.timeSeries(data(smallcap.ts))
   Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
   Data
    
## solveRlpSolve -
   # ...
}


\keyword{models}

