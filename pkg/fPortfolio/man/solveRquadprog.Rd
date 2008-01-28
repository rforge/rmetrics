\name{solveRquadprog}


\alias{solveRquadprog}


\title{Quadratic Programming Solver}


\description{

    Optimizes a portfolio with a the quadratic
    programming solver quadprog.

}


\usage{
solveRquadprog(data, spec, constraints)
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


\author{

    Diethelm Wuertz and Oliver Greshake for the Rmetrics port.

}


\examples{
## data - 
   # Load Data:
   Data = as.timeSeries(data(smallcap.ts))
   Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
   Data
   
## solveRquadprog -
   # ...
}


\keyword{models}

