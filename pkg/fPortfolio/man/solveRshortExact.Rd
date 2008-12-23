\name{solveRshortExact}


\alias{solveRshortExact}


\title{Exat unlimit Short Selling Solver}


\description{

    Optimizes an unlimited short selling portfolio 
    analytically.

}


\usage{
solveRshortExact(data, spec, constraints)
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
   Data = SMALLCAP.RET
   Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
   Data
   
## solveRshortExact - 
   # ...
}


\keyword{models}

