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
        \code{"maxsumW[assets]=percentage"} for sector constraints.
        }
}


\value{
    
    a list with the following named ebtries: 
    \code{solver}, 
    \code{optim},
    \code{weights},
    \code{targetReturn},
    \code{targetRisk}, 
    \code{objective},
    \code{status},
    \code{message}.

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
   
## spec - 
   Spec = portfolioSpec()
   setSolver(Spec) = "solveRshortExact" 
   setTargetReturn(Spec) = mean(Data)
   Spec

## constraints - 
   Constraints = "LongOnly"
         
## solveRshortExact -
   solveRshortExact(Data, Spec, Constraints)
}


\keyword{models}

