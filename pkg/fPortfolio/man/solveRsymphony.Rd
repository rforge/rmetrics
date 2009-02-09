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

SYMPHONY development home page 
    \url{https://projects.coin-or.org/SYMPHONY/wiki}.

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
   setType(Spec) = "CVaR" 
   setSolver(Spec) = "solveRsymphony" 
   setTargetReturn(Spec) = mean(Data)
   Spec

## constraints - 
   Constraints = "LongOnly"
         
## solveRsymphony -
   solveRsymphony(Data, Spec, Constraints)
}


\keyword{models}

