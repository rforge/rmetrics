\name{portfolioSolver}

\alias{portfolioSolver}

\alias{solveShortExact}
\alias{solveRquadprog}
\alias{solveRdonlp2} 
\alias{solveRlpSolve}


\title{Portfolio Solver}


\description{

    A collection and description of solver and utility 
    functions for portfolio optimization.
    \cr

    The functions are:

    \tabular{ll}{
    \code{solveRQuadprog} \tab Calls Goldfarb and Idnani's QP solver, \cr
    \code{solveRDonlp2} \tab Calls Spelucci's donlp2 solver, \cr
    \code{solveRlpSolve} \tab Calls lp_solve linear/integer programming system, \cr
    \code{setSolver} \tab Sets the desired solver. }

}


\usage{
solveShortExact(data, spec, constraints)
solveRquadprog(data, spec, constraints)
solveRdonlp2(data, spec, constraints)
solveRlpSolve(data, spec, constraints)
}



\arguments{

    \item{data}{
        [portfolioStatistics] - \cr
        a time series or a named list, containing either a series of returns 
        or named entries 'mu' and 'Sigma' being mean and covariance matrix.
        }
    \item{spec}{
        an S4 object of class \code{fPFOLIOSPEC}, containing slots call, model,
        portfolio, title, description, see \code{\link{PortfolioSpec}}
        for a full slot description.
        }
    \item{constraints}{
        a character string vector, containing the constraints of the form\cr
        \code{"minW[asset]=percentage"} for box constraints resp. \cr
        \code{"maxsumW[assets]=percentage} for sector constraints.
        }
}

%\references{}


\seealso{

   \code{\link{portfolioData}},
   \code{\link{portfolioSpec}},
   \code{\link{portfolioConstraints}},
   \code{\link{fPORTFOLIO}}.

}


\author{

    Diethelm Wuertz and Oliver Greshake for the Rmetrics port.

}


\examples{
## Solver Rquadprog:
   # Load Data:
   Data = as.timeSeries(data(smallcap.ts))
   Data = Data[, c("BKE", "GG", "GYMB", "KRON")]
   Data
   
## Solve Rdonlp2:
   # ...
   
## Solve RlpSolve:
   # ...
}


\keyword{models}

