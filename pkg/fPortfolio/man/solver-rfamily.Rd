\name{solver-family}


\alias{solveRglpk.CVAR}
\alias{solveRglpk.MAD}
\alias{solveRampl.CVAR}
\alias{solveRshortExact}
\alias{solveRquadprog}
\alias{solveRquadprog.CLA}
\alias{solveRipop}
\alias{solveRampl.MV}
\alias{solveRsocp}
\alias{solveRdonlp2}
\alias{solveRsolnp}


\title{LP, QP, and NLP Programming Solvers}


\description{

    Rmetrics solver interface.

}


\usage{
solveRglpk.CVAR(data, spec, constraints)
solveRglpk.MAD(data, spec, constraints)
solveRampl.CVAR(data, spec, constraints)

solveRshortExact(data, spec, constraints)
solveRquadprog(data, spec, constraints)
solveRquadprog.CLA(data, spec, constraints)
solveRipop(data, spec, constraints)
solveRampl.MV(data, spec, constraints)

solveRsocp(data, spec, constraints)

solveRdonlp2(data, spec, constraints)
solveRsolnp(data, spec, constraints)
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


\keyword{models}

