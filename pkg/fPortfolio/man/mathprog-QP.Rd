\name{mathprog-QP}


\alias{quadprogQP}
\alias{quadprogControl}

\alias{ipopQP}
\alias{ipopControl}


\title{Quadratic Programming Solvers}


\description{

    Optimizes a portfolio with quadratic programming solvers.

}


\usage{
quadprogQP(
    objective = list(dvec=NULL, Dmat=NULL), 
    par.lower = NULL, par.upper = NULL, 
    eqA = NULL, eqA.bound = NULL,  
    ineqA = NULL, ineqA.lower = NULL, ineqA.upper = NULL,
    control = list())

quadprogControl(solver = "quadprog", trace = FALSE)


ipopQP(objective = list(dvec = NULL, Dmat  =  NULL), 
    par.lower = NULL, par.upper = NULL, 
    eqA = NULL, eqA.bound = NULL,
    ineqA = NULL, ineqA.lower = NULL, ineqA.upper = NULL,
    control = list())

ipopControl(sigf = 7, maxiter = 40, margin = 0.05, bound = 10, 
    verb = 0, trace = FALSE)
}


\arguments{

    \item{objective}{
        objective function parameter.
        }
    \item{par.lower, par.upper}{
        lower and upper bounds.
        }
    \item{eqA, eqA.bound}{
        linear equality constraints.
        }
    \item{ineqA, ineqA.lower, ineqA.upper}{
        linear inequality constraints.
        }
    \item{control}{
        control parameter list.
        }
    \item{solver, sigf, maxiter, margin, bound, verb, trace}{
        control parameter aaguments.
    }
    
}


\value{
    
    a list with named ebtries: 

}


\references{

Wuertz, D., Chalabi, Y., Chen W., Ellis A. (2009);
    \emph{Portfolio Optimization with R/Rmetrics}, 
    Rmetrics eBook, Rmetrics Association and Finance Online, Zurich.
    
}


%\examples{
%}


\keyword{models}

