\name{mathprog-NLP}


\alias{solnpNLP}
\alias{solnpControl}


\title{Quadratic Programming Solver}


\description{

    Optimizes a portfolio with a non-linear programming.

}


\usage{
solnpNLP(start, fun, 
    par.lower = NULL, par.upper = NULL,
    eqA = NULL, eqA.bound = NULL,
    ineqA = NULL, ineqA.lower = NULL, ineqA.upper = NULL,
    eqFun = list(), eqFun.bound = NULL,
    ineqFun = list(), ineqFun.lower = NULL, ineqFun.upper = NULL,
    control = list())

solnpControl(rho = 1, outer.iter = 400, inner.iter = 800, 
    delta = 1.0e-7, tol = 1.0e-8, trace = 0)
}


\arguments{

    \item{start}{
        start values.
        }
    \item{fun}{
        objective function.
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
    \item{eqFun, eqFun.bound}{
        non-linear equality constraints.
        }
    \item{ineqFun, ineqFun.lower, ineqFun.upper}{
        non-linear inequality constraints.
        }
    \item{control}{
        control parameter list.
        }
    \item{rho, outer.iter, inner.iter, delta, tol, trace}{
        control parameter arguments.
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

