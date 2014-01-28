\name{mathprog-LP}


\alias{glpkLP}
\alias{glpkControl}

\alias{symphonyLP}
\alias{symphonyControl}


\title{Linear Programming Solvers}


\description{

    Optimizes a portfolio with a linear programming solver.

}


\usage{
glpkLP(obj, mat, dir, rhs, bounds = NULL, types = NULL, max = FALSE)
glpkControl(trace = FALSE) 

symphonyLP(obj, mat, dir, rhs, bounds=NULL, types=NULL, max = FALSE)
symphonyControl(trace = FALSE) 
}


\arguments{

    \item{obj, mat, dir, rhs, bounds, types, max}{
        solver arguments.
        }
    \item{trace}{
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

