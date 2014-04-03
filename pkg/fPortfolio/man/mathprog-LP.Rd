\name{mathprog-LP}


\alias{rsolveLP} 

\alias{rglpkLP} 
\alias{glpkLP}
\alias{glpkLPControl}

\alias{rsymphonyLP}
\alias{symphonyLP} 
\alias{symphonyLPControl}

\alias{ramplLP} 
\alias{amplLP}
\alias{amplLPControl}

\alias{rneosLP}
\alias{neosLP}
\alias{neosLPControl} 


\title{Mathematical Linear Programming}


\description{

    Mathematical Linear Programming.

}


\usage{
rsolveLP(objective, lower=0, upper=1, linCons, 
    control=list(solver="glpk", invoke=c("R", "AMPL", "NEOS")))

rglpkLP(objective, lower=0, upper=1, linCons, control=list())
glpkLP
glpkLPControl(solver = "glpk", project="r", trace=FALSE) 

rsymphonyLP(objective, lower=0, upper=1, linCons, control=list())
symphonyLP
symphonyLPControl(solver="symphony", project="r", trace=FALSE) 

ramplLP(objective, lower = 0, upper = 1, linCons, control=list()) 
amplLP(objective, x_L=NULL, x_U=NULL, A=NULL, b_L=NULL, b_U=NULL,
    control=list())
amplLPControl(solver="ipopt", project="ampl", inf=1e12, trace=FALSE) 
    
rneosLP(objective, lower = 0, upper = 1, linCons, control=list()) 
neosLP(objective, x_L=NULL, x_U=NULL, A=NULL, b_L=NULL, b_U=NULL,
    control=list())
neosLPControl(solver="ipopt", category="lp", project="neos", 
    inf=1e12, trace=FALSE) 
}


\arguments{

    \item{objective}{
        a numeric vector.
        }
    \item{lower, upper}{
        lower and upper bounds.
        }
    \item{linCons}{
        list of linear constraints: mat, lower, upper.
        }
    \item{control}{
        control list.
        }
        
    \item{x_L, x_U}{
        lower and upper box bounds.
        }
    \item{A}{
        linear constraints matrix.
        }
    \item{b_L, b_U}{
        lower and upper linear constraints bounds.
        }
        
    \item{solver}{
        a character string, the solver name.
        }
    \item{category}{
        a character string, the NEOS category name.
        }
    \item{project}{
        a character string, the AMPL project name.
        }
    \item{inf}{
        a numeric value, the maximum value used for bounds.
        }
    \item{trace}{
        a logical flag, if TRUE the optimization will be traced.
        }
    
}


\value{
    
    a list of class \code{solver} with the following named ebtries: 
    \code{opt}, 
    \code{solution},
    \code{objective},
    \code{status},
    \code{message}, 
    \code{solver},
    \code{version}.

}


\references{

Wuertz, D., Chalabi, Y., Chen W., Ellis A. (2009);
    \emph{Portfolio Optimization with R/Rmetrics}, 
    Rmetrics eBook, Rmetrics Association and Finance Online, Zurich.
    
}


\keyword{models}

