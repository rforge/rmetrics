\name{mathprog-QP}


\alias{rsolveQP}
 
\alias{rquadprogQP} 
\alias{quadprogQP}
\alias{quadprogQPControl}
\alias{rquadprog}

\alias{ripopQP} 
\alias{ipopQP}
\alias{ipopQPControl}

\alias{ramplQP} 
\alias{amplQP}
\alias{amplQPControl}

\alias{rkestrelQP} 
\alias{kestrelQP} 

\alias{rneosQP}
\alias{neosQP}



\alias{kestrelQPControl}
\alias{neosQPControl}


\title{Mathematical Linear Programming}


\description{

    Mathematical Quadratic Programming.

}


\usage{
rsolveQP(objective, lower=0, upper=1, linCons, 
    control=list(solver="quadprog", invoke=c("R", "AMPL", "NEOS")))

rquadprogQP(objective, lower=0, upper=1, linCons, control=list())
quadprogQP(objective=list(dvec=NULL, Dmat=NULL), 
    par.lower=NULL, par.upper=NULL, 
    eqA=NULL, eqA.bound=NULL,  
    ineqA=NULL, ineqA.lower=NULL, ineqA.upper=NULL,
    control=list())
quadprogQPControl(solver="quadprog", trace=FALSE)
rquadprog

ripopQP(objective, lower=0, upper=1, linCons, control=list())
ipopQP(objective=list(dvec=NULL, Dmat = NULL), 
    par.lower=NULL, par.upper=NULL, 
    eqA=NULL, eqA.bound=NULL,
    ineqA=NULL, ineqA.lower=NULL, ineqA.upper=NULL,
    control=list())
ipopQPControl(
    sigf=12, maxiter=400, margin=0.05, bound=10, verb=0, 
    inf=1e12, solver="ipop", trace=FALSE) 
ripop
       
ramplQP(objective, lower=0, upper=1, linCons, control=list())
amplQP(objective=list(dvec=NULL, Dmat=NULL), 
    x_L=NULL, x_U=NULL, A=NULL, b_L=NULL, b_U=NULL,
    control=list(), \dots)
amplQPControl(solver="ipopt", project="ampl", 
    inf=1e12, trace = FALSE)
       
rkestrelQP(objective, lower=0, upper=1, linCons, control=list())
kestrelQP(objective=list(dvec=NULL, Dmat=NULL), 
    x_L=NULL, x_U=NULL, A=NULL, b_L=NULL, b_U=NULL,
    control=list(), \dots)
kestrelQPControl(solver="loqo", project="kestrel", 
    inf=1e12, trace = FALSE)
        
rneosQP(objective, lower=0, upper=1, linCons, control=list())
neosQP(objective=list(dvec=NULL, Dmat=NULL), 
    x_L=NULL, x_U=NULL, A=NULL, b_L=NULL, b_U=NULL,
    control=list(), \dots)    
neosQPControl(solver="ipopt", category="nco", project="neos", 
    inf=1e12, trace=FALSE) 
}



\arguments{

    \item{objective}{
        ...
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
    \item{\dots}{
        optional arguments to be passed.
        }
        
    \item{par.lower, par.upper}{...}
    \item{eqA}{...}
    \item{eqA.bound}{...}
    \item{ineqA}{...}
    \item{ineqA.lower,ineqA.upper}{...}
    
    \item{x_L,x_U}{...}
    \item{A}{...}
    \item{b_L,b_U}{...}
    
    \item{solver}{...}
    \item{category}{...}
    \item{project}{...}
    \item{inf}{...}
    \item{trace}{...}
    
    \item{sigf}{...}
    \item{maxiter}{...}
    \item{margin}{...}
    \item{bound}{...}
    \item{verb}{...}
    
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

