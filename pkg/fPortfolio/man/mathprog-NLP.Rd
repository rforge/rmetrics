\name{mathprog-NLP}


\alias{rdonlp2NLP} 
\alias{donlp2NLP} 
\alias{donlp2NLPControl} 
\alias{rdonlp2}

\alias{rsolnpNLP} 
\alias{solnpNLP} 
\alias{solnpNLPControl} 
\alias{rsolnp}

\alias{rnlminb2NLP} 
\alias{nlminb2NLP} 
\alias{nlminb2NLPControl} 
\alias{rnlminb2}

\alias{ramplNLP} 
\alias{amplNLP} 
\alias{amplNLPControl} 


\title{Mathematical Non-Linear Programming}


\description{

    Mathematical Non-Linear Programming.

}


\usage{
rdonlp2NLP(start, objective, 
    lower=0, upper=1, linCons, funCons, control=list())   
donlp2NLP(start, objective, 
    par.lower=NULL, par.upper=NULL,
    eqA=NULL, eqA.bound=NULL,
    ineqA=NULL, ineqA.lower=NULL, ineqA.upper=NULL,
    eqFun=list(), eqFun.bound=NULL,
    ineqFun=list(), ineqFun.lower=NULL, ineqFun.upper=NULL,
    control=list())    
donlp2NLPControl(
    iterma=4000, nstep=20, fnscale=1, report=FALSE, rep.freq=1, 
    tau0=1, tau=0.1, del0=1, epsx=1e-05, delmin=0.1 * del0, 
    epsdif=1e-08, nreset.multiplier=1, difftype=3, epsfcn=1e-16, 
    taubnd=1, hessian=FALSE, te0=TRUE, te1=FALSE, te2=FALSE, 
    te3=FALSE, silent=TRUE, intakt=TRUE) 
rdonlp2

rsolnpNLP(start, objective, 
    lower=0, upper=1, linCons, funCons, control=list())   
solnpNLP(start, objective, 
    par.lower=NULL, par.upper=NULL,
    eqA=NULL, eqA.bound=NULL,
    ineqA=NULL, ineqA.lower=NULL, ineqA.upper=NULL,
    eqFun=list(), eqFun.bound=NULL,
    ineqFun=list(), ineqFun.lower=NULL, ineqFun.upper=NULL,
    control=list())    
solnpNLPControl(
    rho=1, outer.iter=400, inner.iter=800, delta=1e-07, tol=1e-08, trace=0)
rsolnp
      
rnlminb2NLP(start, objective, 
    lower=0, upper=1, linCons, funCons, control=list())   
nlminb2NLP(start, objective, 
    par.lower=NULL, par.upper=NULL,
    eqA=NULL, eqA.bound=NULL,
    ineqA=NULL, ineqA.lower=NULL, ineqA.upper=NULL,
    eqFun=list(), eqFun.bound=NULL,
    ineqFun=list(), ineqFun.lower=NULL, ineqFun.upper=NULL,
    control=list())    
nlminb2NLPControl(
    eval.max=500, iter.max=400, trace=0, abs.tol=1e-20, rel.tol=1e-10, 
    x.tol=1.5e-08, step.min=2.2e-14, scale=1, R=1, beta.tol=1e-20)  
rnlminb2
      
ramplNLP(start, objective, 
    lower=0, upper=1, amplCons, control=list(), ...)   
amplNLP()    
amplNLPControl(
    solver="minos", project="ampl", trace=FALSE)
}


\arguments{

    \item{start}{
        a numeric vector, the start values.
        }
    \item{objective}{
        a function object, the function to be optimized.
        }
    \item{lower, upper}{
        lower and upper bounds.
        }
    \item{linCons}{
        list of linear constraints: mat, lower, upper.
        }
    \item{funCons}{
        list of function constraints.
        }
    \item{amplCons}{
        AMPL constraints.
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
    \item{eqFun}{...}
    \item{eqFun.bound}{...}
    \item{ineqFun}{...}
    \item{ineqFun.lower,ineqFun.upper}{...}
    
    %\item{x_L,x_U}{...}
    %\item{A}{...}
    %\item{b_L,b_U}{...}
    
    %\item{solver}{...}
    %\item{category}{...}
    %\item{project}{...}
    %\item{inf}{...}
    %\item{trace}{...}
    
    \item{iterma}{4000}
    \item{nstep}{20}
    \item{fnscale}{1}
    \item{report}{FALSE}
    \item{rep.freq}{1}
    \item{tau0}{1}
    \item{tau}{0.1}
    \item{del0}{1}
    \item{epsx}{1e-5}
    \item{delmin}{0.1 * del0}
    \item{epsdif}{1e-8}
    \item{nreset.multiplier}{1}
    \item{difftype}{3}
    \item{epsfcn}{1e-16}
    \item{taubnd}{1}
    \item{hessian}{FALSE}
    \item{te0}{TRUE}
    \item{te1}{FALSE}
    \item{te2}{FALSE}
    \item{te3}{FALSE}
    \item{silent}{TRUE}
    \item{intakt}{TRUE}
    
    \item{rho}{1}
    \item{outer.iter}{400}
    \item{inner.iter}{800}
    \item{delta}{1.0e-7}
    \item{tol}{1.0e-8}
    %\item{trace}{0}
    
    \item{eval.max}{500}
    \item{iter.max}{400}
    \item{trace}{0}
    \item{abs.tol}{1e-20}
    \item{rel.tol}{1e-10}
    \item{x.tol}{1.5e-08}
    \item{step.min}{2.2e-14}
    \item{scale}{1}
    \item{R}{1}
    \item{beta.tol}{1e-20}
    
    \item{solver}{solver name}
    \item{project}{project name}    
  
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

