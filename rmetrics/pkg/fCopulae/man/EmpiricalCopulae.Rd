\name{EmpiricalCopulae}

\alias{EmpiricalCopulae}

\alias{pempiricalCopula}
\alias{dempiricalCopula}


\title{Bivariate Empirical Copulae}


\description{
    
    A collection and description of functions to investigate 
    bivariate empirical copulae. 
    \cr
    
     
    Empirical Copulae Functions:
    
    \tabular{ll}{
    \code{pempiricalCopula} \tab computes empirical copula probability, \cr
    \code{dempiricalCopula} \tab computes empirical copula density. }
    
}


\usage{

pempiricalCopula(u, v, N = 10)
dempiricalCopula(u, v, N = 10) 

}


\arguments{
  
    \item{N}{
        [empiricalCopula] - \cr
        ... .
        }
    \item{u, v}{
        [*evCopula][*archmCopula] - \cr
        two numeric values or vectors of the same length at which
        the copula will be computed. If \code{u} is a list then the
        the \code{$x} and \code{$y} elements will be used as \code{u} 
        and \code{v}. If \code{u} is a two column matrix then the 
        first column will be used as \code{u} and the the second 
        as \code{v}.
       }
   
}


\value{
    
    Th functions \code{*Spec} return an S4 object 
    of class \code{"fCOPULA"}. The object contains the following slots:
    
    \item{@call}{
        the function call.  
        }
    \item{@copula}{
        the name of the copula.
        }
    \item{@param}{
        a list whose elements specify the model parameters.
        }
    \item{@title}{
        a character string with the name of the copula. This can be 
        overwritten specifying a user defined input argument.
        }
    \item{@description}{
        a character string with an optional user defined description. 
        By default just the current date when the test was applied will
        be returned.
        }
        
    The function \code{pcopula} returns a numeric matrix of probabilities 
    computed at grid positions \code{x}|\code{y}.
    \cr
    
    The function \code{parchmCopula} returns a numeric matrix with values
    computed for the Archemedean copula.
    \cr
    
    The function \code{darchmCopula} returns a numeric matrix with values
    computed for thedensity of the Archemedean copula.
    \cr
    
    The functions \code{Phi*} return a numeric vector with the values
    computed from the Archemedean generator, its derivatives, or its
    inverse.
    \cr
    
    The functions \code{cK} and {cKInv} return a numeric vector with the 
    values of the density and inverse for Archimedian copulae.
        
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}

% \examples{
% ## fCOPULA -
%    # getClass("fCOPULA")
   
% ## pcopula -
%    # The default Normal Copula:
%    # contour(pcopula())
% }


\keyword{models}

