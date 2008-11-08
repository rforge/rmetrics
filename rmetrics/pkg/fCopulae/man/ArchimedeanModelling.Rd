\name{ArchimedeanModelling}

\alias{ArchimedeanModelling}


\alias{archmCopulaSim}
\alias{archmCopulaFit}

\title{Bivariate Archimedean Copulae}


\description{
    
    A collection and description of functions to 
    investigate bivariate Archimedean copulae. 
    \cr
    
    Archimedean Copulae Functions:
    
    \tabular{ll}{   
    \code{archmCopulaSim} \tab simulates an Archimedean copula, \cr
    \code{archmCopulaFit} \tab fits the parameters of an Archimedean copula. }
    
}


\usage{
archmCopulaSim(n, alpha = NULL, type = archmList())
archmCopulaFit(u, v = NULL, type = archmList(), \dots)
}


\arguments{
  
    \item{alpha}{
        [Phi*][*archmCopula] - \cr
        the parameter of the Archemedean copula. A numerical value.
        }
    \item{n}{
        [rarchmCopula] - \cr         
        the number of random deviates to be generated, an integer value.
        }
    \item{type}{
        the type of the Archimedean copula. A character string ranging 
        beween \code{"1"} and \code{"22"}. By default copula No. 1 will 
        be chosen.
        }
    \item{u, v}{
        [*archmCopula] - \cr
        two numeric values or vectors of the same length at which
        the copula will be computed. If \code{u} is a list then the
        the \code{\$x} and \code{\$y} elements will be used as \code{u} 
        and \code{v}. If \code{u} is a two column matrix then the 
        first column will be used as \code{u} and the the second 
        as \code{v}.
       }
    \item{\dots}{
        [archmCopulaFit] - \cr
        arguments passed to the optimization function in use, \code{nlminb}.
        }
    
}


\value{
      
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

