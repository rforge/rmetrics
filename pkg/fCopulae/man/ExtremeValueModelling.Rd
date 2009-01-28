\name{ExtremeValueModelling}

\alias{ExtremeValueModelling}

\alias{evCopulaSim}
\alias{evCopulaFit}


\title{Bivariate Extreme Value Copulae}


\description{
    
    A collection and description of functions to investigate 
    bivariate extreme value copulae. 
    \cr
    
     
    Extreme Value Copulae Functions:
    
    \tabular{ll}{ 
    \code{evCopulaSim} \tab simulates an extreme value copula, \cr
    \code{evCopulaFit} \tab fits the parameters of an extreme value copula. }
    
}


\usage{
evCopulaSim(n, param = NULL,  type = evList())
evCopulaFit(u, v = NULL, type = evList(), \dots)
}


\arguments{
  
    \item{n}{
        [revCopula][evCopulaSim] - \cr         
        the number of random deviates to be generated, an integer value.
        }
    \item{param}{
        [*ev*][A*] - \cr
        distribution and copulae parameters.
        A numeric value or vector of named parameters as required by 
        the copula specified by the variable \code{type}.
        If set to \code{NULL}, then the default parameters will be taken.
        }
    \item{type}{
        [*ev*][Afunc] - \cr
        the type of the extreme value copula. A character
        string selected from: "gumbel", "galambos", "husler.reiss", 
        "tawn", or "bb5".
        \cr
        [evSlider] - \cr
        a character string specifying the plot type. Either a 
        perspective plot which is the default or a contour plot 
        with an underlying image plot will be created.
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
    \item{\dots}{
        [evCopulaFit] - \cr
        arguments passed to the optimization function \code{nlminb}.
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
    
    The functions \code{cK} and \code{cKInv} return a numeric vector with the 
    values of the density and inverse for Archimedian copulae.
        
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## fCOPULA -
   getClass("fCOPULA")
   getSlots("fCOPULA")
   
## revCopula -
   # Not yet implemented
   # revCopula(n = 10, type = "galambos")
   
## pevCopula -
   pevCopula(u = grid2d(), type = "galambos", output = "list")
   
## devCopula -
   devCopula(u = grid2d(), type = "galambos", output = "list")
   
## AfuncSlider -
   # Generator, try:
   if (require(tcltk)) {
     AfuncSlider()
   }
}


\keyword{models}

