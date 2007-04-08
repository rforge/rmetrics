\name{ExtremeValueGenerator}

\alias{ExtremeValueGenerator}


\alias{evList}
\alias{evParam}
\alias{evRange}
\alias{evCheck}

\alias{Afunc}
\alias{AfuncSlider}


\title{Bivariate Extreme Value Copulae}


\description{
    
    A collection and description of functions 
    concerned with the generator function for 
    the extreme value copula and with functions
    for setting and checking the distributional
    parameters.
    \cr
    
    Functions:
    
    \tabular{ll}{
    \code{evList} \tab Returns list of implemented extreme value copulae, \cr
    \code{evParam} \tab sets default parameters for an extreme value copula, \cr
    \code{evRange} \tab returns the range of valid rho values, \cr
    \code{evCheck} \tab checks if rho is in the valid range, \cr

    \code{Afunc} \tab computes dependence function, \cr
    \code{AfuncSlider} \tab displays interactively dependence function. }
    
}


\usage{
evList()
evParam(type = evList())
evRange(type = evList())
evCheck(param, type = evList())

Afunc(x, param = NULL, type = evList())
AfuncSlider()
}


\arguments{

    \item{param}{
        distribution and copulae parameters. A numeric value or vector 
        of named parameters as required by the copula specified by the 
        variable \code{type}. If set to \code{NULL}, then the default 
        parameters will be taken.
        }
    \item{type}{
        the type of the extreme value copula. A character string selected 
        from: "gumbel", "galambos", "husler.reiss", "tawn", or "bb5".
        }
    \item{x}{
        a numeric value or vector ranging between zero and one.
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
   # AfuncSlider() 
}


\keyword{models}

