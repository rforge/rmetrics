\name{CopulaeClass}

\alias{CopulaeClass}

\alias{fCOPULA}
\alias{fCOPULA-class}

\alias{show.fCOPULA}
\alias{show,fCOPULA-method}

\alias{pfrechetCopula}


\title{Bivariate Copulae Class}


\description{
    
    A collection and description of functions to investigate 
    bivariate copulae. 
    \cr
    
    The class representation and methods are:

    \tabular{ll}{
    \code{fCOPULA} \tab representation for an S4 object of class "fCOPULA", \cr
    \code{show} \tab S4 print method. }
       
    Frechet Copulae:
    
    \tabular{ll}{
    \code{pfrechetCopula} \tab computes Frechet copula probability. }
    
}


\usage{
show.fCOPULA(object)

pfrechetCopula(u = 0.5, v = u, type = c("m", "pi", "w"), 
    output = c("vector", "list"))
    
}


\arguments{
  
    \item{object}{
        [show] - \cr
        an S4 object of class \code{"fCOPULA"}.
        }
    \item{output}{
        [*frechetCopula] - \cr
        output - a character string specifying how the output should
        be formatted. By default a vector of the same length as \code{u} 
        and \code{v}. If specified as \code{"list"} then \code{u} and 
        \code{v} are expected to span a two-dimensional grid as outputted 
        by the function \code{grid2d} and the function returns a list with
        elements \code{$x}, \code{y}, and \code{z} which can be directly 
        used for example by 2D plotting functions.
        }
    \item{type}{
        [*frechetCopula] - \cr
        the type of the Frechet copula. A character
        string selected from: \code{"m"}, \code{"pi"}, or \code{"w"}.
        }
    \item{u, v}{
        two numeric values or vectors of the same length at which
        the copula will be computed. If \code{u} is a list then the
        the \code{$x} and \code{$y} elements will be used as \code{u} 
        and \code{v}. If \code{u} is a two column matrix then the 
        first column will be used as \code{u} and the the second 
        as \code{v}.
       }
    
}


\value{
    
    Th function \code{*Copula} returns an S4 object of 
    class \code{"fCOPULA"}. The object contains the following slots:
    
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
        By default just the current date will be returned.
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


\examples{
## SOURCE("fCopulae.E1-CopulaeClass")

## fCOPULA -
   # getClass("fCOPULA")
   
## pcopula -
   # The default Normal Copula:
   # contour(pcopula())
}


\keyword{models}

