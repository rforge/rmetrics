\name{CopulaeClass}

\alias{CopulaeClass}

\alias{fCOPULA}
\alias{fCOPULA-class}

\alias{show.fCOPULA}
\alias{show,fCOPULA-method}

\alias{pfrechetCopula}


\title{Bivariate Copulae Class}


\description{
    
    A collection and description of functions to specify the 
    copula class and to investigate bivariate Frechet copulae. 
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
    
    The print method \code{show} returns an S4 object of 
    class \code{"fCOPULA"}. The object contains the following slots:
    
    \item{@call}{
        the function call.  
        }
    \item{@copula}{
        the name of the copula.
        }
    \item{@param}{
        a list whose elements specify the model parameters of the
        copula.
        }
    \item{@title}{
        a character string with the name of the copula. This can be 
        overwritten specifying a user defined input argument.
        }
    \item{@description}{
        a character string with an optional user defined description. 
        By default just the current date will be returned.
        }
        
    The function \code{pfrechetCopula} returns a numeric vector of 
    probabilities. An attribute named \code{"control"} is added
    which returns the name of the Frechet copula.
    
}


\details{

    The function \code{pfrechetCopula} returns a numeric matrix of 
    probabilities computed at grid positions \code{u}|\code{v}. The
    arguments \code{u} and \code{v} are two single values or two 
    numeric vectors of the same length. If \code{v} is not specified
    then the same values are taken as for \code{u}. Alternatively,
    \code{u} may be given as a two column vector or as a list with
    two entries as vectors. The first column or entry is taken as
    \code{u} and the second as \code{v}. 

}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## SOURCE("fCopulae.061A-CopulaeClass")

## fCOPULA -
   # getClass("fCOPULA")
   
## pcopula -
   # The Frechet Copula - m:
   pfrechetCopula(0.5)
   pfrechetCopula(0.25, 0.75)
   pfrechetCopula(runif(5))
   pfrechetCopula(grid2d())
}


\keyword{models}

