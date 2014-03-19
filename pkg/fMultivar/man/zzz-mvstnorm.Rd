\name{zzz-mvstnorm}


\alias{mvstnorm}

\alias{dmvsnorm}
\alias{pmvsnorm}
\alias{rmvsnorm}

\alias{dmvst}
\alias{pmvst}
\alias{rmvst}

\alias{mvFit}

\title{Obsolete Functions}


\description{
    
    Obsolete Functions: Alternative multivariate distribution and 
    parameter estimation functions for the skew normal and skew
    Student-t distribution functions. 
    
}


\details{

  The former implementations have been replaced by wrpper functions
  calling functions from the package \code{"sn"}.
}


\usage{
  dmvsnorm(x, dim=2, mu=rep(0, dim), Omega=diag(dim), alpha=rep(0, dim))
  pmvsnorm(q, dim=2, mu=rep(0, dim), Omega=diag(dim), alpha=rep(0, dim))
  rmvsnorm(n, dim=2, mu=rep(0, dim), Omega=diag(dim), alpha=rep(0, dim))

  dmvst(x, dim=2, mu=rep(0, dim), Omega=diag(dim), alpha=rep(0, dim), df=4)
  pmvst(q, dim=2, mu=rep(0, dim), Omega=diag(dim), alpha=rep(0, dim), df=4)
  rmvst(n, dim=2, mu=rep(0, dim), Omega=diag(dim), alpha=rep(0, dim), df=4)   

  mvFit(x, method = c("snorm", "st"), fixed.df = NA, 
      title = NULL, description = NULL, trace = FALSE, ...)
}


\arguments{

    \item{x, q}{
        the vector of quantiles, a matrix with "dim" columns.
        }
    \item{n}{
        the number of desired observations.
        }
    \item{dim}{
        the dimension, by default the bivariate case is considered
        where \code{dim=2}
        }
    \item{mu, Omega, alpha, df}{
        \code{mu} is a numeric vector of length "dim" representing 
        the location parameter of the distribution,
        \code{Omega} is a symmetric positive-definite matrix of dimension 
        "d" timesd "d",
        \code{alpha} is a numeric vector which regulates the 
        the slant of the density,
        \code{df} a positive value representing the degrees of freedom.
        }
    \item{method}{
        selects the type of distribution function, either \code{"snorm"}
        which is the default, or \code{"st"}. 
        }
    \item{fixed.df}{
        set to a positive value to keep fixed the parameter \code{nu}
        of the skew student-t distribution in the optimization process; 
        with default value NULL, i.e. \code{nu} is estimated like the 
        other parameters.
        }   
    \item{title}{
        an optional project title.
        }
    \item{description}{
        an option project desctiption. 
        }
    \item{trace}{
        a logical, should the estimation be traced?
        }
    \item{...}{
        arguments passed to the underlying "sn" density functions.
        }
        
}


\value{

  \code{dm*} gives the density, \code{pm*} gives the distribution 
  function, and \code{rm*} generates \code{n} random deviates of
  dimension \code{dim}
  
  \code{mvFit} returns an object of class \code{fDISTFEED}, see
  package \code{fBasics}.

}


%\references{}



%\author{}


\examples{
## [dr]mvsnorm - 
   dmvsnorm(rnorm2d(100))
   rmvsnorm(100)

## [dr]mvst - 
   dmvst(rt2d(100))
   rmvst(100)
}


\keyword{math}

