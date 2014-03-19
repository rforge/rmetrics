\name{mvdist-mscFit}


\alias{mscFit}


\title{Multivariate Skew Cauchy Parameter Estimation}


\description{
    
    Fitting the parameters for the Multivariate Skew Cauchy
    Distribution.
    
}


\usage{
    mscFit(x, dp = NULL, trace=FALSE, title = NULL, description = NULL, ...)
}


\arguments{

    \item{x}{
        a matrix with "d" columns, giving the coordinates of the 
        point(s) where the density must be evaluated.
        }
    \item{dp}{
        a list with three elements, corresponding to \code{xi}, 
        \code{Omega} and \code{alpha}. 
        \code{xi} is a numeric vector of length "d" representing 
        the location parameter of the distribution,
        \code{Omega} is a symmetric positive-definite matrix of 
        dimension "d" timesd "d",
        \code{alpha} is a numeric vector which regulates the 
        the slant of the density.
        }
    \item{trace}{
        a logical value, should the estimation be traced? By default
        FALSE.
        }
    \item{title}{
        an optional project title.
        }
    \item{description}{
        an option project desctiption. 
        }
    \item{...}{
        optional parameters passed to the underlying "sn" functions.
    }

}


\examples{
## mscFit - 
   # Fit Example:
   N <- 1000
   xi <- c(0, 0) 
   Omega <- diag(2); Omega[2,1] <- Omega[1,2] <- 0.5
   alpha <- c(2, -6)
   X <- rmsc(n=N, xi, Omega, alpha)
   ans <- mscFit(X)
   ans
}


\keyword{math}

