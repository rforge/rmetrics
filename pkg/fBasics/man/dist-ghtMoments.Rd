\name{ghtMoments}

\alias{ghtMoments}

\alias{ghtMean}
\alias{ghtVar}
\alias{ghtSkew}
\alias{ghtKurt}
\alias{ghtMoments}


\title{Generalized Hyperbolic Student-t Moments}


\description{
    
    Calculates moments of the generalized hyperbbolic
    Student-t distribution function.
    
}


\usage{
ghtMean(beta=0.1, delta=1, mu=0, nu=10)
ghtVar(beta=0.1, delta=1, mu=0, nu=10)
ghtSkew(beta=0.1, delta=1, mu=0, nu=10)
ghtKurt(beta=0.1, delta=1, mu=0, nu=10)

ghtMoments(order, type = c("raw", "central", "mu"),
    beta=0.1, delta=1, mu=0, nu=10)
}


\arguments{

    \item{beta, delta, mu}{
        numeric values.
        \code{beta} is the skewness parameter in the range \code{(0, alpha)};
        \code{delta} is the scale parameter, must be zero or positive; 
        \code{mu} is the location parameter, by default 0.
        These are the parameters in the first parameterization.
        }
    \item{nu}{
        a numeric value, the number of degrees of freedom.
        Note, \code{alpha} takes the limit of \code{abs(beta)}, 
        and \code{lambda=-nu/2}.
        }
    \item{order}{
        an integer value, the order of the moment.
        }
    \item{type}{
        a character value, 
        \code{"raw"} returns the moments about zero,
        \code{"central"} returns the central moments about the mean, and
        \code{"mu"} returns the moments about the location parameter \code{mu}.
        }    
}


\value{

    a numerical value.
    
}


\references{
Scott, D.J., Wuertz, D. and Tran, T.T. (2008) 
    \emph{Moments of the Generalized Hyperbolic Distribution}. 
    Preprint. 
}


\author{

    Diethelm Wuertz.
    
}


\examples{   
## ghtMean -
   ghtMean(beta=0.2, delta=1.2, mu=-0.5, nu=4)
   
## ghtKurt -
   ghtKurt(beta=0.2, delta=1.2, mu=-0.5, nu=4)
   
## ghtMoments -
   ghtMoments(4, 
     beta=0.2, delta=1.2, mu=-0.5, nu=4)
   ghtMoments(4, "central",
     beta=0.2, delta=1.2, mu=-0.5, nu=4)
}


\keyword{distribution}

