\name{ghMoments}

\alias{ghMoments}

\alias{ghMean}
\alias{ghVar}
\alias{ghSkew}
\alias{ghKurt}
\alias{ghMoments}


\title{Generalized Hyperbolic Distribution Moments}


\description{
    
    Calculates moments of the generalized hyperbbolic
    distribution function
    
}


\usage{
ghMean(alpha=1, beta=0, delta=1, mu=0, lambda=-1/2)
ghVar(alpha=1, beta=0, delta=1, mu=0, lambda=-1/2)
ghSkew(alpha=1, beta=0, delta=1, mu=0, lambda=-1/2)
ghKurt(alpha=1, beta=0, delta=1, mu=0, lambda=-1/2)

ghMoments(order, type = c("raw", "central", "mu"),
    alpha = 1, beta=0, delta=1, mu=0, lambda=-1/2)
}


\arguments{

    \item{alpha, beta, delta, mu, lambda}{
        numeric values.
        \code{alpha} is the first shape parameter;
        \code{beta} is the second shape parameter in the range \code{(0, alpha)};
        \code{delta} is the scale parameter, must be zero or positive; 
        \code{mu} is the location parameter, by default 0; and
        \code{lambda} defines the sublclass, by default -1/2.
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
Scott, D. J., Wuertz, D. and Tran, T. T. (2008) 
    \emph{Moments of the Generalized Hyperbolic Distribution}. 
    Preprint. 
}


\author{

    Diethelm Wuertz.
    
}


\examples{   
## ghMean -
   ghMean(alpha=1.1, beta=0.1, delta=0.8, mu=-0.3, lambda=1)
   
## ghKurt -
   ghKurt(alpha=1.1, beta=0.1, delta=0.8, mu=-0.3, lambda=1)
   
## ghMoments -
   ghMoments(4, 
     alpha=1.1, beta=0.1, delta=0.8, mu=-0.3, lambda=1)
   ghMoments(4, "central",
     alpha=1.1, beta=0.1, delta=0.8, mu=-0.3, lambda=1)
}


\keyword{distribution}

