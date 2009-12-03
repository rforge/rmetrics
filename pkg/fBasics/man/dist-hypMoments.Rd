\name{hypMoments}

\alias{hypMoments}

\alias{hypMean}
\alias{hypVar}
\alias{hypSkew}
\alias{hypKurt}
\alias{hypMoments}


\title{Hyperbolic Distribution Moments}


\description{
    
    Calculates moments of the hyperbbolic distribution 
    function
    
}


\usage{
hypMean(alpha=1, beta=0, delta=1, mu=0)
hypVar(alpha=1, beta=0, delta=1, mu=0)
hypSkew(alpha=1, beta=0, delta=1, mu=0)
hypKurt(alpha=1, beta=0, delta=1, mu=0)

hypMoments(order, type = c("raw", "central", "mu"),
    alpha=1, beta=0, delta=1, mu=0)
}


\arguments{

    \item{alpha, beta, delta, mu}{
        numeric values.
        \code{alpha} is the first shape parameter;
        \code{beta} is the second shape parameter in the range \code{(0, alpha)};
        \code{delta} is the scale parameter, must be zero or positive; 
        \code{mu} is the location parameter, by default 0.
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
## hypMean -
   hypMean(alpha=1.1, beta=0.1, delta=0.8, mu=-0.3)
   
## ghKurt -
   hypKurt(alpha=1.1, beta=0.1, delta=0.8, mu=-0.3)
   
## hypMoments -
   hypMoments(4, alpha=1.1, beta=0.1, delta=0.8, mu=-0.3)
   hypMoments(4, "central", alpha=1.1, beta=0.1, delta=0.8, mu=-0.3)
}


\keyword{distribution}

