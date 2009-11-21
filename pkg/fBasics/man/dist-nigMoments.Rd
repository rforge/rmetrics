\name{nigMoments}

\alias{nigMoments}

\alias{nigMean}
\alias{nigVar}
\alias{nigSkew}
\alias{nigKurt}


\title{Moments for the Normal Inverse Gaussian}


\description{
    
    Computes the first four moments for the
    normal inverse Gaussian distribution. 
    
}


\usage{
nigMean(alpha = 1, beta = 0, delta = 1, mu = 0)
nigVar(alpha = 1, beta = 0, delta = 1, mu = 0)
nigSkew(alpha = 1, beta = 0, delta = 1, mu = 0)
nigKurt(alpha = 1, beta = 0, delta = 1, mu = 0)
}


\arguments{

    \item{alpha, beta, delta, mu}{
        are numeric values where
        \code{alpha} is the location parameter,
        \code{beta} is the location parameter,
        \code{delta} is the first shape parameter, and
        \code{mu} is the second shape parameter.
        }
    
}


\value{

    All values for the \code{*nig} functions are numeric vectors: 
    \code{d*} returns the density,
    \code{p*} returns the distribution function,
    \code{q*} returns the quantile function, and
    \code{r*} generates random deviates.
    
    All values have attributes named \code{"param"} listing
    the values of the distributional parameters.
    
}


%\references{
%    go here ...
%}


\author{

    Diethelm Wuertz.
    
}


\examples{   
## nigMean -
   # Median:
   nigMean(alpha = 1, beta = 0, delta = 1, mu = 0)
 
## nigVar - 
   # Inter-quartile Range:
   nigVar(alpha = 1, beta = 0, delta = 1, mu = 0)
 
## nigSKEW -  
   # Robust Skewness:
   nigSkew(alpha = 1, beta = 0, delta = 1, mu = 0)
   
## nigKurt -
   # Robust Kurtosis:
   nigKurt(alpha = 1, beta = 0, delta = 1, mu = 0)
}


\keyword{distribution}

