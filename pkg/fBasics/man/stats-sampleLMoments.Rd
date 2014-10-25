\name{sampleRobMoments}

\alias{sampleRobMoments}

\alias{sampleMED}
\alias{sampleIQR}
\alias{sampleSKEW}
\alias{sampleKURT}


\title{Robust Moments for the GLD}


\description{
    
    Computes the first four robust moments for the
    Normal Inverse Gaussian Distribution. 
    
}


\usage{
sampleMED(x)
sampleIQR(x)
sampleSKEW(x)
sampleKURT(x)
}


\arguments{

    \item{x}{
        are numeric vector, the sample values.
        }
        
}


\value{

    All values for the \code{*sample} functions are numeric vectors: 
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
## Sample:
   x = rt(100, 4)
   
## sampleMED -
   # Median:
   sampleMED(x)
 
## sampleIQR - 
   # Inter-quartile Range:
   sampleIQR(x)
 
## sampleSKEW -  
   # Robust Skewness:
   sampleSKEW(x)
   
## sampleKURT -
   # Robust Kurtosis:
   sampleKURT(x)
}


\keyword{distribution}

