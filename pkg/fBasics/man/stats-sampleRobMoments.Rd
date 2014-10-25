\name{sampleLMoments}

\alias{sampleLMoments}
\alias{sampleLmoments}


\title{Sample L-Moments}


\description{
    
    Computes L-moments from an empirical sample data set.
    
}


\usage{
sampleLmoments(x, rmax=4)

}


\arguments{

    \item{x}{
        are numeric vector, the sample values.
        }
    \item{rmax}{
        an integer value, the number of L-moments to
        be returned.
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
   
## sampleLmoments -
   sampleLmoments(x)
}


\keyword{distribution}

