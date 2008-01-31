\name{kurtosis}


\alias{kurtosis}
\alias{kurtosis.default}
\alias{kurtosis.data.frame}
\alias{kurtosis.POSIXct}
\alias{kurtosis.POSIXlt}


\title{Kurtosis}


\description{

    Functions to compute kurtosis.

}


\usage{
kurtosis(x, \dots)

\method{kurtosis}{default}(x, na.rm = FALSE, method = c("excess", "moment", "fisher"), \dots)
\method{kurtosis}{data.frame}(x, \dots)
\method{kurtosis}{POSIXct}(x, \dots)
\method{kurtosis}{POSIXlt}(x, \dots)
}


\arguments{
  
    \item{na.rm}{
        a logical. Should missing values be removed?
        }
    \item{method}{
        a character string which specifies the method of computation. 
        These are either \code{"moment"}, \code{"fisher"}, or \code{"excess"}. 
        If \code{"excess"} is 
        selected, then the value of the kurtosis is computed by the
        \code{"moment"} method and a value of 3 will be subtracted.
        The \code{"moment"} method is based on the definitions of 
        kurtosis for distributions; these forms should 
        be used when resampling (bootstrap or jackknife). The 
        \code{"fisher"} method correspond to the usual "unbiased" 
        definition of sample variance, although in the case of
        kurtosis exact unbiasedness is not possible. 
        }
    \item{\dots}{
        arguments to be passed.
        }
        
}


\value{
    
    \code{kurtosis}
    \cr
    
    returns the value of the statistics, a numeric value. An 
    attribute which reports the used method is added.
    
}


\seealso {

    \code{link{skewness}.
    
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{   
## mean -
## var -
   # Mean, Variance:
   r = rnorm(100)
   mean(r)
   var(r)
   
## kurtosis - 
   kurtosis(r)   
}


\keyword{univar}

