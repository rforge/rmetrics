\name{skewness}


\alias{skewness}
\alias{skewness.default}
\alias{skewness.data.frame}
\alias{skewness.POSIXct}
\alias{skewness.POSIXlt}


\title{Skewness}


\description{

    Functions to compute skewness.

}


\usage{
skewness(x, \dots)

\method{skewness}{default}(x, na.rm = FALSE, method = c("moment", "fisher"), \dots)
\method{skewness}{data.frame}(x, \dots)
\method{skewness}{POSIXct}(x, \dots)
\method{skewness}{POSIXlt}(x, \dots)
}


\arguments{
  
    \item{na.rm}{
        a logical. Should missing values be removed?
        }
    \item{method}{
        a character string which specifies the method of computation. 
        These are either \code{"moment"} or \code{"fisher"}
        The \code{"moment"} method is based on the definitions of 
        skewnessfor distributions; these forms should 
        be used when resampling (bootstrap or jackknife). The 
        \code{"fisher"} method correspond to the usual "unbiased" 
        definition of sample variance, although in the case of skewness 
        exact unbiasedness is not possible. 
        }
    \item{x}{
        a numeric vector or object.      
        }
    \item{\dots}{
        arguments to be passed.
        }
        
}


\value{
    
    \code{skewness}\cr
    \cr
    
    returns the value of the statistics, a numeric value. An 
    attribute which reports the used method is added.
    
}


\seealso{

    \code{link{kurtosis}}.
    
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
   
## skewness -
   skewness(r)  
}


\keyword{univar}

