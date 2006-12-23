\name{SkewnessKurtosis}

\alias{SkewnessKurtosis}

\alias{skewness}
\alias{skewness.default}
\alias{skewness.data.frame}
\alias{skewness.POSIXct}
\alias{skewness.POSIXlt}

\alias{kurtosis}
\alias{kurtosis.default}
\alias{kurtosis.data.frame}
\alias{kurtosis.POSIXct}
\alias{kurtosis.POSIXlt}


\title{Skdewness and Kurtosis}


\description{

    A collection and description of functions to compute 
    skewness and kurtosis.
    \cr
    
    The functions are:
    
    \tabular{ll}{
    \code{skewness} \tab returns value of skewness, \cr
    \code{kurtosis} \tab returns value of kurtosis. }

}


\usage{
skewness(x, \dots)
\method{skewness}{default}(x, na.rm = FALSE, method = c("moment", "fisher"), \dots)
\method{skewness}{data.frame}(x, \dots)
\method{skewness}{POSIXct}(x, \dots)
\method{skewness}{POSIXlt}(x, \dots)

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
        [skewness][kurtosis] - \cr
        a character string which specifies the method of computation. 
        These are either \code{"moment"} or \code{"fisher"}, kurtosis 
        allows in addition for \code{"excess"}. If \code{"excess"} is 
        selected, then the value of the kurtosis is computed  by the
        \code{"moment"} method and a value of 3 will be subtracted.
        The \code{"moment"} method is based on the definitions of 
        skewness and kurtosis for distributions; these forms should 
        be used when resampling (bootstrap or jackknife). The 
        \code{"fisher"} method correspond to the usual "unbiased" 
        definition of sample variance, although in the case of skewness 
        and kurtosis exact unbiasedness is not possible. 
        }
    \item{x}{
        a numeric vector.
        \cr
        [basicStats] - \cr
        allows also a matrix, data.frame or timeSeries object as input.
        In this case only the first column of data will be considered
        and a a warning will be printed.        
        }
    \item{\dots}{
        arguments to be passed.
        }
        
}


\value{
    
    \code{skewness}\cr
    \code{kurtosis}
    \cr
    return the value of the statistics, a numeric value. An 
    attribute which reports the used method is added.
    
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## SOURCE("fEcofin.3B-SkewnessKurtosis")
     
## mean -
## var -
   # Mean, Variance:
   mean(tS)
   var(tS)
   
## skewness -
## kurtosis -
   # Skewness, Kurtosis:
   class(tS)
   skewness(tS)
   kurtosis(tS)   
}


\keyword{univar}

