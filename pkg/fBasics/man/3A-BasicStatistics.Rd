\name{BasicStatistics}

\alias{BasicStatistics}

\alias{basicStats}

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

\alias{stdev}

\title{Basic Statistics Summary}


\description{

    A collection and description of functions to compute 
    basic statistical properties of financial and
    economic time series data. Missing functions in 
    R to calculate skewness and kurtosis, and a function
    which creates a summary statistics are added.
    \cr
    
    The functions are:
    
    \tabular{ll}{
    \code{skewness} \tab returns value of skewness, \cr
    \code{kurtosis} \tab returns value of kurtosis, \cr 
    \code{basicStats} \tab computes an overview of basic statistical values. }
    
    For SPLUS Compatibility:
    
    \tabular{ll}{
    \code{stdev} \tab Returns the standard deviation of a vector or matrix. }

}


\usage{
stdev(x, na.rm = FALSE)

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

basicStats(x, ci = 0.95)
}


\arguments{
  
    \item{ci}{
        confidence interval, a numeric value, by default 0.95, 
        i.e. 95 percent.
        }
    \item{na.rm}{
        a logical. Should missing values be removed?
        }
    \item{method}{
        [kurtosis][skewness] - \cr
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
    \cr
    
    \code{basicsStats}
    \cr
    returns data frame with the following entries and row names:
    nobs, NAs, Minimum, Maximum , 1. Quartile, 3. Quartile,
    Mean, Median, Sum, SE Mean, LCL Mean, UCL Mean, Variance,
    Stdev, Skewness, Kurtosis.
    
}


\note{

    The function \code{stdev} computes the standard deviation for a
    vector or matrix and was introduced for SPlus compatibility.
    Under R use the function \code{sd}. 
    
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## SOURCE("fBasics.3A-BasicStatistics")

## basicStats -
   # Simulated Monthly Return Data:
   tS = timeSeries(matrix(rnorm(12)), timeCalendar())
   # ... must be univariate:
   basicStats(tS)  
     
## mean -
## var -
## skewness -
## kurtosis -
   # Mean, Variance:
   mean(tS)
   var(tS)
   # Skewness, Kurtosis:
   class(tS)
   skewness(tS)
   kurtosis(tS)   
}


\keyword{univar}

