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

\alias{rowStats} 
\alias{rowAvgs}
\alias{rowVars}
\alias{rowStdevs}
\alias{rowSkewness}
\alias{rowKurtosis}
\alias{rowCumsums}

\alias{colStats} 
\alias{colAvgs}
\alias{colVars}
\alias{colStdevs}
\alias{colSkewness}
\alias{colKurtosis}
\alias{colCumsums}

\alias{stdev}

\title{Basic Statistics Summary}


\description{

    A collection and description of functions to compute 
    basic statistical properties. Missing functions in 
    R to calculate skewness and kurtosis are added, a 
    function which creates a summary statistics, and 
    functions to calculate column and row statistics.
    \cr
    
    The functions are:
    
    \tabular{ll}{
    \code{skewness} \tab returns value of skewness, \cr
    \code{kurtosis} \tab returns value of kurtosis, \cr 
    \code{basicStats} \tab computes an overview of basic statistical values, \cr    
    \code{rowStats}\tab calculates row statistics, \cr
    \code{colStats} \tab calculates column statistics, \cr
    \code{rowAvgs} \tab calculates row means, \cr
    \code{colAvgs} \tab calculates column means, \cr
    \code{rowVars} \tab calculates row variances, \cr
    \code{colVars} \tab calculates column variances, \cr
    \code{rowStdevs} \tab calculates row standard deviations, \cr
    \code{colStdevs} \tab calculates column standard deviations, \cr
    \code{rowSkewness} \tab calculates row skewness, \cr
    \code{colSkewness} \tab calculates column skewness, \cr
    \code{rowKurtosis} \tab calculates row kurtosis, \cr
    \code{colKurtosis} \tab calculates column kurtosis, \cr
    \code{rowCumsums} \tab calculates row cumulated Sums, \cr
    \code{colCumsums} \tab calculates column cumulated Sums. }
    
    For SPLUS Compatibility:
    
    \tabular{ll}{
    \code{stdev} \tab Returns the standard deviation of a vector or matrix. }
    
    
    
}


\usage{
stdev(x, na.rm = FALSE)

skewness(x, \dots)
\method{skewness}{default}(x, na.rm = FALSE, \dots)
\method{skewness}{data.frame}(x, \dots)
\method{skewness}{POSIXct}(x, \dots)
\method{skewness}{POSIXlt}(x, \dots)

kurtosis(x, \dots)
\method{kurtosis}{default}(x, na.rm = FALSE, \dots)
\method{kurtosis}{data.frame}(x, \dots)
\method{kurtosis}{POSIXct}(x, \dots)
\method{kurtosis}{POSIXlt}(x, \dots)

basicStats(x, ci = 0.95, column = 1)

rowStats(x, FUN, na.rm = FALSE, \dots) 
rowAvgs(x, na.rm = FALSE, \dots)
rowVars(x, na.rm = FALSE, \dots)
rowStdevs(x, na.rm = FALSE, \dots)
rowSkewness(x, na.rm = FALSE, \dots)
rowKurtosis(x, na.rm = FALSE, \dots)
rowCumsums(x, na.rm = FALSE, \dots)

colStats(x, FUN, na.rm = FALSE, \dots) 
colAvgs(x, na.rm = FALSE, \dots)
colVars(x, na.rm = FALSE, \dots)
colStdevs(x, na.rm = FALSE, \dots)
colSkewness(x, na.rm = FALSE, \dots)
colKurtosis(x, na.rm = FALSE, \dots)
colCumsums(x, na.rm = FALSE, \dots)
}


\arguments{
  
    \item{ci}{
        confidence interval, a numeric value, by default 0.95, 
        i.e. 95 percent.
        }
    \item{column}{
        [basicStats] - \cr
        which column should be selected from the input matrix,
        data frame or timeSeries object. By default an integer
        value set to 1.
        }
    \item{FUN}{
        the statistical function to be applied.
        }
        \item{na.rm}{
        a logical. Should missing values be removed?
        }
    \item{x}{
        a numeric vector, or a matrix for column statistics.
        \cr
        [basicStats] - \cr
        allows also a matrix, data.frame or timeSeries as input.
        In this case only the first column of data will be considered
        and a a warning will be printed.        
        }
    \item{\dots}{
        arguments to be passed.
        }
        
}


\value{
    
    \code{skewness}, \code{kurtosis}
    \cr
    returns the value of the statistics, a numeric value.
    \cr
    
    \code{basicsStats}
    \cr
    returns data frame with the following entries and row names:
    nobs, NAs, Minimum, Maximum , 1. Quartile, 3. Quartile,
    Mean, Median, Sum, SE Mean, LCL Mean, UCL Mean, Variance,
    Stdev, Skewness, Kurtosis.
    \cr
    
    \code{rowStats}, \code{rowAvgs}, \code{rowVars}, \code{rowStdevs},\cr
    \code{rowSkewness}, \code{rowKurtosis}, \code{rowCumsum}
    \cr 
    computes sample statistics by column. Missing values can be
    handled.
    \cr
    
    \code{colStats}, \code{colAvgs}, \code{colVars}, \code{colStdevs},\cr
    \code{colSkewness}, \code{colKurtosis}, \code{colCumsum}
    \cr 
    computes sample statistics by column. Missing values can be
    handled. 
    
}


\note{

    R's-base package contains a function \code{colMeans} with an
    additional argument \code{dim=1}. Therefore, the function used 
    here to compute column means (averages) is named \code{colAvgs}.
    \cr
    
    The function \code{stdev} computes the standard deviation for a
    vector or matrix and was introduced for SPlus compatibility.
    Under R use the function \code{sd}. 
    
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## SOURCE("fBasics.15A-BasicStatistics")

## basicStats -
   xmpBasics("\nStart: Basic Statistics of log-Returns > ")
   # Data NYSE Composite Index:
   data(nyseres)
   basicStats(nyseres)  
     
## mean -
## var -
## skewness -
## kurtosis -
   xmpBasics("\nNext: Moments, Skewness and Kurtosis > ")
   # Mean, Variance:
   mean(nyseres)
   var(nyseres)
   # Skewness, Kurtosis:
   class(nyseres)
   skewness(nyseres[, 1])
   kurtosis(nyseres[, 1])   
}


\keyword{univar}

