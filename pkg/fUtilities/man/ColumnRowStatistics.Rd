\name{ColumnRowStatistics}

\alias{ColumnRowStatistics}

\alias{rowStats} 
\alias{rowSums.timeSeries}
\alias{rowMeans.timeSeries}
\alias{rowAvgs}
\alias{rowVars}
\alias{rowStdevs}
\alias{rowSkewness}
\alias{rowKurtosis}
\alias{rowCumsums}

\alias{colStats} 
\alias{colSums.timeSeries}
\alias{colMeans.timeSeries}
\alias{colAvgs}
\alias{colVars}
\alias{colStdevs}
\alias{colSkewness}
\alias{colKurtosis}
\alias{colCumsums}


\title{Column and Row Statistics}


\description{

    A collection and description of functions to compute 
    column and row statistical properties of financial and
    economic time series data. 
    \cr
    
    The functions are:
    
    \tabular{ll}{  
    \code{rowStats}\tab calculates row statistics, \cr
    \code{colStats} \tab calculates column statistics, \cr
    \code{rowAvgs|rowSums} \tab calculates row sums, \cr
    \code{colAvgs|colSums} \tab calculates column sums, \cr
    \code{rowAvgs|rowMeans} \tab calculates row means, \cr
    \code{colAvgs|colMeans} \tab calculates column means, \cr
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

}


\usage{
rowStats(x, FUN, na.rm = FALSE, \dots) 
\method{rowSums}{timeSeries}(x, na.rm = FALSE, \dots)
\method{rowMeans}{timeSeries}(x, na.rm = FALSE, \dots)
rowAvgs(x, na.rm = FALSE, \dots)
rowVars(x, na.rm = FALSE, \dots)
rowStdevs(x, na.rm = FALSE, \dots)
rowSkewness(x, na.rm = FALSE, \dots)
rowKurtosis(x, na.rm = FALSE, \dots)
rowCumsums(x, na.rm = FALSE, \dots)

colStats(x, FUN, na.rm = FALSE, \dots) 
\method{colSums}{timeSeries}(x, na.rm = FALSE, \dots)
\method{colMeans}{timeSeries}(x, na.rm = FALSE, \dots)
colAvgs(x, na.rm = FALSE, \dots)
colVars(x, na.rm = FALSE, \dots)
colStdevs(x, na.rm = FALSE, \dots)
colSkewness(x, na.rm = FALSE, \dots)
colKurtosis(x, na.rm = FALSE, \dots)
colCumsums(x, na.rm = FALSE, \dots)
}


\arguments{
  
    \item{FUN}{
        [colStats][rowStats - \cr
        the statistical function to be applied.
        }
    \item{na.rm}{
        a logical. Should missing values be removed?
        }
    \item{x}{
        a rectangular object which can be transformed into a matrix
        by the function \code{as.matrix}.
        Note, this allows also a data.frame or timeSeries object as input.   
        }
    \item{\dots}{
        arguments to be passed.
        }
        
}


\value{
    
    \code{rowStats}\cr
    \code{rowAvgs}\cr
    \code{rowVars}\cr
    \code{rowStdevs}\cr
    \code{rowSkewness}\cr
    \code{rowKurtosis}\cr
    \code{rowCumsum}
    \cr 
    compute sample statistics by column. Missing values can be
    handled.
    \cr
    
    \code{colStats}\cr
    \code{colAvgs}\cr
    \code{colVars}\cr
    \code{colStdevs},\cr
    \code{colSkewness}\cr
    \code{colKurtosis}\cr
    \code{colCumsum}
    \cr 
    compute sample statistics by column. Missing values can be
    handled. 
    
}


\note{

    R's-base package contains a function \code{colMeans} with an
    additional argument \code{dim=1}. Therefore, the function used 
    here to compute column means (averages) is named \code{colAvgs}.
    \cr
    
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## Simulated Monthly Return Data:
   x = matrix(rnorm(24), ncol = 2)
     
## colStats -
   colStats(x, FUN = mean)  
}


\keyword{univar}

