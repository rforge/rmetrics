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

\alias{colStats} 
\alias{colSums.timeSeries}
\alias{colMeans.timeSeries}
\alias{colAvgs}
\alias{colVars}
\alias{colStdevs}
\alias{colSkewness}
\alias{colKurtosis}

\alias{rowSums}    
\alias{rowMeans}

\alias{colSums}    
\alias{colMeans}     
\alias{colSds}        
\alias{colMaxs}         
\alias{colProds}            


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
    \code{colSums} \tab computes sums of all values in each column, \cr
    \code{rowSums} \tab computes sums of all values in each row, \cr
    \code{colMeans} \tab computes sums of all values in each column, \cr
    \code{rowMeans} \tab computes sums of all values in each row, \cr
    \code{colSds} \tab computes standardard deviation of each column, \cr
    \code{colMaxs} \tab computes maximum values in each colum, \cr
    \code{colProds} \tab computes product of all values in each colum. }

}


\usage{
rowStats(x, FUN, na.rm = FALSE, \dots) 
rowSums(x, na.rm = FALSE, dims = 1)
rowMeans(x, na.rm = FALSE, dims = 1)
rowAvgs(x, na.rm = FALSE, \dots)
rowVars(x, na.rm = FALSE, \dots)
rowStdevs(x, na.rm = FALSE, \dots)
rowSkewness(x, na.rm = FALSE, \dots)
rowKurtosis(x, na.rm = FALSE, \dots)

colStats(x, FUN, na.rm = FALSE, \dots) 
colSums(x, na.rm = FALSE, dims = 1)
colMeans(x, na.rm = FALSE, dims = 1)
colAvgs(x, na.rm = FALSE, \dots)
colVars(x, na.rm = FALSE, \dots)
colStdevs(x, na.rm = FALSE, \dots)
colSkewness(x, na.rm = FALSE, \dots)
colKurtosis(x, na.rm = FALSE, \dots)

colSds(x)
colMaxs(x)
colProds(x)
}


\arguments{
  
    \item{dims}{
        Which dimensions are regarded as \code{rows} or \code{columns} to 
        sum over. For \code{row*}, the sum or mean is over dimensions 
        \code{dims+1}, \code{...}; for \code{col*} it is over dimensions 
        \code{1:dims}.
        }
    \item{FUN}{
        a function name. The statistical function to be applied.
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
    
    the functions return a numeric vector of the statistics.
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

