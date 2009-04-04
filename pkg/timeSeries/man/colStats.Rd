\name{colStats}


\alias{colStats} 
\alias{colSums,timeSeries-method}
\alias{colMeans,timeSeries-method}
\alias{colSds}
\alias{colVars}
\alias{colSkewness}
\alias{colKurtosis}        
\alias{colMaxs} 
\alias{colMins}         
\alias{colProds} 
\alias{colQuantiles} 

\alias{colAvgs}           
\alias{colStdevs}

% \alias{mean.timeSeries}           
% \alias{var.timeSeries}


\title{Column Statistics}


\description{

    A collection and description of functions to  
    compute column statistical properties of 
    financial and economic time series data. 
    \cr
    
    The functions are:
    
    \tabular{ll}{  
    \code{colStats}\tab calculates column statistics, \cr
    
    \code{colSums} \tab calculates column sums, \cr
    \code{colMeans} \tab calculates column means, \cr
    \code{colSds} \tab calculates column standard deviations, \cr
    \code{colVars} \tab calculates column variances, \cr 
    \code{colSkewness} \tab calculates column skewness, \cr
    \code{colKurtosis} \tab calculates column kurtosis, \cr
    \code{colMaxs} \tab calculates maximum values in each column, \cr
    \code{colMins} \tab calculates minimum values in each column, \cr
    \code{colProds} \tab computes product of all values in each column, \cr
    \code{colQuantiles} \tab computes quantiles of each column. }

}


\usage{
colStats(x, FUN, \dots) 

colSds(x, \dots)
colVars(x, \dots)
colSkewness(x, \dots)
colKurtosis(x, \dots)
colMaxs(x, \dots)
colMins(x, \dots)
colProds(x, \dots)
colQuantiles(x, prob = 0.05, \dots)

colStdevs(x, \dots)
colAvgs(x,  \dots)

% \method{mean}{timeSeries}(x, \dots)
% \method{var}{timeSeries}(x, \dots)

}


\arguments{
  
    \item{FUN}{
        a function name. The statistical function to be applied.
        }
    \item{prob}{
        a numeric value, the probability with value in [0,1].
        }
    \item{x}{
        a rectangular object which can be transformed into a matrix
        by the function \code{as.matrix}.  
        }
    \item{\dots}{
        arguments to be passed.
        }
        
}


\value{
    
    the functions return a numeric vector of the statistics.
}


\seealso{

    \code{link{rowStats}}.
    
}


\examples{
## Simulated Return Data in Matrix Form:
   x = matrix(rnorm(252), ncol = 2)
     
## colStats -
   colStats(x, FUN = mean)
   
## colQuantiles -
   colQuantiles(x, prob = 0.10, type = 1)  
}


\keyword{univar}

