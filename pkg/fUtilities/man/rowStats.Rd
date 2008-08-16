\name{rowStats}


\alias{rowStats} 
\alias{rowSums}
\alias{rowMeans}
\alias{rowSds}
\alias{rowVars}
\alias{rowSkewness}
\alias{rowKurtosis}        
\alias{rowMaxs} 
\alias{rowMins}         
\alias{rowProds} 
\alias{rowQuantiles} 

\alias{rowAvgs}           
\alias{rowStdevs}


\title{Row Statistics}


\description{

    Functions to compute row statistical properties 
    of financial and economic time series data. 
    \cr
    
    The functions are:
    
    \tabular{ll}{  
    \code{rowStats}\tab calculates row statistics, \cr
    \code{rowSums} \tab calculates row sums, \cr
    \code{rowMeans} \tab calculates row means, \cr
    \code{rowSds} \tab calculates row standard deviations, \cr
    \code{rowVars} \tab calculates row variances, \cr 
    \code{rowSkewness} \tab calculates row skewness, \cr
    \code{rowKurtosis} \tab calculates row kurtosis, \cr
    \code{rowMaxs} \tab calculates maximum values in each row, \cr
    \code{rowMins} \tab calculates minimum values in each row, \cr
    \code{rowProds} \tab computes product of all values in each row, \cr
    \code{rowQuantiles} \tab computes quantiles of each row. }

}


\usage{
rowStats(x, FUN, \dots) 

rowSums(x, \dots)
rowMeans(x, \dots)

rowSds(x, \dots)
rowVars(x, \dots)
rowSkewness(x, \dots)
rowKurtosis(x, \dots)
rowMaxs(x, \dots)
rowMins(x, \dots)
rowProds(x, \dots)
rowQuantiles(x, prob = 0.05, \dots)

rowStdevs(x, \dots)
rowAvgs(x, \dots)
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

    \code{link{colStats}}.
    
}



\examples{
## Simulated Return Data in Matrix Form:
   x = matrix(rnorm(10*10), nrow = 10)
     
## rowStats -
   rowStats(x, FUN = mean)
   
## rowMaxs -
   rowMaxs(x)  
}


\keyword{univar}

