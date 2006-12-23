\name{BivariateBinning}

\alias{BivariateBinning}

\alias{squareBinning}
\alias{hexBinning}


\title{Square and Hexagonal Data Binning}


\description{
    
    A collection and description of functions which
    allow to interpolate bivariate irregular data sets
    \cr

    Bivariate Binning Functions:
    
    \tabular{ll}{
    \code{squareBinning} \tab does a square binning of data points, \cr
    \code{hexBinning} \tab does a hexagonal binning of data points}
    
}


\usage{ 
squareBinning(x, y = NULL, bins = 30)

hexBinning(x, y = NULL, bins = 30)
}


\arguments{
  
    \item{bins}{
        an integer specifying the number of bins.
        } 
    \item{x, y}{
        either two numeric vectors of equal length or if  \code{y} 
        is NULL, a list with entries \code{x}, \code{y}, 
        or named data frame with \codde{x} in the first and
        \code{y} in the second column.
        }
}


\value{
    
    A list with three entries, \code{x}, \code{y} and \code{z},
    specified by an oject of class \code{squareBinning} or 
    \code{hexBinning}.
    
    The returned value, can be directly used by the \code{persp()} 
    and \code{contour} 3D plotting functions.
    
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## SOURCE("fEcofin.5A-BivariateBinning)

## squareBinning -
   sB = squareBinning(x = rnorm(1000), y = rnorm(1000))
   plot(sB)
   
## hexBinning -
   hB = hexBinning(x = rnorm(1000), y = rnorm(1000))
   plot(hB)

}


\keyword{programming}

