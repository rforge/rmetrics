\name{BivariateBinning}

\alias{BivariateBinning}

\alias{squareBinning}
\alias{hexBinning}
\alias{plot.squareBinning}
\alias{plot.hexBinning}

\title{Square and Hexagonal Data Binning}


\description{
    
    A collection and description of functions which
    allow to create histograms due to sqaure and
    hexagonal binning.
    \cr

    Bivariate Binning Functions:
    
    \tabular{ll}{
    \code{squareBinning} \tab does a square binning of data points, \cr
    \code{hexBinning} \tab does a hexagonal binning of data points}
    
}


\usage{ 
squareBinning(x, y = NULL, bins = 30)
hexBinning(x, y = NULL, bins = 30)

\method{plot}{squareBinning}(x, col = heat.colors(12), addPoints = TRUE, 
    addRug = TRUE, \dots)
\method{plot}{hexBinning}(x, col = heat.colors(12), addPoints = TRUE, 
    addRug = TRUE, \dots)
}

\arguments{
  
    \item{addPoints}{
        a logical flag, should the center of mass points added to the
        plot?
        }
    \item{addRug}{
        a logical flag, should a rug representation be added to the
        plot, for details see the function \code{rug}.
        }
    \item{bins}{
        an integer specifying the number of bins.
        } 
    \item{col}{
        color map like for the \code{image} function.
        }
    \item{x, y}{
        [squareBinning][hexBinning] - \cr
        either two numeric vectors of equal length or if  \code{y} 
        is NULL, a list with entries \code{x}, \code{y}, 
        or named data frame with \code{x} in the first and
        \code{y} in the second column.\cr
        [plot] - \cr
        an object of class \code{squareBinning} or \code{hexBinning}.
        }
    \item{\dots}{
        arguments to be passed.
        }
}


\value{
    
    A list with three entries, \code{x}, \code{y} and \code{z},
    specified by an oject of class \code{squareBinning} or 
    \code{hexBinning}.
    
    Note, the returned value, can be directly used by the \code{persp()} 
    and \code{contour} 3D plotting functions.
    
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## squareBinning -
   sB = squareBinning(x = rnorm(1000), y = rnorm(1000))
   plot(sB)
   
## hexBinning -
   hB = hexBinning(x = rnorm(1000), y = rnorm(1000))
   plot(hB)
}


\keyword{programming}

