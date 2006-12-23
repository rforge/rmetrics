\name{BivariateBinning}

\alias{BivariateBinning}

\alias{akimaInterp}
\alias{krigeInterp}


\title{Square and Hexagonal Data Binning}


\description{
    
    A collection and description of functions which
    allow to interpolate bivariate irregular data sets
    \cr

    Bivariate Binning Functions:
    
    \tabular{ll}{
    \code{squareBinning} \tab does a square binning of data points, \cr
    \code{squarePlot} \tab plots square binned data points, \cr
    \code{hexBinning} \tab does a hexagonal binning of data points, \cr
    \code{hexPlot} \tab Plots hexagonal binned data points.}
    
}


\usage{ 
squareBinning(x, y = NULL, bins = 30)

hexBinning(x, y = NULL, bins = 30)
}


\arguments{
  
    \item{x, y, z}{
        either three numeric vectors of equal length or if  \code{y} 
        and \code{z} are NULL, a list with entries \code{x}, \code{y}, 
        \code{z}, or named data frame with \codde{x} in the first, 
        \code{y} in the second, and \code{z} in the third column.
        }
    \item{gridPoints}{
        an integer value specifying the number of grid points in \code{x} 
        and \code{y} direction.
        } 
    \item{xo, yo}{
        a numeric vector of data points spanning the grid.
        }
    \item{extrap}{
        a logical, if \code{TRUE} then the data points are extrapolated.
        }
    \item{polDegree}{
        the polynomial krige degree, an integer ranging between 1 and 6.
        }
    
}


\value{
    
    A list with three entries, \code{x}, \code{y} and \code{z}. 
    
    The returned value, can be directly used by the \code{persp()} 
    and \code{contour} 3D plotting functions.
    
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## SOURCE("fEcofin.5A-BivariateInterpolation")

## akimaInterp -
   # Akima Interpolation:    
   set.seed(1953)
   x = runif(999) - 0.5
   y = runif(999) - 0.5
   z = cos(2*pi*(x^2+y^2))
   ans = akimaInterp(x, y, z, extrap = FALSE)
   persp(ans, theta = -40, phi = 30, col = "steelblue")
   
## krigeInterp -
   # Kriging:    
   ans = krigeInterp(x, y, z, extrap = FALSE)
   persp(ans, theta = -40, phi = 30, col = "steelblue")
}


\keyword{programming}

