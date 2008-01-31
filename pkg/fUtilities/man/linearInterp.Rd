\name{linearInterp}


\alias{linearInterp}
\alias{linearInterpp}


\title{Bivariate Linear Interpolation}


\description{
    
    Bivariate Linear Interpolation. Two options are
    available gridded and pointwise interpolation.
    
}
    

\usage{ 
linearInterp(x, y = NULL, z = NULL, gridPoints = 21,
    xo = seq(min(x), max(x), length = gridPoints),
    yo = seq(min(y), max(y), length = gridPoints))
    
linearInterpp(x, y = NULL, z = NULL, xo, yo)
}


\arguments{
  
    \item{x, y, z}{
        for \code{linearInterp} the arguments \code{x} and \code{y} are 
        two numeric vectors of grid pounts, and \code{z} is a numeric 
        matrix or any other rectangular object which can be transformed 
        by the function \code{as.matrix} into a matrix object.  
        For \code{linearInterpp} we  consider either three numeric vectors 
        of equal length or if  \code{y} and \code{z} are NULL, a list with 
        entries \code{x}, \code{y}, \code{z}, or named data frame with 
        \code{x} in the first, \code{y} in the second, and \code{z} in 
        the third column. 
        }
    \item{gridPoints}{
        an integer value specifying the number of grid points in \code{x} 
        and \code{y} direction.
        } 
    \item{xo, yo}{
        for \code{linearInterp}
        two numeric vectors of data points spanning the grid, and
        for \code{linearInterpp}
        two numeric vectors of data points building pairs for pointwise
        interpolation.
        }
    
}
       
    
\value{
    
    \code{linearInterp}\cr
    \cr

    returns a list with at least three entries, \code{x}, \code{y} 
    and \code{z}. Note, that the returned values, can be directly 
    used by the  \code{persp} and \code{contour} 3D plotting methods.
    
    \code{linearInterpp}\cr
    \cr
    
    returns a data.frame with columns \code{"x"}, \code{"y"}, 
    and \code{"z"}.
    
}


\seealso{

    \code{\link{akimaInterp}}, and \code{\link{krigeInterp}}.
    
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.\cr
    
}


\examples{ 
## linearInterp -
   # Linear Interpolation:    
   if (require(akima)) {
     set.seed(1953)
     x = runif(999) - 0.5
     y = runif(999) - 0.5
     z = cos(2*pi*(x^2+y^2))
     ans = linearInterp(x, y, z, gridPoints = 41)
     persp(ans, theta = -40, phi = 30, col = "steelblue",
        xlab = "x", ylab = "y", zlab = "z")
     contour(ans)
   }
}


\keyword{programming}

