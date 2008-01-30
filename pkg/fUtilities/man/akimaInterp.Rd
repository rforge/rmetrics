\name{akimaInterp}


\alias{akimaInterp}
\alias{akimaInterpp}


\title{Bivariate Spline Interpolation}


\description{
    
    Bivariate Akima Spline Interpolation. Two options are
    available gridded and pointwise interpolation.
    
}
    

\usage{      
akimaInterp(x, y = NULL, z = NULL, gridPoints = 21,
    xo = seq(min(x), max(x), length = gridPoints),
    yo = seq(min(y), max(y), length = gridPoints), extrap = FALSE)
    
akimaInterpp(x, y = NULL, z = NULL, xo, yo, extrap = FALSE)
}


\arguments{
  
    \item{x, y, z}{
        for \code{akimaInterp} the arguments \code{x} and \code{y} are 
        two numeric vectors of grid pounts, and \code{z} is a numeric 
        matrix or any other rectangular object which can be transformed 
        by the function \code{as.matrix} into a matrix object.  
        For \code{akimaInterpp} we  consider either three numeric vectors 
        of equal length or if  \code{y} and \code{z} are NULL, a list with 
        entries \code{x}, \code{y}, \code{z}, or named data frame with 
        \code{x} in the first, \code{y} in the second, and \code{z} in 
        the third column. \cr
    \item{gridPoints}{
        an integer value specifying the number of grid points in \code{x} 
        and \code{y} direction.
        } 
    \item{xo, yo}{
        for \code{akimaInterp}
        two numeric vectors of data points spanning the grid, and
        for \code{akimaInterpp}
        two numeric vectors of data points building pairs for pointwise
        interpolation.
        }
    \item{extrap}{
        a logical, if \code{TRUE} then the data points are extrapolated.
        }
    
}

\note{

    IMPORTANT: The contributed package \code{akima} is not in the 
    dependence list of the DESCRIPTION file due to license conditions.
    The Rmetrics user has to load this package from the CRAN server on 
    his own responsibility, please check the license conditions.

}
       
    
\value{

    \code{akimaInterp}\cr
    \cr
    
    returns a list with at least three entries, \code{x}, \code{y} 
    and \code{z}. Note, that the returned values, can be directly 
    used by the  \code{persp} and \code{contour} 3D plotting methods.
    
    \code{akimaInterpp}\cr
    \cr
    
    returns a data.frame with columns \code{"x"}, \code{"y"}, 
    and \code{"z"}.
    
}


\seealso{

    \code{\link{linearInterp}}, and \code{\link{krigeInterp}}.
    
}



\author{

    Diethelm Wuertz for the Rmetrics \R-port,\cr
    H. Akima for the Fortran Code of the Akima spline interpolation routine.\cr
    
}


\examples{ 
## akimaInterp -
   # Akima Interpolation:    
   if (require(akima)) {
     set.seed(1953)
     x = runif(999) - 0.5
     y = runif(999) - 0.5
     z = cos(2*pi*(x^2+y^2))
     ans = akimaInterp(x, y, z, gridPoints = 41, extrap = FALSE)
     persp(ans, theta = -40, phi = 30, col = "steelblue",
        xlab = "x", ylab = "y", zlab = "z")
     contour(ans)
   }
}


\keyword{programming}

