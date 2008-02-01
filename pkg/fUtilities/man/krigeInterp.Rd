\name{krigeInterp}


\alias{krigeInterp}


\title{Bivariate Krige Interpolation}


\description{
    
    Bivariate Krige Interpolation.
    
}
    

\usage{ 
krigeInterp(x, y = NULL, z = NULL, gridPoints = 21,
    xo = seq(min(x), max(x), length = gridPoints),
    yo = seq(min(y), max(y), length = gridPoints), 
    extrap = FALSE, polDegree = 6)
}


\arguments{
  
    \item{x, y, z}{
        the arguments \code{x} and \code{y} are 
        two numeric vectors of grid pounts, and \code{z} is a numeric 
        matrix or any other rectangular object which can be transformed 
        by the function \code{as.matrix} into a matrix object. 
        } 
    \item{gridPoints}{
        an integer value specifying the number of grid points in \code{x} 
        and \code{y} direction.
        } 
    \item{xo, yo}{
        two numeric vectors of data points spanning the grid.
        }
    \item{extrap}{
        a logical, if \code{TRUE} then the data points are extrapolated.
        }
    \item{polDegree}{
        the polynomial krige degree, an integer ranging between 1 and 6.
        }
    
}


\note{

    The function \code{krigeInterp} requires loading of the R package
    \code{spatial}.

}
       
    
\value{
    
    \code{krigeInterp}
    
    returns a list with at least three entries, \code{x}, \code{y} 
    and \code{z}. Note, that the returned values, can be directly 
    used by the  \code{persp} and \code{contour} 3D plotting methods.
    
}


\seealso{

    \code{\link{akimaInterp}}, and \code{\link{linearInterp}}.
    
}


\examples{   
## krigeInterp -
   # Kriging:    
   set.seed(1953)
   x = runif(999) - 0.5
   y = runif(999) - 0.5
   z = cos(2*pi*(x^2+y^2))
   ans = krigeInterp(x, y, z, extrap = FALSE)
   persp(ans, theta = -40, phi = 30, col = "steelblue",
        xlab = "x", ylab = "y", zlab = "z")
   contour(ans)
   }
}


\keyword{programming}

