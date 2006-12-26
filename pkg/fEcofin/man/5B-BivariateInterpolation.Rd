\name{BivariateInterpolation}

\alias{BivariateInterpolation}

\alias{linearInterp}
\alias{akimaInterp}
\alias{krigeInterp}

\alias{linearInterpp}
\alias{akimaInterpp}

\title{Bivariate Interpolation}


\description{
    
    A collection and description of functions which
    allow to interpolate bivariate irregular data sets.
    \cr
    
    Gridded Interpolation Functions:
    
    \tabular{ll}{
    \code{linearInterp} \tab performs linear spline interpolation, \cr
    \code{akimaInterp} \tab performs Akima spline interpolation, \cr
    \code{krigeInterp} \tab performs krige interpolation.}
    
    Pointwise Interpolation Functions:
    
    \tabular{ll}{
    \code{linearInterpp} \tab performs linear spline interpolation, \cr
    \code{akimaInterpp} \tab performs Akima spline interpolation}
    
}
    

\usage{ 
linearInterp(x, y = NULL, z = NULL, gridPoints = 21,
    xo = seq(min(x), max(x), length = gridPoints),
    yo = seq(min(y), max(y), length = gridPoints))
linearInterpp(x, y = NULL, z = NULL, xo, yo)
     
akimaInterp(x, y = NULL, z = NULL, gridPoints = 21,
    xo = seq(min(x), max(x), length = gridPoints),
    yo = seq(min(y), max(y), length = gridPoints), extrap = FALSE)
akimaInterpp(x, y = NULL, z = NULL, xo, yo, extrap = FALSE)

krigeInterp(x, y = NULL, z = NULL, gridPoints = 21,
    xo = seq(min(x), max(x), length = gridPoints),
    yo = seq(min(y), max(y), length = gridPoints), 
    extrap = FALSE, polDegree = 6)
}


\arguments{
  
    \item{x, y, z}{
        [griddata] - \cr
        \code{x} and \code{y} are two numeric vectors of grid pounts
        and \code{z} is a numeric matrix or any other rectangular object 
        which can be transformed by the function \code{as.matrix} into 
        a matrix object. \cr
        [akimaInterp][krigeInterp] - \cr
        either three numeric vectors of equal length or if  \code{y} 
        and \code{z} are NULL, a list with entries \code{x}, \code{y}, 
        \code{z}, or named data frame with \code{x} in the first, 
        \code{y} in the second, and \code{z} in the third column. \cr
        [persp][contour] - \cr
        \code{x} is an pbject of class \code{gridData}.
        }
    \item{gridPoints}{
        an integer value specifying the number of grid points in \code{x} 
        and \code{y} direction.
        } 
    \item{xo, yo}{
        [*Interp] - \cr
        two numeric vectors of data points spanning the grid.\cr
        [*Interpp] - \cr
        two numeric vectors of data points building pairs for pointwise
        interpolation.
        }
    \item{extrap}{
        a logical, if \code{TRUE} then the data points are extrapolated.
        }
    \item{polDegree}{
        [krige] - \cr
        the polynomial krige degree, an integer ranging between 1 and 6.
        }
    
}

\note{

    The function \code{krigeInterp} requires loading of the R package
    \code{spatial}.

}
       
    
\value{
    
    \code{linearInterp} - \cr
    \code{akimaInterp} - \cr
    \code{krigeInterp} - \cr
    A list with at least three entries, \code{x}, \code{y} and \code{z}. 
    Note, that the returned values, can be directly used by the 
    \code{persp.gridData()} and \code{contour.gridData} 3D plotting methods.
    
    \code{linearInterpp} - \cr
    \code{akimaInterpp} - \cr
    A data.frame with columns \code{"x"}, \code{"y"}, and \code{"z"}.
    
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port,\cr
    H. Akima for the Fortran Code of the Akima spline interpolation routine.\cr
    
}


\examples{
## SOURCE("fEcofin.5B-BivariateInterpolation")
  
## akimaInterp -
   # Akima Interpolation:    
   set.seed(1953)
   x = runif(999) - 0.5
   y = runif(999) - 0.5
   z = cos(2*pi*(x^2+y^2))
   ans = akimaInterp(x, y, z, extrap = FALSE)
   persp(ans)
   contour(ans)
   
## krigeInterp -
   # Kriging:    
   ans = krigeInterp(x, y, z, extrap = FALSE)
   persp(ans)
   contour(ans)
}


\keyword{programming}

