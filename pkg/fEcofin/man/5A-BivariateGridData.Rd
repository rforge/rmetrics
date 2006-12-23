\name{BivariateGridData}

\alias{BivariateGridData}


\alias{gridData}
\alias{persp.gridData}
\alias{contour.gridData}

\alias{akimaInterp}
\alias{krigeInterp}


\title{Bivariate Gridded Data Sets}


\description{
    
    A collection and description of functions which
    allow to generate bivariate gridded data sets and
    to interpolate bivariate irregular data sets.
    \cr

    Grid Data Functions:
    
    \tabular{ll}{
    \code{gridData} \tab generates a grid data set of class 'gridData', \cr
    \code{persp} \tab generates a perspective plot from a grid data set, \cr
    \code{contour} \tab generates a contour plot from a grid data set.}
    
    Interpolation Functions:
    
    \tabular{ll}{
    \code{akimaInterp} \tab performs Akima spline interpolation, \cr
    \code{krigeInterp} \tab performs krige interpolation.}
    
}
    

\usage{ 
gridData(x = (-10:10)/10, y = x, z = outer(x, y, function(x, y) (x^2+y^2)) ))

\method{persp}{gridData}(x, theta = -40, phi = 30, col = "steelblue", 
    ticktype = "detailed", \dots)
\method{contour}{gridData}(x, addImage = TRUE, \dots)

akimaInterp(x, y = NULL, z = NULL, gridPoints = 21,
    xo = seq(min(x), max(x), length = gridPoints),
    yo = seq(min(y), max(y), length = gridPoints), extrap = TRUE)

krigeInterp(x, y = NULL, z = NULL, gridPoints = 21,
xo = seq(min(x), max(x), length = gridPoints),
yo = seq(min(y), max(y), length = gridPoints), extrap = TRUE, polDegree = 6)
}


\arguments{
  
    \item{addImage}{
        [contour] - \cr
        a logical flag indicating if an image plot should be underlayed
        to the contour level plot.
        }
    \item{x, y, z}{
        [griddata] - \cr
        \code{x} and \code{y} are two numeric vectors of grid pounts
        and \code{z} is a numeric matrix or any other rectangular object 
        which can be transformed by the function \code{as.matrix} into 
        a matrix object. \cr
        [akimaInterp][krigeInterp] - \cr
        either three numeric vectors of equal length or if  \code{y} 
        and \code{z} are NULL, a list with entries \code{x}, \code{y}, 
        \code{z}, or named data frame with \codde{x} in the first, 
        \code{y} in the second, and \code{z} in the third column. \cr
        [persp][contour] - \cr
        \code{x} is an pbject of class \code{gridData}.
        }
    \item{gridPoints}{
        an integer value specifying the number of grid points in \code{x} 
        and \code{y} direction.
        } 
    \item{theta, phi, col, ticktype}{
        [persp] - \cr
        tailored parameters passed the perspective plot function \code{persp}.
    \item{xo, yo}{
        a numeric vector of data points spanning the grid.
        }
    \item{extrap}{
        a logical, if \code{TRUE} then the data points are extrapolated.
        }
    \item{polDegree}{
        the polynomial krige degree, an integer ranging between 1 and 6.
        }
    \item{\dots}{
        [contour][persp] - \cr
        additional arguments to be passed to the perspectice and countour
        plot functions.
        }
    
}

\note{

    The function \code{krigeInterp} requires loading of the R package
    \code{spatial}.
    
    
\value{
    
    \code{gridData} - \cr
    \code{akimaInterp} - \cr
    \code{akimaInterp} - \cr
    A list with at least three entries, \code{x}, \code{y} and \code{z}. 
    
    The returned values, can be directly used by the \code{persp.gridData()} 
    and \code{contour.gridData} 3D plotting methods.
    
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port,\cr
    H. Akima for the Fortran Code of the Akima spline interpolation routine.\cr
    
}


\examples{
## SOURCE("fEcofin.5A-BivariateInterpolation")

## gridData -
   # Grid Data Set
   gD = gridData()
   persp(gD)
   contour(gD)
   
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

