\name{BivariateGridData}

\alias{BivariateGridData}


\alias{gridData}
\alias{persp.gridData}
\alias{contour.gridData}


\title{Bivariate Gridded Data Sets}


\description{
    
    A collection and description of functions which
    allow to generate bivariate gridded data sets.
    \cr

    Grid Data Functions:
    
    \tabular{ll}{
    \code{gridData} \tab generates a grid data set of class 'gridData', \cr
    \code{persp} \tab generates a perspective plot from a grid data set, \cr
    \code{contour} \tab generates a contour plot from a grid data set.}
    
}
    

\usage{ 
gridData(x = (-10:10)/10, y = x, z = outer(x, y, function(x, y) (x^2+y^2)) ))

\method{persp}{gridData}(x, theta = -40, phi = 30, col = "steelblue", 
    ticktype = "detailed", \dots)
\method{contour}{gridData}(x, addImage = TRUE, \dots)
}


\arguments{
  
    \item{addImage}{
        [contour] - \cr
        a logical flag indicating if an image plot should be underlayed
        to the contour level plot.
        }
    \item{x, y, z}{
        [gridData] - \cr
        \code{x} and \code{y} are two numeric vectors of grid pounts
        and \code{z} is a numeric matrix or any other rectangular object 
        which can be transformed by the function \code{as.matrix} into 
        a matrix object.
        }
    \item{theta, phi, col, ticktype}{
        [persp] - \cr
        tailored parameters passed the perspective plot function \code{persp}.
        }
    \item{\dots}{
        [contour][persp] - \cr
        additional arguments to be passed to the perspectice and countour
        plot functions.
        }
    
}

 
\value{
    
    \code{gridData} - \cr
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
}


\keyword{programming}

