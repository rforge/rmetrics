\name{density2d}

\alias{density2d}

\alias{grid2d}
\alias{density2d}
\alias{hist2d}
\alias{integrate2d}


\title{Bivariate Density Tools}


\description{
    
    Grid generator, kernel density estimator, 
    histogram counter, and integrator for 
    bivariate distributions

}


\usage{
grid2d(x = (0:10)/10, y = x)
density2d(x, y = NULL, n = 20, h = NULL, limits = c(range(x), range(y))) 
hist2d(x, y = NULL, n = c(20, 20))
integrate2d(fun, error = 1.0e-5, \dots)
}


\arguments{

    \item{error}{
        the error bound to be achieved by the integration formula.
        A numeric value.
        }
    \item{fun}{
        the function to be integrated. The first argument requests
        the x values, the second the y values, and the remaining
        are reserved for additional parameters. The integration is 
        over the unit square "[0,1]\^2".
        }
    \item{h}{
        a vector of bandwidths for \code{x} and \code{y} directions. 
        Defaults to normal reference bandwidth. 
        }
    \item{limits}{
        the limits of the rectangle covered by the grid.   
        }
     \item{n}{
        n - an integer specifying the number of grid points in each 
        direction. The default value is 20.\cr
        [hist2D] - \cr
        In this case \code{n} may be a scalar or a two element vector. 
        The default value is 20.\cr
        [rnorm2d] - \cr
        the number of random deviates to be generated, an integer value.
        }
    \item{x, y}{
        two numeric vectors defining the x and y coordinates. \cr
        [density2D][hist2D] - \cr
        two vectors of coordinates of data. If \code{y} is NULL then
        \code{x} is assumed to be a two column matrix, where the first 
        column contains the \code{x} data, and the second column the 
        \code{y} data. 
        }
    \item{\dots}{
        parameters passed to the function to be integrated.     
    }
        
}


\value{

    \code{grid2d} 
    \cr
    returns a list with two vectors named \code{$x} and \code{$y} spanning 
    the grid defined by the coordinate vectors \code{x} and \code{y}.
    
    \code{density2d}\cr
    \code{hist2d}
    \cr
    returns a list with three elements \code{$x}, \code{$y}, and 
    \code{$z}. \code{x} and \code{y} are vectors spanning the two 
    dimensional grid and \code{z} the corresponding matrix. The 
    output can directly serve as input to the plotting functions 
    \code{image}, \code{contour} and \code{persp}.
    
    \code{integrate2d}
    \cr
    returns a list with the \code{$value} of the integral over the
    unit square [0,1]\^2, an \code{$error} estimate and the number 
    of grid \code{$points} used by the integration function. 
    
}


\references{
    
Azzalini A., (2004);
    \emph{The sn Package};
    R Reference Guide available from www.r-project.org.
    
Venables W.N., Ripley B.D., (2002); 
    \emph{Modern Applied Statistics with S},
    Fourth Edition, Springer.
    
Warnes G.R., (2004);
    \emph{The gregmisc Package};
    R Reference Guide available from www.r-project.org.
         
}


\author{

    W.N. Venables and B.D. Ripley for the underlying kde2d function, \cr
    Gregory R. Warnes for the underlying hist2d function, \cr
    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\keyword{math}

