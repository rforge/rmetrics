\name{utils-density2}

\alias{density2d}
\alias{hist2d}


\title{Bivariate Density Tools}


\description{
    
    Kernel density estimator and histogram counter for 
    bivariate distributions

}


\usage{
density2d(x, y = NULL, n = 20, h = NULL, limits = c(range(x), range(y))) 

hist2d(x, y = NULL, n = c(20, 20))
}


\arguments{

    \item{x, y}{
        two vectors of coordinates of data. If \code{y} is NULL then
        \code{x} is assumed to be a two column matrix, where the first 
        column contains the \code{x} data, and the second column the 
        \code{y} data. 
        }
    \item{n}{
        n - an integer specifying the number of grid points in each 
        direction. The default value is 20.\cr
        [hist2D] - \cr
        In this case \code{n} may be a scalar or a two element vector. 
        The default value is 20.
        }
    \item{h}{
        a vector of bandwidths for \code{x} and \code{y} directions. 
        Defaults to normal reference bandwidth. 
        }
    \item{limits}{
        the limits of the rectangle covered by the grid.   
        }
        
}


\value{

    \code{density2d} and \code{hist2d}
    return a list with three elements \code{$x}, \code{$y}, and 
    \code{$z}. \code{x} and \code{y} are vectors spanning the two 
    dimensional grid and \code{z} the corresponding matrix. The 
    output can directly serve as input to the plotting functions 
    \code{image}, \code{contour} and \code{persp}.
    
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

