\name{utils-grid2}

\alias{grid2d}


\title{Bivariate Density Tools}


\description{
    
    Grid generator for bivariate distributions.

}


\usage{
grid2d(x = (0:10)/10, y = x)
}


\arguments{

    \item{x, y}{
        two numeric vectors defining the \code{x} and \code{y} coordinates.
        }
        
}


\value{

    \code{grid2d} 
    returns a list with two vectors named \code{$x} and \code{$y} spanning 
    the grid defined by the coordinate vectors \code{x} and \code{y}.
    
   
}


%\references{}


\author{

    Diethelm Wuertz.
    
}


\examples{
## grid2d -
   # Create a square grid:
   x <- seq(0, 10, length = 6)
   X <- grid2d(x = x, y = x)
   cbind(X$x, X$y)
}


\keyword{math}

