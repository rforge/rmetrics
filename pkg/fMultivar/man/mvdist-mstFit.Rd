\name{mvdist-mstFit}


\alias{mstFit}

\title{Multivariate Skew Student-t Parameter Estimation}


\description{
    
    Fitting the parameters for the Multivariate Skew Student-t
    Distribution
    
}


\usage{
    mstFit(x, dp = NULL, trace=FALSE, title = NULL, description = NULL, ...)
}


\arguments{

    \item{x}{
        a matrix with "d" columns, giving the coordinates of the 
        point(s) where the density must be evaluated.
        }
    \item{dp}{
        a list with three elements, corresponding to \code{xi}, 
        \code{Omega} and \code{alpha}. 
        \code{xi} is a numeric vector of length "d" representing 
        the location parameter of the distribution,
        \code{Omega} is a symmetric positive-definite matrix of 
        dimension "d" timesd "d",
        \code{alpha} is a numeric vector which regulates the 
        the slant of the density,
        \code{nu} a positive value representing the degrees of freedom.
        }
    \item{trace}{
        a logical value, should the estimation be traced? By default
        FALSE.
        }
    \item{title}{
        an optional project title.
        }
    \item{description}{
        an option project desctiption. 
        }
    \item{...}{
        optional parameters passed to the underlying "sn" functions.
    }
    
}


\examples{
\dontrun{
## Load Library:
   require(sn)
   
## mstFit - 
   # Fit Example:
   N <- 1000
   xi <- c(0, 0) 
   Omega <- diag(2); Omega[2,1] <- Omega[1,2] <- 0.5
   alpha <- c(2, -6)
   nu <- 4
   set.seed(4711)
   X <- rmst(n=N, xi, Omega, alpha, nu=4)
   ans <- mstFit(X)

   # 2-D Density Plot:
   plot(hexBinning(X[,1], X[, 2], bins = 30))

   # Add Contours:
   N <- 101
   x <- seq(min(X[, 1]), max(X[, 1]), l=N)
   y <- seq(min(X[, 2]), max(X[, 2]), l=N)        
   u <- grid2d(x, y)$x
   v <- grid2d(x, y)$y
   X <- cbind(u, v)
   param <- ans@fit$estimate
   Z <- matrix(dmst(X, 
     xi=param$xi[1,], Omega=param$Omega, alpha=param$alpha, nu=param$nu), 
     ncol=N)
   contour(x, y, Z, add=TRUE, col="green", lwd=2)
   grid(col="brown", lty=3)
}
}


\keyword{math}
