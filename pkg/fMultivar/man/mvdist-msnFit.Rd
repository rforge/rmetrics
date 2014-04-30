\name{mvdist-msnFit}


\alias{msnFit}


\title{Multivariate Skew Normal Parameter Estimation}


\description{
    
    Fitting the parameters for the multivariate skew Normal
    distribution.
    
}


\usage{
    msnFit(x, trace = FALSE, title = NULL, description = NULL) 
}


\arguments{

     \item{x}{
        a matrix with "d" columns, giving the coordinates of the 
        point(s) where the density must be evaluated.
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
    
}


\details{

    This is an easy to use wrapper function using default function 
    settings for fitting the distributional parameters in the framework 
    of the contributed package \code{"sn"} written by Adelchi Azzalini. 
    
    Starting values for the estimation have not to be provided, they
    are automatically created.
    
}


\examples{
\dontrun{
## Load Library:
   require(sn)
   
## msnFit - 
   # Fit Example:
   N <- 1000
   xi <- c(0, 0) 
   Omega <- diag(2); Omega[2,1] <- Omega[1,2] <- 0.5
   alpha <- c(2, -6)
   set.seed(4711)
   X <- rmsn(n=N, xi, Omega, alpha)
   ans <- msnFit(X)
   print(ans)

   # 2-D Density Plot:
   plot(hexBinning(X[,1], X[, 2], bins = 30), main="Skew Normal")
   # Add Contours:
   N <- 101
   x <- seq(min(X[, 1]), max(X[, 1]), l=N)
   y <- seq(min(X[, 2]), max(X[, 2]), l=N)        
   u <- grid2d(x, y)$x
   v <- grid2d(x, y)$y
   XY <- cbind(u, v)
   param <- ans@fit$estimate
   Z <- matrix(dmsn(XY, param[[1]][1,], param[[2]], param[[3]]), ncol=N)
   contour(x, y, Z, add=TRUE, col="green", lwd=2)
   grid(col="brown", lty=3)
}
}


\keyword{math}


