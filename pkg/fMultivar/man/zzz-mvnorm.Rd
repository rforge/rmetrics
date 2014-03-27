\name{zzz-mvnorm}


\title{Multivariate Normal Distribution}


\description{
    
    Alternative density, distribution function, and random generation 
    for the multivariate Normal distribution. 
    
}


\details{
    
    The multivariate distribution functions to compute densities 
    \code{dmvnorm}, probabilities \code{pmvnorm}, and to generate 
    random numbers \code{rmvnorm} are available from the contributed 
    R package \code{mvtnorm}.
    The function \code{qmvnorm} computes the equicoordinate quantile 
    function of the multivariate normal distribution for arbitrary 
    correlation matrices based on inversion of \code{pmvnorm}.
    
    \code{dmvnorm(x, mean, sigma, <<...>>}\cr
    \code{pmvnorm(<<...>>)}\cr
    \code{qmvnorm(p, <<...>>)}\cr
    \code{rmvnorm(n, mean, sigma, <<...>>}
    
    NOTE: The function are not builtin in the package \code{fMultivar}.
    Fur details we refer to the help page of \code{mvnorm}.
         
}


\author{

    Friedrich Leisch and Fabian Scheipl.

}


\examples{
\dontrun{
## Load Libray:
   require(mvtnorm)
   
## dmvnorm - 
   # Multivariate Normal Density Function:
   mean <- c(1, 1)
   sigma <- matrix(c(1, 0.5, 0.5, 1), ncol=2) 
   dmvnorm(x = c(0, 0),mean, sigma)
   
## dmvnorm - 
   # Across a Grid:
   x <- seq(-4, 4, length=90)
   X <- grid2d(x)
   X <- cbind(X$x, X$y) 
   # Write Density Function:
   dmvnorm. <- function(X, mean, sigma)
     matrix(apply(X, 1, dmvnorm, mean=mean, sigma=sigma), ncol=sqrt(dim(X)[1]))
   z <- dmvnorm.(X, mean, sigma)
   contour(list(x = x, y = x, z = z))
   
## qmvnorm -
   # Equicoordinate Quantile Function:
   qmvnorm(p = 0.95, sigma = diag(2), tail = "both")
   
## rmvnorm - 
   # Random Numbers:
   sigma <- matrix(c(4, 2, 2, 3), ncol=2)
   x <- rmvnorm(n = 500, mean = c(1, 2), sigma = sigma)
   colMeans(x)
   var(x)
   # Next Generation:
   x <- rmvnorm(n = 500, mean = c(1, 2), sigma = sigma, method = "chol")
   colMeans(x)
   var(x)
   plot(x, cex=0.5, pch=19, col="steelblue")
}
}


\keyword{math}

