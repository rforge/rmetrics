\name{mvdist-mvnorm}


\title{Multivariate Normal Distribution}


\description{
    
    Density, distribution function, and random generation for the 
    multivariate Normal distribution. 
    
}


\details{
    
    The multivariate distribution functions to compute densities 
    \code{dmvnorm}, probabilities \code{pmvorm}, and to generate 
    random numbers \code{rmvnorm} are available from the contributed 
    R package \code{mvtnorm}.
    
    The function \code{qmvnorm} computes the equicoordinate quantile 
    function of the multivariate normal distribution for arbitrary 
    correlation matrices based on inversion of \code{pmvnorm}.
    
    \code{dmvnorm(x, mean, sigma, log = FALSE)}\cr
    \code{pmvnorm(lower = -Inf, upper = Inf, mean = rep(0, length(lower)),}\cr
        \code{corr = NULL, sigma = NULL, algorithm = GenzBretz(), ...)}\cr
    \code{qmvnorm(p, interval = NULL, tail = c("lower.tail", "upper.tail","both.tails"),}\cr 
        \code{mean = 0, corr = NULL, sigma = NULL, algorithm = GenzBretz(), ...)}\cr
    \code{rmvnorm(n, mean = rep(0, nrow(sigma)), sigma = diag(length(mean)),}\cr 
        \code{method = c("eigen", "svd", "chol"), pre0.9_9994 = FALSE)}
         
}


\author{

    Alan Genz, Frank Bretz, Tetsuhisa Miwa, Xuefei Mi, Friedrich Leisch, 
    Fabian Scheipl, Bjoern Bornkamp, Torsten Hothorn.

}


\examples{
## dmvnorm - 
   # Multivariate Normal Density Function:
   mean <- c(1, 1)
   sigma <- matrix(c(1, 0.5, 0.5, 1), ncol=2) 
   dmvnorm(x = c(0, 0),mean, sigma)
   
## dmvnorm. - 
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
   plot(x)
}


\keyword{math}

