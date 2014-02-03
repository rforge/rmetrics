\name{mvdist-msn}


\title{Multivariate Skew-Normal Distribution}


\description{
    
    Density, distribution function, and random number generation 
    for the multivariate Skew-Normal distribution. 
    
}

\details{
    
    The functions to compute densities \code{dmsn}, probabilities 
    \code{pmsn}, and to generate random numbers \code{rmsn} are 
    available from the contributed R package \code{sn}.
    
    \code{dmsn(x, xi = rep(0, length(alpha)), Omega, alpha, tau = 0, dp = NULL, log = FALSE)}\cr
    \code{pmsn(x, xi = rep(0, length(alpha)), Omega, alpha, tau = 0, dp = NULL, ...)}\cr
    \code{rmsn(n = 1, xi = rep(0, length(alpha)), Omega, alpha, tau = 0, dp = NULL)}
         
}


\examples{
## Parameter Settings:
   x <- seq(-3,3,length=15)
   xi <- c(0.5, -1)
   Omega <- diag(2)
   Omega[2,1] <- Omega[1,2] <- 0.5
   alpha <- c(2,-6)

## dmsn - 
   # Density Function:
   dmsn(cbind(x, 2*x-1), xi, Omega, alpha)

## pmsn - 
   # Probability Function:
   pmsn(c(2,1), xi, Omega, alpha)
   pmsn(c(2,1), xi, Omega, alpha, abseps=1e-12, maxpts=10000)

## rmsn -
   # Random Number Generation:
   rmsn(10, xi, Omega, alpha)
}


\keyword{math}

