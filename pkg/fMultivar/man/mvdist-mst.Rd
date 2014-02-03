\name{mvdist-mst}


\title{Multivariate Skew Student-t Distribution}


\description{
    
    Density, distribution function, and random number generation 
    for the multivariate Skew-Student-t distribution. 
    
}

\details{
    
    The functions to compute densities \code{dmst}, probabilities 
    \code{pmst}, and to generate random numbers \code{rmst} are 
    available from the contributed R package \code{sn}.
    
    \code{dmst(x, xi = rep(0, length(alpha)), Omega, alpha, nu = Inf, dp = NULL, log = FALSE)}\cr
    \code{pmst(x, xi = rep(0, length(alpha)), Omega, alpha, nu = Inf, dp = NULL, ...)}\cr
    \code{rmst(n = 1, xi = rep(0, length(alpha)), Omega, alpha, nu = Inf, dp = NULL)}\cr
         
}


\examples{
## Parameter Settings:
   x <- seq(-3, 3,length=15)
   xi <- c(0.5, -1)
   Omega <- diag(2)
   Omega[2, 1] <- Omega[1, 2] <- 0.5
   alpha <- c(2, -6)

## dmst - 
   # Student-t Density Function:
   dmst(cbind(x, 2*x-1), xi, Omega, alpha)

## pmst - 
   # Student-t Probability Function:
   pmst(c(2,1), xi, Omega, alpha, nu=5)
   pmst(c(2,1), xi, Omega, alpha, nu=5, abseps=1e-12, maxpts=10000)

## rmst -
   # Student-t Random Number Generation:
   rmst(10,  xi, Omega, alpha, 6)
}


\keyword{math}

