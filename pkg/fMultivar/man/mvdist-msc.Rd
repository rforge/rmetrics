\name{mvdist-msc}


\title{Multivariate Skew Cauchy Distribution}


\description{
    
    Density, distribution function, and random number generation 
    for the multivariate Cauchy distribution. 
    
}


\details{
    
    The functions to compute densities \code{dmsc}, probabilities 
    \code{pmsc}, and to generate random numbers \code{rmsc} are 
    available from the contributed R package \code{sn}.
    
    \code{dmsc(x, xi = rep(0, length(alpha)), Omega, alpha, dp = NULL, log = FALSE)}\cr 
    \code{pmsc(x, xi = rep(0, length(alpha)), Omega, alpha, dp = NULL, ...) }\cr
    \code{rmsc(n = 1, xi = rep(0, length(alpha)), Omega, alpha, dp = NULL)}
         
}


\examples{
## Settings:
    x <- seq(-3, 3, length = 101)
    X2 <- grid2d(x)
    X <- cbind(X2$x, X2$y)
    xi <- c(0.5, -1)
    Omega <- diag(2)
    Omega[2, 1] <- Omega[1, 2] <- 0.5
  
## dmsc -
    # Cauchy:
    alpha <- c(0, 0)
    d <- dmsc(X, xi, Omega, alpha)
    contour(x, x, matrix(d, ncol=length(x)),
        xlim = c(-3, 3)+xi[1], ylim=c(-3,3)+xi[2] )
 
## dcauchy - 
    # Cauchy Density:
    d <- dcauchy2d(X2$x-xi[1], X2$y-xi[2], rho=Omega[1,2])
    contour(x, x, matrix(d, ncol=length(x)), 
        xlim = c(-3, 3)+xi[1], ylim=c(-3, 3)+xi[2] )
    
## dmsc -
    # Skew Cauchy Density:
    alpha <- c(2, 2) 
    d <- dmsc(X, xi, Omega, alpha)
    contour(x, x, matrix(d, ncol=length(x)), 
        xlim=c(-1, 3), ylim=c(-3,2))
}


\keyword{math}

