\name{mvdist-mst}


\title{Multivariate Skew Student-t Distribution}


\description{
    
    Density, distribution function, and random number generation 
    for the multivariate Skew-Student-t distribution. 
    
}

\details{
    
    The functions to compute densities \code{dmsc}, probabilities 
    \code{pmsc}, and to generate random numbers \code{rmsc} for the
    multivariate skew Student-t distribution are available in the contributed 
    R package \code{sn} (note, they are no longer builtin in fMultivar). 
    The reason is that the performance for these functions in package
    \code{sn} has superseeded those used before in the package fMultivar.
    
    The usage of the \code{sn} functions is:
    
    \code{dmst(x, xi, Omega, alpha, nu = Inf, dp = NULL, log = FALSE)}\cr
    \code{pmst(x, xi, Omega, alpha, nu = Inf, dp = NULL, ...)}\cr
    \code{rmst(n, xi, Omega, alpha, nu = Inf, dp = NULL)}\cr
    
    NOTE: The multivariate skew-normal distribution is discussed by 
    Azzalini and Dalla Valle (1996). The \code{(Omega,alpha)} 
    parametrization adopted here is the one of Azzalini and Capitanio 
    (1999). Chapter 5 of Azzalini and Capitanio (2014) provides an 
    extensive account, including subsequent developments.
    Be aware that the location vector \code{xi} does not represent 
    the mean vector of the distribution. Similarly,\code{Omega} is 
    not the covariance matrix of the distribution, although it is 
    a covariance matrix.

    For further details we refer to the help page in the package 
    \code{sn}.
    
}


\references{
  Azzalini, A. and Dalla Valle, A. (1996), 
  The multivariate skew-normal distribution,
  Biometrika 83, 715-726.

  Azzalini, A. and Capitanio, A. (1999), 
  Statistical applications of the multivariate skew normal distribution,
  Journal Roy.Statist.Soc. B 61, 579-602, 
  Full-length version available at http://arXiv.org/abs/0911.2093

  Azzalini, A. with the collaboration of Capitanio, A. (2014),
  The Skew-Normal and Related Families,
  Cambridge University Press, IMS Monographs Series.
}


\examples{
## Make 2-D Grid Coordinates:
   N <- 101
   x <- y <- seq(-3, 3, l=N)
   X <- cbind(u=grid2d(x)$x, v=grid2d(x)$y)
   
## dmst - 
   # Set Parameters:
   xi <- c(0, 0) 
   Omega <- diag(2); Omega[2,1] <- Omega[1,2] <- 0.5
   alpha <- c(2, -6)
   nu <- 4
   # Compute skew Student-t Density:
   z <- dmst(X, xi, Omega, alpha, nu)
   Z <- list(x=x, y=x, z=matrix(z, ncol = length(x)))
   # Plot:   
   image(Z)
   contour(Z)
   grid(col="red")
   
## rmst - 
   set.seed(4711)
   r <- rmst(n=5000, xi, Omega, alpha, nu)
   plot(hexBinning(r))
   contour(Z, add=TRUE, col="darkblue", lwd=2)
   grid(col="red")
}


\keyword{math}

