\name{mvdist-mvt}


\title{Multivariate Student-t Distribution}


\description{
    
    Density, distribution function, and random generation for the 
    multivariate Student-t distribution. 
    
}


\details{
    
    The functions to compute densities \code{dmvt}, probabilities 
    \code{pmvt}, and to generate random numbers \code{rmvt} are 
    available from the contributed R package \code{mvtnorm}.
    
    The function \code{qmvt} computes the equicoordinate quantile 
    function of the multivariate normal distribution for arbitrary 
    correlation matrices based on inversion of \code{pmvt}.
    
    \code{dmvt(x, delta, sigma, df = 1, log = TRUE, type = "shifted")}\cr
    \code{pmvt(lower = -Inf, upper = Inf, delta = rep(0, length(lower)),}\cr
        \code{df = 1, corr = NULL, sigma = NULL, algorithm = GenzBretz(),}\cr 
        \code{type = c("Kshirsagar", "shifted"), ...) }\cr
    \code{rmvt(n, sigma = diag(2), df = 1, delta = rep(0, nrow(sigma)),}\cr
        \code{type = c("shifted", "Kshirsagar"), ...)}
         
}


\author{

    Alan Genz, Frank Bretz, Tetsuhisa Miwa, Xuefei Mi, Friedrich 
    Leisch, Fabian Scheipl, Bjoern Bornkamp, Torsten Hothorn.
    
}


\examples{
## dmvt -
   # Density Function:
   dmvt(x = c(0,0), sigma = diag(2))
   
   # Check behavior for df=0 and df=Inf
   x <- c(1.23, 4.56)
   mu <- 1:2
   Sigma <- diag(2)
   x0 <- dmvt(x, delta = mu, sigma = Sigma, df = 0) # default log = TRUE!
   x8 <- dmvt(x, delta = mu, sigma = Sigma, df = Inf) # default log = TRUE!
   xn <- dmvnorm(x, mean = mu, sigma = Sigma, log = TRUE)
   identical(x0, xn)

## rmvt - 
   # Random Number Generation:
   # X ~ t_3(0, diag(2))
   x <- rmvt(100, sigma = diag(2), df = 3) # t_3(0, diag(2)) sample
   plot(x)
   
   # Next:
   # X ~ t_3(mu, Sigma)
   n <- 1000
   mu <- 1:2
   Sigma <- matrix(c(4, 2, 2, 3), ncol=2)
   set.seed(271)
   x <- rep(mu, each=n) + rmvt(n, sigma=Sigma, df=3)
   plot(x)

   # Note that the call rmvt(n, mean=mu, sigma=Sigma, df=3) does *not*
   # give a valid sample from t_3(mu, Sigma)! [and thus throws an error]
   try(rmvt(n, mean=mu, sigma=Sigma, df=3))

   # df=Inf correctly samples from a multivariate normal distribution
   set.seed(271)
   x <- rep(mu, each=n) + rmvt(n, sigma=Sigma, df=Inf)
   set.seed(271)
   x. <- rmvnorm(n, mean=mu, sigma=Sigma)
   identical(x, x.)
}


\keyword{math}

