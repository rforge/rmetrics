\name{zzz-mvt}


\title{Multivariate Student-t Distribution}


\description{
    
    Alternative density, distribution function, and random generation 
    for the multivariate Student-t distribution. 
    
}


\details{
    
    The functions to compute densities \code{dmvt}, probabilities 
    \code{pmvt}, and to generate random numbers \code{rmvt} are 
    available from the contributed R package \code{mvtnorm}.
    The function \code{qmvt} computes the equicoordinate quantile 
    function of the multivariate normal distribution for arbitrary 
    correlation matrices based on inversion of \code{pmvt}.
    
    \code{dmvt(x, delta, sigma, df, <<...>>)}\cr
    \code{pmvt(<<...>>) }\cr
    \code{rmvt(n, sigma, df, delta, <<...>>}\cr
    
    NOTE: The function are not builtin in the package \code{fMultivar}.
    Fur details we refer to the help page of \code{mvnorm}.
         
}


\references{

  McNeil, A. J., Frey, R., and Embrechts, P. (2005), 
  \emph{Quantitative Risk Management: Concepts, Techniques, Tools},
  Princeton University Press.
  
}

\author{

  Alan Genz, Frank Bretz, Tetsuhisa Miwa, Xuefei Mi, Friedrich 
  Leisch, Fabian Scheipl, Bjoern Bornkamp, Torsten Hothorn.
    
}


\examples{
\dontrun{
## Load Libray:
   require(mvtnorm)
   
## dmvt - 
   # basic evaluation
   dmvt(x = c(0,0), sigma = diag(2))

## dmvt | dmvnorm - 
   # check behavior for df=0 and df=Inf
   x <- c(1.23, 4.56)
   mu <- 1:2
   Sigma <- diag(2)
   x0 <- dmvt(x, delta = mu, sigma = Sigma, df = 0) # default log = TRUE!
   x8 <- dmvt(x, delta = mu, sigma = Sigma, df = Inf) # default log = TRUE!
   xn <- dmvnorm(x, mean = mu, sigma = Sigma, log = TRUE)
   stopifnot(identical(x0, x8), identical(x0, xn))

## rmvt -
   # X ~ t_3(0, diag(2))
   x <- rmvt(100, sigma = diag(2), df = 3) # t_3(0, diag(2)) sample
   plot(x)

## rmvt -
   # X ~ t_3(mu, Sigma)
   n <- 1000
   mu <- 1:2
   Sigma <- matrix(c(4, 2, 2, 3), ncol=2)
   set.seed(271)
   x <- rep(mu, each=n) + rmvt(n, sigma=Sigma, df=3)
   plot(x)

## rmvt -
   # Note that the call rmvt(n, mean=mu, sigma=Sigma, df=3) does *not*
   # give a valid sample from t_3(mu, Sigma)! [and thus throws an error]
   try(rmvt(n, mean=mu, sigma=Sigma, df=3))

## rmvnorm - 
   # df=Inf correctly samples from a multivariate normal distribution
   set.seed(271)
   x <- rep(mu, each=n) + rmvt(n, sigma=Sigma, df=Inf)
   set.seed(271)
   x. <- rmvnorm(n, mean=mu, sigma=Sigma)
   stopifnot(identical(x, x.))
}
}


\keyword{math}


