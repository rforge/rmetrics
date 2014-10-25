\name{StableMode}
\alias{stableMode}
\title{Mode of the Stable Distribution Function}
\description{
  Computes the mode of the stable distribution, i.e., the maximum of its
  density function in the "0" parametrization, i.e., the maximum \eqn{x_0} of
  \code{\link{dstable}(x, alpha, beta, gamma = 1, delta = 0, pm = 0)}.

  Finds the maximum of \code{\link{dstable}} numerically, using
  \code{\link{optimize}}.
}
\usage{
stableMode(alpha, beta,
           beta.max = 1 - 1e-11,
           tol = .Machine$double.eps^0.25)
}
\arguments{
  \item{alpha, beta}{numeric parameters:
    value of the index parameter \code{alpha} in the range \eqn{(0,2]},
    and the skewness parameter \code{beta}, in the range \eqn{[-1, 1]}.}
  \item{beta.max}{for numerical purposes, values of beta too close to 1,
    are set to \code{beta.max}.  Do not modify unless you know what
    you're doing.}
  \item{tol}{numerical tolerance for \code{\link{optimize}()}.}
}
\value{
  returns a numeric value, the location of the stable mode.
}
\author{
  Diethelm Wuertz for the Rmetrics \R-port;
  minor cleanup by Martin Maechler.
}
\seealso{
  For definition and the \dQuote{dpqr}-functions,
  \code{\link{StableDistribution}},
  also for the references.
}
\examples{
## beta = 0  <==>  symmetric  <==>  mode = 0
all.equal(stableMode(alpha=1, beta=0), 0)
al.s <- c(1e-100, seq(0,2, by = 1/32)[-1])
stopifnot(vapply(al.s, function(alp)
                 stableMode(alpha=alp, beta=0), 1.) == 0)

## more interesting: asymmetric (beta != 0):
stableMode(alpha=1.2, beta=0.1)

if(stabledist:::doExtras()) { # takes 2.5 seconds
 sm0.5 <- vapply(al.s, function(AA)
                 stableMode(alpha=AA, beta= 0.5), 1.)
 plot(al.s, sm0.5, type = "o", col=2, xlab = quote(alpha), ylab="mode",
      main = quote("Mode of stable"*{}(alpha, beta == 0.5, pm==0)))
}%doExtras
}
\keyword{distribution}

