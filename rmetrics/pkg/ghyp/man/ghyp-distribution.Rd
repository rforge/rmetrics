\name{ghyp-distribution}
\alias{dghyp}
\alias{pghyp}
\alias{rghyp}
\alias{qghyp}
\title{The Generalized Hyperbolic Distribution}
% <---------------------------------------------------------------------->
\description{

  Density, distribution function, quantile function, expected-shortfall
  and random generation for the univariate and multivariate generalized
  hyperbolic distribution and its special cases.

}
% <---------------------------------------------------------------------->
\usage{
dghyp(x, object = ghyp(), logvalue = FALSE)

pghyp(q, object = ghyp(), n.sim = 10000, subdivisions = 200,
      rel.tol = .Machine$double.eps^0.5, abs.tol = rel.tol,
      lower.tail = TRUE)

qghyp(p, object = ghyp(), method = c("integration", "splines"),
      spline.points = 200, subdivisions = 200,
      root.tol = .Machine$double.eps^0.5,
      rel.tol = root.tol^1.5, abs.tol = rel.tol)

rghyp(n, object = ghyp())
}
% <---------------------------------------------------------------------->
\arguments{
  \item{p}{A vector of probabilities.}
  \item{x}{A vector, matrix or data.frame of quantiles.}
  \item{q}{A vector, matrix or data.frame of quantiles.}
  \item{n}{Number of observations.}
  \item{object}{An object inheriting from class \code{\link[=ghyp-class]{ghyp}}.}
  \item{logvalue}{If \code{TRUE} the logarithm of the density will be returned.}
  \item{n.sim}{The number of simulations when computing \code{pghyp} of a multivariate
               generalized hyperbolic distribution.}
  \item{subdivisions}{The number of subdivisions passed to \code{\link{integrate}} when
                      computing the distribution function \code{pghyp} of a
                      univariate generalized hyperbolic distribution.}
  \item{rel.tol}{The relative accuracy requested from  \code{\link{integrate}}.}
  \item{abs.tol}{The absolute accuracy requested from  \code{\link{integrate}}.}
  \item{lower.tail}{If TRUE (default), probabilities are
                    \eqn{P[X \le x]}{P[X <= x]}, otherwise, \eqn{P[X > x]}{P[X > x]}.}
  \item{method}{The method how quantiles are computed (see \bold{Details}).}
  \item{spline.points}{The number of support points when computing the quantiles with the method
                       \dQuote{splines} instead of \dQuote{integration}.}
  \item{root.tol}{The tolerance of \code{\link{uniroot}}.}
}
% <---------------------------------------------------------------------->
\details{

   \code{qghyp} only works for univariate generalized hyperbolic
   distributions. \cr

   \code{pghyp} performs a numeric integration of the density in the
   univariate case. The multivariate cumulative distribution is computed
   by means of monte carlo simulation. \cr

   \code{qghyp} computes the quantiles either by using the
   \dQuote{integration} method where the root of the distribution
   function is solved or via \dQuote{splines} which interpolates the
   distribution function and solves it with \code{\link{uniroot}}
   afterwards. The \dQuote{integration} method is recommended when only
   few quantiles are required. If more than approximately 20 quantiles
   are needed to be calculated the \dQuote{splines} method becomes
   faster.  The accuracy can be controlled with an adequate setting of
   the parameters \code{rel.tol}, \code{abs.tol}, \code{root.tol} and
   \code{spline.points}.\cr

   \code{rghyp} uses the random generator for generalized inverse
    gaussian distributed random variates from the S-Plus and R library
    \emph{QRMlib} (see
    \url{http://www.math.ethz.ch/~mcneil/book/QRMlib.html}).

}
% <---------------------------------------------------------------------->
\value{
  \code{dghyp} gives the density, \cr \code{pghyp} gives the distribution function, \cr
  \code{qghyp} gives the quantile function, \cr
  \code{rghyp} generates random deviates.
}
% <---------------------------------------------------------------------->
\references{

  \emph{Quantitative Risk Management: Concepts, Techniques and Tools} by
  Alexander J. McNeil, Ruediger Frey and Paul Embrechts \cr Princeton
  Press, 2005 \cr

  \code{ghyp}-package vignette in the \code{doc} folder or on
  \url{http://cran.r-project.org/web/packages/ghyp/}

}
% <---------------------------------------------------------------------->
\author{David Luethi}
% <---------------------------------------------------------------------->
\note{

  Objects generated with \code{\link{hyp}}, \code{\link{NIG}},
  \code{\link{VG}} and \code{\link{student.t}} have to use \code{Xghyp}
  as well. E.g. \code{dNIG(0, NIG())} does not work but \code{dghyp(0,
  NIG())}.\cr

  When the skewness becomes very large the functions using \code{qghyp}
  may fail.  The functions \code{\link{qqghyp}},
  \code{\link[=pairs.ghyp]{pairs}} and \code{\link{portfolio.optimize}}
  are based on \code{qghyp}.

}
% <---------------------------------------------------------------------->
\seealso{\code{\link{ghyp-class}} definition, \code{\link{ghyp}} constructors,
         fitting routines \code{\link{fit.ghypuv}} and \code{\link{fit.ghypmv}},
         risk and performance measurement \code{\link{ESghyp}} and \code{\link{ghyp.omega}},
         \code{\link[=transform.ghyp]{transformation}} and \code{\link[=transform.ghyp]{subsettting}} of \code{ghyp} objects,
         \code{\link{integrate}}, \code{\link{spline}}.}
% <---------------------------------------------------------------------->
\examples{
  ## Univariate generalized hyperbolic distribution
  univariate.ghyp <- ghyp()

  par(mfrow=c(5, 1))

  quantiles <- seq(-4, 4, length = 500)
  plot(quantiles, dghyp(quantiles, univariate.ghyp))
  plot(quantiles, pghyp(quantiles, univariate.ghyp))

  probabilities <- seq(1e-4, 1-1e-4, length = 500)
  plot(probabilities, qghyp(probabilities, univariate.ghyp, method = "splines"))

  hist(rghyp(n=10000,univariate.ghyp),nclass=100)

  ## Mutivariate generalized hyperbolic distribution
  multivariate.ghyp <- ghyp(sigma=var(matrix(rnorm(10),ncol=2)),mu=1:2,gamma=-(2:1))

  par(mfrow=c(2, 1))

  quantiles <- outer(seq(-4, 4, length = 50), c(1, 1))
  plot(quantiles[, 1], dghyp(quantiles, multivariate.ghyp))
  plot(quantiles[, 1], pghyp(quantiles, multivariate.ghyp, n.sim = 1000))

  rghyp(n = 10, multivariate.ghyp)
}
% <---------------------------------------------------------------------->
\keyword{distribution}
\keyword{models}
\keyword{multivariate}
\keyword{datagen}
