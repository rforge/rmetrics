\name{plot-lines-methods}
\alias{plot.ghyp}
\alias{plot,ghyp,missing-method}
\alias{plot-methods}
\alias{lines.ghyp}
\alias{lines-methods}
\alias{lines,ghyp-method}
% <---------------------------------------------------------------------->
\title{Plot univariate generalized hyperbolic densities}
% <---------------------------------------------------------------------->
\description{

  These functions plot probability densities of generalized hyperbolic
  distribution objects.

}
% <---------------------------------------------------------------------->
\usage{
\S4method{plot}{ghyp,missing}(x, range = qghyp(c(0.001, 0.999), x), length = 1000, ...)
\S4method{lines}{ghyp}(x, range = qghyp(c(0.001, 0.999), x), length = 1000, ...)
}
% <---------------------------------------------------------------------->
\arguments{
  \item{x}{An univariate \code{\link[=ghyp-class]{ghyp}} object.}
  \item{range}{The range over which the density will be plotted. The default is the range from
               the 0.1 \% quantile to the 99.9 \% quantile. When \code{range} has a length
               greater than 2 it is assumed to be the vector of quantiles and the density
               is computed on \code{range}.}
  \item{length}{The desired length of the density vector.}
  \item{\dots}{Arguments passed to \code{\link{plot}} and \code{\link{lines}} respectively.}

}
% <---------------------------------------------------------------------->
\details{
  When the density is very skewed, the computation of the quantile
  may fail. See \code{\link{qghyp}} for details.
}
% <---------------------------------------------------------------------->
\author{David Luethi}
% <---------------------------------------------------------------------->
\seealso{\code{\link[=hist.ghyp]{hist}}, \code{\link{qqghyp}},
\code{\link[=pairs.ghyp]{pairs}}, \code{\link{plot}},
\code{\link{lines}}.}
% <---------------------------------------------------------------------->
\examples{
  data(smi.stocks)

  smi.fit   <-  fit.tuv(data = smi.stocks[,"SMI"], symmetric = TRUE)
  nestle.fit <- fit.tuv(data = smi.stocks[,"Nestle"], symmetric = TRUE)

  ## Student-t distribution
  plot(smi.fit, type = "l", log = "y")
  lines(nestle.fit, col = "blue")

  ## Empirical
  lines(density(smi.stocks[,"SMI"]), lty = "dashed")
  lines(density(smi.stocks[,"Nestle"]), lty = "dashed", col = "blue")
}
% <---------------------------------------------------------------------->
\keyword{hplot}
\keyword{methods}
