\name{pairs-methods}
\alias{pairs.ghyp}
\alias{pairs-methods}
\alias{pairs,ghyp-method}
% <---------------------------------------------------------------------->
\title{Pairs plot for multivariate generalized hyperbolic distributions}
% <---------------------------------------------------------------------->
\description{

  This function is intended to be used as a graphical diagnostic tool
  for fitted multivariate generalized hyperbolic distributions. An array
  of graphics is created and qq-plots are drawn into the diagonal part
  of the graphics array. The upper part of the graphics matrix shows
  scatter plots whereas the lower part shows 2-dimensional histogramms.

}
% <---------------------------------------------------------------------->
\usage{
\S4method{pairs}{ghyp}(x, data = ghyp.data(x), main = "'ghyp' pairwise plot",
      nbins = 30, qq = TRUE, gaussian = TRUE,
      hist.col = c("white", topo.colors(40)),
      spline.points = 150, root.tol = .Machine$double.eps^0.5,
      rel.tol = root.tol, abs.tol = root.tol^1.5, ...)
}
% <---------------------------------------------------------------------->
\arguments{
  \item{x}{Usually a fitted multivariate generalized hyperbolic distribution
           of class \code{\link[=mle.ghyp-class]{mle.ghyp}}. Alternatively
           an object of class \code{\link[=ghyp-class]{ghyp}} and a data matrix.}
  \item{data}{An object coercible to a \code{matrix}.}
  \item{main}{The title of the plot.}
  \item{nbins}{The number of bins passed to \code{\link[pkg:gplots]{hist2d}}.}
  \item{qq}{If \code{TRUE} qq-plots are drawn.}
  \item{gaussian}{If \code{TRUE} qq-plots with the normal distribution are plotted.}
  \item{hist.col}{A vector of colors passed to \code{\link[pkg:gplots]{hist2d}}.}
  \item{spline.points}{The number of support points when computing the quantiles used by the
                       qq-plot. Passed to \code{\link{qqghyp}}.}
  \item{root.tol}{The tolerance of the quantiles. Passed to \code{\link{uniroot}} via \code{\link{qqghyp}}.}
  \item{rel.tol}{The tolerance of the quantiles. Passed to \code{\link{integrate}} via \code{\link{qqghyp}}.}
  \item{abs.tol}{The tolerance of the quantiles. Passed to \code{\link{integrate}} via \code{\link{qqghyp}}.}
  \item{\dots}{Arguments passed to \code{\link{plot}} and \code{\link{axis}}.}
}
% <---------------------------------------------------------------------->
\author{David Lüthi}
% <---------------------------------------------------------------------->
\seealso{\code{\link{pairs}}, \code{\link{fit.ghypmv}},
         \code{\link{qqghyp}}, \code{\link[pkg:gplots]{hist2d}}}
% <---------------------------------------------------------------------->
\examples{
  data(smi.stocks)
  fitted.smi.stocks <- fit.NIGmv(data = smi.stocks[1:200, ])
  pairs(fitted.smi.stocks)
}
% <---------------------------------------------------------------------->
\keyword{multivariate}
\keyword{methods}
\keyword{hplot}
