\name{hist-methods}
\alias{hist.ghyp}
\alias{hist-methods}
\alias{hist,ghyp-method}
% <---------------------------------------------------------------------->
\title{Histogram for univariate generalized hyperbolic distributions}
% <---------------------------------------------------------------------->
\description{

  The function \code{hist} computes a histogram of the given data values
  and the univariate generalized hyperbolic distribution.

}
% <---------------------------------------------------------------------->
\usage{
\S4method{hist}{ghyp}(x, data = ghyp.data(x), gaussian = TRUE,
     log.hist = F, ylim = NULL, ghyp.col = 1, ghyp.lwd = 1,
     ghyp.lty = "solid", col = 1, nclass = 30, plot.legend = TRUE,
     location = if (log.hist) "bottom" else "topright", legend.cex = 1, ...)
}
% <---------------------------------------------------------------------->
\arguments{
  \item{x}{Usually a fitted univariate generalized hyperbolic distribution
           of class \code{\link[=mle.ghyp-class]{mle.ghyp}}. Alternatively
           an object of class \code{\link[=ghyp-class]{ghyp}} and a data vector.}
  \item{data}{An object coercible to a \code{vector}.}
  \item{gaussian}{If \code{TRUE} the probability density of the normal distribution is plotted as a reference.}
  \item{log.hist}{If \code{TRUE} the logarithm of the histogramm is plotted.}
  \item{ylim}{The \dQuote{y} limits of the plot.}
  \item{ghyp.col}{The color of the density of the generalized
                  hyperbolic distribution.}
  \item{ghyp.lwd}{The line width of the density of the generalized
                  hyperbolic distribution.}
  \item{ghyp.lty}{The line type of the density of the generalized
                  hyperbolic distribution.}
  \item{col}{The color of the histogramm.}
  \item{nclass}{A single number giving the number of cells for the histogramm.}
  \item{plot.legend}{If \code{TRUE} a legend is drawn.}
  \item{location}{The location of the legend. See \code{\link{legend}} for possible values.}
  \item{legend.cex}{The character expansion of the legend.}
  \item{\dots}{Arguments passed to \code{\link{plot}} and  \code{\link{qqghyp}}.}

}
% <---------------------------------------------------------------------->
\value{
  No value is returned.
}
% <---------------------------------------------------------------------->
\author{David Lüthi}
% <---------------------------------------------------------------------->
\seealso{\code{\link{qqghyp}}, \code{\link{fit.ghypuv}},
        \code{\link{hist}}, \code{\link{legend}}, \code{\link[=plot.ghyp]{plot}},
        \code{\link[=lines.ghyp]{lines}}.}
% <---------------------------------------------------------------------->
\examples{
  data(smi.stocks)
  univariate.fit <- fit.ghypuv(data = smi.stocks[,"SMI"],
                               opt.pars = c(mu = FALSE, sigma = FALSE),
                               symmetric = TRUE)
  hist(univariate.fit)
}
% <---------------------------------------------------------------------->
\keyword{hplot}
\keyword{methods}
