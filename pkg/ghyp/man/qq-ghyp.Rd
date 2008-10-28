\name{qq-ghyp}
\alias{qqghyp}
% <---------------------------------------------------------------------->
\title{Quantile-Quantile Plot}
% <---------------------------------------------------------------------->
\description{

  This function is intended to be used as a graphical diagnostic tool
  for fitted univariate generalized hyperbolic distributions. Optionally
  a qq-plot of the normal distribution can be added.

}
% <---------------------------------------------------------------------->
\usage{
qqghyp(object, data = ghyp.data(object), gaussian = TRUE, line = TRUE,
       main = "Generalized Hyperbolic Q-Q Plot",
       xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
       ghyp.pch = 1, gauss.pch = 6, ghyp.lty = "solid",
       gauss.lty = "dashed", ghyp.col = "black", gauss.col = "black",
       plot.legend = TRUE, location = "topleft", legend.cex = 0.8,
       spline.points = 150, root.tol = .Machine$double.eps^0.5,
       rel.tol = root.tol, abs.tol = root.tol^1.5, add = FALSE, ...)
}
% <---------------------------------------------------------------------->
\arguments{
  \item{object}{Usually a fitted univariate generalized hyperbolic distribution
                of class \code{\link[=mle.ghyp-class]{mle.ghyp}}. Alternatively
                an object of class \code{\link[=ghyp-class]{ghyp}} and a data vector.}
  \item{data}{An object coercible to a \code{vector}.}
  \item{gaussian}{If \code{TRUE} a qq-plot of the normal distribution is plotted as a reference.}
  \item{line}{If \code{TRUE} a line is fitted and drawn.}
  \item{main}{An overall title for the plot.}
  \item{xlab}{A title for the x axis.}
  \item{ylab}{A title for the y axis.}
  \item{ghyp.pch}{A plotting character, i.e., symbol to use for quantiles of
                  the generalized hyperbolic distribution.}
  \item{gauss.pch}{A plotting character, i.e., symbol to use for quantiles of
                  the normal distribution.}
  \item{ghyp.lty}{The line type of the fitted line to the quantiles of the generalized
                  hyperbolic distribution.}
  \item{gauss.lty}{The line type of the fitted line to the quantiles of the normal distribution.}
  \item{ghyp.col}{A color of the quantiles of the generalized
                  hyperbolic distribution.}
  \item{gauss.col}{A color of the quantiles of the normal distribution.}
  \item{plot.legend}{If \code{TRUE} a legend is drawn.}
  \item{location}{The location of the legend. See \code{\link{legend}} for possible values.}
  \item{legend.cex}{The character expansion of the legend. }
  \item{spline.points}{The number of support points when computing the quantiles.
                       Passed to \code{\link{qghyp}}.}
  \item{root.tol}{The tolerance of the quantiles. Passed to \code{\link{uniroot}}.}
  \item{rel.tol}{The tolerance of the quantiles. Passed to \code{\link{integrate}}.}
  \item{abs.tol}{The tolerance of the quantiles. Passed to \code{\link{integrate}}.}
  \item{add}{If \code{TRUE} the points are added to an existing plot window. The \code{legend} argument then becomes deactivated.}
  \item{\dots}{Arguments passed to \code{\link{plot}}.}
}
% <---------------------------------------------------------------------->
\author{David Luethi}
% <---------------------------------------------------------------------->
\seealso{\code{\link[=hist.ghyp]{hist}}, \code{\link{fit.ghypuv}}, \code{\link{qghyp}},
         \code{\link[=plot.ghyp]{plot}},
         \code{\link[=lines.ghyp]{lines}}}
% <---------------------------------------------------------------------->
\examples{
  data(smi.stocks)

  smi <- fit.ghypuv(data = smi.stocks[, "Swiss.Re"])

  qqghyp(smi, spline.points = 100)

  qqghyp(fit.tuv(smi.stocks[, "Swiss.Re"], symmetric = TRUE),
         add = TRUE, ghyp.col = "red", line = FALSE)
}
% <---------------------------------------------------------------------->
\keyword{models}
\keyword{hplot}
