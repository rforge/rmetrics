\name{ghyp-risk-performance}
\alias{ESghyp}
\alias{ghyp.omega}
\title{Risk and Performance Measures}
% <---------------------------------------------------------------------->
\description{

  Functions to compute the risk measure \emph{Expected Shortfall} and
  the performance measure \emph{Omega} based on univariate generalized
  hyperbolic distributions.

}
% <---------------------------------------------------------------------->
\usage{
ESghyp(alpha, object = ghyp(), distr = c("return", "loss"), ...)

ghyp.omega(L, object = ghyp(), ...)
}
% <---------------------------------------------------------------------->
\arguments{
  \item{alpha}{A vector of confidence levels.}
  \item{L}{A vector of threshold levels.}
  \item{object}{A univarite generalized hyperbolic distribution object inheriting from class \code{\link[=ghyp-class]{ghyp}}.}
  \item{distr}{Whether the ghyp-object specifies a return or a loss-distribution (see \bold{Details}).}
  \item{\dots}{Arguments passed from \code{ESghyp} to \code{\link{qghyp}} and from \code{ghyp.omega} \code{\link{integrate}}.}
}
% <---------------------------------------------------------------------->
\details{

   The parameter \code{distr} specifies whether the ghyp-object
   describes a return or a loss-distribution.  In case of a return
   distribution the expected-shortfall on a confidence level
   \eqn{\alpha}{alpha} is defined as \eqn{\hbox{ES}_\alpha := \hbox{E}(X
   | X \leq F^{-1}_X(\alpha))}{ES_alpha := E(X | X <= F^-1(alpha))}
   while in case of a loss distribution it is defined on a confidence
   level \eqn{\alpha}{alpha} as \eqn{\hbox{ES}_\alpha := \hbox{E}(X | X
   > F^{-1}_X(\alpha))}{ES_alpha := E(X | X > F^-1(alpha))}.\cr

   \emph{Omega} is defined as the ratio of a European call-option price
   divided by a put-option price with strike price L (see
   \bold{References}): \eqn{\Omega(L) := \frac{C(L)}{P(L)}}{Omega(L) =
   C(L)/P(L)}.

}
% <---------------------------------------------------------------------->
\value{
 \code{ESghyp} gives the expected shortfall and  \cr
 \code{ghyp.omega} gives the performance measure Omega.
}
% <---------------------------------------------------------------------->
\references{
  \emph{Omega as a Performance Measure} by Hossein Kazemi, Thomas
  Schneeweis and Raj Gupta \cr
  University of Massachusetts, 2003 \cr
}
% <---------------------------------------------------------------------->
\author{David Lüthi}
% <---------------------------------------------------------------------->
\seealso{

  \code{\link{ghyp-class}} definition, \code{\link{ghyp}} constructors,
         univariate fitting routines, \code{\link{fit.ghypuv}},
         \code{\link{portfolio.optimize}} for portfolio optimization
         with respect to alternative risk measures,
         \code{\link{integrate}}.

}
% <---------------------------------------------------------------------->
\examples{
  data(smi.stocks)

  ## Fit a NIG model to Credit Suisse and Swiss Re log-returns
  cs.fit <- fit.NIGuv(smi.stocks[, "CS"], silent = TRUE)
  swiss.re.fit <- fit.NIGuv(smi.stocks[, "Swiss.Re"], silent = TRUE)

  ## Confidence levels for expected shortfalls
  es.levels <- c(0.001, 0.01, 0.05, 0.1)

  cs.es <- ESghyp(es.levels, cs.fit)
  swiss.re.es <- ESghyp(es.levels, swiss.re.fit)

  ## Threshold levels for Omega
  threshold.levels <- c(0, 0.01, 0.02, 0.05)

  cs.omega <- ghyp.omega(threshold.levels, cs.fit)
  swiss.re.omega <- ghyp.omega(threshold.levels, swiss.re.fit)

  par(mfrow = c(2, 1))

  barplot(rbind(CS = cs.es, Swiss.Re = swiss.re.es), beside = TRUE,
          names.arg = paste(100 * es.levels, "percent"), col = c("gray40", "gray80"),
          ylab = "Expected Shortfalls (return distribution)", xlab = "Level")

  legend("bottomright", legend = c("CS", "Swiss.Re"), fill = c("gray40", "gray80"))

  barplot(rbind(CS = cs.omega, Swiss.Re = swiss.re.omega), beside = TRUE,
          names.arg = threshold.levels, col = c("gray40", "gray80"),
          ylab = "Omega", xlab = "Threshold level")
  legend("topright", legend = c("CS", "Swiss.Re"), fill = c("gray40", "gray80"))

  ## => the higher the performance, the higher the risk (as it should be)
}
% <---------------------------------------------------------------------->
\keyword{utilities}
\keyword{misc}
