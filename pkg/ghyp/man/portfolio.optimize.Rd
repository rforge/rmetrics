\name{portfolio.optimize}
\alias{portfolio.optimize}
% <---------------------------------------------------------------------->
\title{Portfolio optimization with respect to alternative risk measures}
% <---------------------------------------------------------------------->
\description{

  This function performs a optimization of a portfolio with respect to
  one of the risk measures \dQuote{sd}, \dQuote{value.at.risk} or
  \dQuote{expected.shortfall}. The optimization task is either to
  find the \emph{global minimum risk} portfolio, the \emph{tangency}
  portfolio or the \emph{minimum risk} portfolio given a target-return.

}
% <---------------------------------------------------------------------->
\usage{
portfolio.optimize(object,
                   risk.measure = c("sd", "value.at.risk", "expected.shortfall"),
                   type = c("minimum.risk", "tangency", "target.return"),
                   level = 0.95, distr = c("loss", "return"),
                   target.return = NULL, risk.free = NULL,
                   silent = FALSE, ...)

}
% <---------------------------------------------------------------------->
\arguments{

\item{object}{A multivariate \code{\link[=ghyp-class]{ghyp}} object
  representing the loss distribution. In case \code{object} gives the
  return distribution set the argument \code{distr} to \dQuote{return}.}

\item{risk.measure}{How risk shall be measured.  Must be one of
                      \dQuote{sd} (standard deviation),
                      \dQuote{value.at.risk} or
                      \dQuote{expected.shortfall}.}

\item{type}{The tpye of the optimization problem. Must be one of
  \dQuote{minimum.risk}, \dQuote{tangency} or \dQuote{target.return}
  (see \bold{Details}).}

\item{level}{The confidence level which shall be used if
    \code{risk.measure} is either \dQuote{value.at.risk} or
    \dQuote{expected.shortfall}.}

\item{distr}{The default distribution is \dQuote{loss}. If \code{object}
  gives the return distribution set \code{distr} to \dQuote{return}.}

\item{target.return}{A numeric scalar specifying the target return if
  the optimization problem is of \code{type} \dQuote{target.return}.}

\item{risk.free}{A numeric scalar giving the risk free rate in case the
  optimization problem is of \code{type} \dQuote{tangency}.}

\item{silent}{If \code{TRUE} no prompts will appear in the console.}

\item{\dots}{Arguments passed to \code{\link{optim}}.}

}
% <---------------------------------------------------------------------->
\value{
  A list with components:
  \item{portfolio.dist}{An univariate generalized hyperbolic object of class \code{\link[=ghyp-class]{ghyp}} which
                   represents the distribution of the optimal portfolio.}
  \item{risk.measure}{The risk measure which was used.}
  \item{risk}{The risk.}
  \item{opt.weights}{The optimal weights.}
  \item{converged}{Convergence returned from \code{\link{optim}}.}
  \item{message}{A possible error message returned from \code{\link{optim}}.}
  \item{n.iter}{The number of iterations returned from \code{\link{optim}}.}
}
% <---------------------------------------------------------------------->
\author{David Lüthi}
% <---------------------------------------------------------------------->
\details{

  If \code{type} is \dQuote{minimum.risk} the global minimum risk
  portfolio is returned. \cr

  If \code{type} is \dQuote{tangency} the portfolio maximizing the slope
  of \dQuote{(expected return - risk free rate) / risk} will be
  returned. \cr

  If \code{type} is \dQuote{target.return} the portfolio with expected
  return \code{target.return} which minimizes the risk will be
  returned. \cr

  Note that in case of an elliptical distribution (symmetric generalized
  hyperbolic distributions) it does not matter which risk measure is
  used. That is, minimizing the standard deviation results in a
  portfolio which also minimizes the value-at-risk et cetera. \cr

}

% <---------------------------------------------------------------------->
\note{

  In case \code{object} denotes a non-elliptical distribution and the
  risk measure is either \dQuote{value.at.risk} or
  \dQuote{expected.shortfall}, then the type \dQuote{tangency}
  optimization problem is not supported. \cr

  Constraints like avoiding short-selling are not supported yet. \cr

}
% <---------------------------------------------------------------------->
\seealso{\code{\link[=transform.ghyp]{transform}}, \code{\link{fit.ghypmv}}}
% <---------------------------------------------------------------------->
\examples{

data(indices)

t.object <- fit.tmv(-indices, silent = TRUE)
gauss.object <- fit.gaussmv(-indices)

t.ptf <- portfolio.optimize(t.object,
                            risk.measure = "expected.shortfall",
                            type = "minimum.risk",
                            level = 0.99,
                            distr = "loss",
                            silent = TRUE)

gauss.ptf <- portfolio.optimize(gauss.object,
                                risk.measure = "expected.shortfall",
                                type = "minimum.risk",
                                level = 0.99,
                                distr = "loss")

par(mfrow = c(1, 3))

plot(c(t.ptf$risk, gauss.ptf$risk),
     c(-mean(t.ptf$portfolio.dist), -mean(gauss.ptf$portfolio.dist)),
     xlim = c(0, 0.035), ylim = c(0, 0.004),
     col = c("black", "red"), lwd = 4,
     xlab = "99 percent expected shortfall",
     ylab = "Expected portfolio return",
     main = "Global minimum risk portfolios")

legend("bottomleft", legend = c("Asymmetric t", "Gaussian"),
       col = c("black", "red"), lty = 1)

plot(t.ptf$portfolio.dist, type = "l",
     xlab = "log-loss ((-1) * log-return)", ylab = "Density")
lines(gauss.ptf$portfolio.dist, col = "red")

weights <- cbind(Asymmetric.t = t.ptf$opt.weights,
                 Gaussian = gauss.ptf$opt.weights)

barplot(weights, beside = TRUE, ylab = "Weights")

}
% <---------------------------------------------------------------------->
\keyword{optimize}
\keyword{multivariate}
\keyword{iteration}
