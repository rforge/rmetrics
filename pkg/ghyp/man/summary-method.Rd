\name{summary-method}
\alias{summary.mle.ghyp}
\alias{summary-methods}
\alias{summary,mle.ghyp-method}
% <---------------------------------------------------------------------->
\title{mle.ghyp summary}
% <---------------------------------------------------------------------->
\description{

  Produces a formatted output of a fitted generalized hyperbolic
  distribution.

}
% <---------------------------------------------------------------------->
\usage{
\S4method{summary}{mle.ghyp}(object)
}
% <---------------------------------------------------------------------->
\arguments{
  \item{object}{An object of class \code{\link[=mle.ghyp-class]{mle.ghyp}}.}
}
% <---------------------------------------------------------------------->
\value{
  Nothing is returned.
}
% <---------------------------------------------------------------------->
\author{David Luethi}
% <---------------------------------------------------------------------->
\seealso{Fitting functions \code{\link{fit.ghypuv}} and \code{\link{fit.ghypmv}},
         \code{\link[=coef.ghyp]{coef}}, \code{\link[=mean.ghyp]{mean}},
         \code{\link[=vcov.ghyp]{vcov}} and \code{\link{ghyp.fit.info}}
         for accessor functions for \code{\link[=mle.ghyp-class]{mle.ghyp}} objects.
}
% <---------------------------------------------------------------------->
\examples{
  data(smi.stocks)
  mle.ghyp.object <- fit.NIGmv(smi.stocks[, c("Nestle", "Swiss.Re", "Novartis")])
  summary(mle.ghyp.object)
}
% <---------------------------------------------------------------------->
\keyword{methods}
