\name{hyperbCvMTest}
\alias{hyperbCvMTest}
\alias{hyperbCvMTestPValue}
\alias{print.hyperbCvMTest}

\title{Cramer-von~Mises Test of a Hyperbolic Distribution}
\description{
  Carry out a Cramer-von~Mises test of a hyperbolic
  distribution where the parameters of the distribution are estimated,
  or calculate the p-value for such a test.
}
\usage{
hyperbCvMTest(x, Theta, conf.level = 0.95, \dots)
hyperbCvMTestPValue(xi, chi, Wsq, digits = 3)
\method{print}{hyperbCvMTest}(x, prefix = "\t", \dots)
}
\arguments{
  \item{x}{A numeric vector of data values for \code{hyperbCvMTest}, or
    object of class \code{"hyperbCvMTest"} for \code{print.hyperbCvMTest}.}
  \item{Theta}{Parameters of the hyperbolic distribution taking the form
    \code{c(pi,zeta,delta,mu)}.}
  \item{conf.level}{Confidence level of the the confidence interval.}
  \item{\dots}{Further arguments to be passed to or from methods.}
  \item{xi}{Value of \eqn{\xi}{xi} in the \eqn{(\xi,\chi)}{(xi,chi)}
    parameterization of the hyperbolic distribution.}
  \item{chi}{Value of \eqn{\chi}{chi} in the \eqn{(\xi,\chi)}{(xi,chi)}
    parameterisation of the hyperbolic distribution.}
  \item{Wsq}{Value of the test statistic in the Cramer-von~Mises 
    test of the hyperbolic distribution.}
  \item{digits}{Number of decimal places for p-value.}
  \item{prefix}{Character(s) to be printed before the description of the
    test.}
}
\details{
  \code{hyperbCvMTest} carries out a Cramer-von~Mises
  goodness-of-fit test of the hyperbolic distribution. The parameter
  \code{Theta} must be given in the \eqn{(\pi,\zeta)}{(pi,zeta)}
  parameterisation.

  \code{hyperbCvMTestPValue} calculates the p-value of the test, and is
  not expected to be called by the user. The method used is
  interpolation in Table 5 given in Puig & Stephens (2001), which
  assumes all the parameters of the distribution are unknown. Since the
  table used is limited, large p-values are simply given as
  \dQuote{>~0.25} and very small ones as \dQuote{<~0.01}. The table is
  created as the matrix \code{wsqTable} when the package
  \code{HyperbolicDist} is invoked.

  \code{print.hyperbCvMTest} prints the output
  from the
  Cramer-von~Mises goodness-of-fit test for
  the hyperbolic distribution in very similar format to that provided by
  \code{print.htest}. The only reason for having a special print method
  is that p-values can be given as less than some value or greater than
  some value, such as \dQuote{<\ ~0.01}, or \dQuote{>\ ~0.25}.


}
\value{
  \code{hyperbCvMTest} returns a list with class \code{hyperbCvMTest}
    containing the following components:
  \item{statistic}{The value of the test statistic.}
  \item{method}{A character string with the value
    \dQuote{Cramer-von~Mises test of hyperbolic
      distribution}.}
  \item{data.name}{A character string giving the name(s) of the data.}
  \item{parameter}{The value of the parameter Theta.}
  \item{p.value}{The p-value of the test.}
  \item{warn}{A warning if the parameter values are outside the limits
    of the table given in Puig & Stephens (2001).}

  \code{hyperbCvMTestPValue} returns a list with the elements
  \code{p.value} and \code{warn} only.

}
\author{David Scott, Thomas Tran}
\references{
  Puig, Pedro and Stephens, Michael A. (2001),
  Goodness-of-fit tests for the hyperbolic distribution.
  \emph{The Canadian Journal of Statistics/La Revue Canadienne de
    Statistique}, \bold{29}, 309--320.
}
\examples{
Theta <- c(2,2,2,2)
dataVector <- rhyperb(500, Theta)
fittedTheta <- hyperbFit(dataVector)$Theta
hyperbCvMTest(dataVector, fittedTheta)
dataVector <- rnorm(1000)
fittedTheta <- hyperbFit(dataVector, startValues = "FN")$Theta
hyperbCvMTest(dataVector, fittedTheta)
}
\keyword{htest}
\keyword{print}
