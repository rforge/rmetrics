\name{distIneqMassart}
\alias{distIneqMassart}
\title{
Massart Inequality for Distributions
}
\description{
The function implements a test of the random number generator and
distribution function based on an inequality due to Massart (1990).
}
\usage{
distIneqMassart(densFn = "norm", n = 10000, probBound = 0.001, ...)
}

\arguments{
  \item{densFn}{Character. The root name of the distribution to be tested.}
  \item{n}{Numeric. The size of the sample to be used.}
  \item{probBound}{Numeric. The value of the bound on the right hand side of
    the Massart inequality. See \bold{Details}.}
  \item{\dots}{Additional arguments to allow specification of the
  parameters of the distribution.}
}
\details{
  Massart (1990) gave a version of the Dvoretsky-Kiefer-Wolfowitz
  inequality with the best possible constant:

  \deqn{P(\left(\sup_{x}|\hat F_n(x)-F(x)|> t\right) \leq%
    2\exp(-2nt^2)}{%
    P(sup_x|F_n(x)-F(x)|> t) <= 2exp(-2nt^2)}

  where \eqn{\hat F_n}{ F_n} is the empirical distribution function for
  a sample of \eqn{n} independent and identically distributed random
  variables with distribution function \eqn{F}. This inequality is true
  for all distribution functions, for all \eqn{n} and \eqn{t}.

  This test is used in base R to check the standard distribution
  functions. The code may be found in the file \code{p-r-random.tests.R}
  in the \code{tests} directory.
}
\value{
  \item{sup}{Numeric. The supremum of the absolute difference between
    the empirical distribution and the true distribution function.}
  \item{probBound}{Numeric. The value of the bound on the right hand side of
    the Massart inequality.}
  \item{t}{Numeric. The lower bound which the supremum of the absolute
    difference between the empirical distribution and the true
    distribution function must exceed.}
  \item{pVal}{Numeric. The probability that the absolute difference
    between the empirical distribution and the true distribution function
    exceeds \code{t}.}
  \item{check}{Logical. Indicates whether the inequality is satisfied
    or not.}
} 
\references{
Massart P. (1990) The tight constant in the Dvoretsky-Kiefer-Wolfovitz
inequality. \emph{Ann. Probab.}, \bold{18}, 1269--1283.
}
\author{
  David Scott \email{d.scott@auckland.ac.nz}, 
  Christine Yang Dong \email{c.dong@auckland.ac.nz}
}

\examples{
## Normal distribution is the default
distIneqMassart()
## Specify parameter values
distIneqMassart(mean = 1, sd = 2)
## Gamma distribution has no default value for shape
distIneqMassart("gamma", shape = 1)
}

\keyword{distribution}
\keyword{univariate}
