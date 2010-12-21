\name{distIneqMassartPlot}
\alias{distIneqMassartPlot}

\title{
Massart Inequality Plot Function
}
\description{
Create a Massart inequality plot implements a test of the empirical
distribution and distribution function based on an inequality due to
Massart (1990).
}
\usage{
distIneqMassartPlot(densFn = "norm", param = NULL,
                    nboots = 50, n = 100, ...)
}

\arguments{
  \item{densFn}{Character. The root name of the distribution to be tested.}
  \item{n}{Numeric. The size of the sample to be used.}
  \item{nboots}{Numeric. The number of bootstrap to approximate the LHS
    probability of the inequality.}
  \item{param}{Numeric. A vector giving the parameter values for the
    distribution specified by \code{densFn}. If no \code{param} values
    are specified, then the default parameter values of each
    distribution are used instead.}
  \item{\dots}{Passes the parameters of the distribution other than
    specified by param.}
}
\details{
  Massart (1990) gave a version of the Dvoretsky-Kiefer-Wolfowitz
  inequality with the best possible constant:

  \deqn{P\left(\sup_{x}|\hat F_n(x)-F(x)|> t\right) \leq%
    2\exp(-2nt^2)}{%
    P(sup_x|F_n(x)-F(x)|> t) <= 2exp(-2nt^2)}

  where \eqn{\hat F_n}{ F_n} is the empirical distribution function for
  a sample of \eqn{n} independent and identically distributed random
  variables with distribution function \eqn{F}. This inequality is true
  for all distribution functions, for all \eqn{n} and \eqn{t}.

  This red curve in the plot is the LHS probabilities and the black
  curve is the RHS bound.
}
\value{Returns \code{NULL} invisibly.
}
\references{
  Massart P. (1990) The tight constant in the Dvoretsky-Kiefer-Wolfovitz
  inequality. \emph{Ann. Probab.}, \bold{18}, 1269--1283.
}
\author{
  David Scott \email{d.scott@auckland.ac.nz}
  Xinxing Li \email{xli053@aucklanduni.ac.nz}
}
\examples{
### The Massart Inequality plot for standard Normal Distribution
distIneqMassartPlot()

### The Massart Inequality plot for Gamma Distribution
distIneqMassartPlot("gamma", shape = 1)
}
\keyword{distribution}
\keyword{univar}