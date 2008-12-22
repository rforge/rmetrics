\name{Generalized Hyperbolic Moments and Mode}
\alias{ghypMean}
\alias{ghypVar}
\alias{ghypMode}
\title{Moments and Mode of the Generalized Hyperbolic Distribution}
\description{
  Functions to calculate the mean, variance, and mode of a specific
  generalized hyperbolic distribution.
}
\usage{
ghypMean(Theta)
ghypVar(Theta)
ghypMode(Theta)
}
\arguments{
  \item{Theta}{Parameter vector of the generalized hyperbolic distribution.}
}

\value{
  \code{ghypMean} gives the mean of the generalized hyperbolic distribution,
  \code{ghypVar} the variance, and \code{ghypMode} the mode. 
  The formulae used for the mean and variance are as given in
  Prause (1999). The mode is found by a numerical optimisation using
  \code{\link{optim}}.
  
  The parameterisation of the generalized hyperbolic distribution used
  for these functions is the \eqn{(\alpha,\beta)}{(alpha,beta)} one. See
  \code{\link{ghypChangePars}} to transfer between parameterisations.
}
\references{
  Prause, K. (1999) \emph{The generalized hyperbolic models: Estimation,
  financial derivatives and risk measurement.} PhD Thesis, Mathematics
  Faculty, University of Freiburg. 
}

\author{David Scott \email{d.scott@auckland.ac.nz}, Thomas Tran}

\seealso{
  \code{\link{dghyp}}, \code{\link{ghypChangePars}}, 
  \code{\link{besselK}}, \code{\link{RLambda}}.
}

\examples{
Theta <- c(2,2,1,2,2)
ghypMean(Theta)
ghypVar(Theta)
ghypMode(Theta)
maxDens <- dghyp(ghypMode(Theta), Theta)
ghypRange <- ghypCalcRange(Theta, tol = 10^(-3)*maxDens)
curve(dghyp(x, Theta), ghypRange[1], ghypRange[2])
abline(v = ghypMode(Theta), col = "blue")
abline(v = ghypMean(Theta), col = "red")
}
\keyword{distribution}
