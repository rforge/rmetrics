\name{Specific Generalized Inverse Gaussian Moments and Mode}
\alias{gigMean}
\alias{gigVar}
\alias{gigSkew}
\alias{gigKurt}
\alias{gigMode}

\title{Moments and Mode of the Generalized Inverse Gaussian Distribution}
\description{
  Functions to calculate the mean, variance, skewness, kurtosis and mode
  of a specific generalized inverse Gaussian distribution.
}

\usage{
gigMean(Theta)
gigVar(Theta)
gigSkew(Theta)
gigKurt(Theta)
gigMode(Theta)
}
\arguments{
  \item{Theta}{Parameter vector of the generalized inverse Gaussian
    distribution.}
}

\value{
  \code{gigMean} gives the mean of the generalized inverse Gaussian
  distribution, \code{gigVar} the variance, \code{gigSkew} the skewness,
  \code{gigKurt} the kurtosis, and \code{gigMode} the mode. The formulae
  used are as given in Jorgensen (1982),
  pp. 13--17. Note that the kurtosis is the standardised fourth cumulant
  or what is sometimes called the kurtosis excess. (See
  \url{http://mathworld.wolfram.com/Kurtosis.html} for a discussion.)

  The parameterization used  for the generalized inverse Gaussian
  distribution is the \eqn{(\chi,\psi)}{(chi, psi)} one (see
  \code{\link{dgig}}). To use another parameterization, use
  \code{\link{gigChangePars}}.
}
\references{
  Jorgensen, B. (1982).
  \emph{Statistical Properties of the Generalized Inverse Gaussian
    Distribution}.
  Lecture Notes in Statistics, Vol. 9, Springer-Verlag, New York.
}

\author{David Scott \email{d.scott@auckland.ac.nz}}

\seealso{
  \code{\link{dgig}}, \code{\link{gigChangePars}},
  \code{\link{besselK}}
}

\examples{
Theta <- c(-0.5,5,2.5)
gigMean(Theta)
gigVar(Theta)
gigSkew(Theta)
gigKurt(Theta)
gigMode(Theta)
}
\keyword{distribution}
