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
gigMean(chi = 1, psi = 1, lambda = 1,
        param = c(chi, psi, lambda))
gigVar(chi = 1, psi = 1, lambda = 1,
       param = c(chi, psi, lambda))
gigSkew(chi = 1, psi = 1, lambda = 1,
        param = c(chi, psi, lambda))
gigKurt(chi = 1, psi = 1, lambda = 1,
        param = c(chi, psi, lambda))
gigMode(chi = 1, psi = 1, lambda = 1,
        param = c(chi, psi, lambda))
}
\arguments{
  \item{chi}{A shape parameter that by default holds a value of 1.}
  \item{psi}{Another shape parameter that is set to 1 by default.}
  \item{lambda}{Shape parameter of the GIG distribution. Common to all forms of 
    parameterization. By default this is set to 1.}
  \item{param}{Parameter vector of the generalized inverse Gaussian
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
  distribution is the \eqn{(\chi, \psi)}{(chi, psi)} one (see
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
param <- c(5, 2.5, -0.5)
gigMean(param = param)
gigVar(param = param)
gigSkew(param = param)
gigKurt(param = param)
gigMode(param = param)
}
\keyword{distribution}
