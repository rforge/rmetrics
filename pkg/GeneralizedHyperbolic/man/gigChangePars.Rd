\name{gigChangePars}
\alias{gigChangePars}

\title{Change Parameterizations of the Generalized Inverse Gaussian
	Distribution}

\description{
  This function interchanges between the following 4 parameterizations
  of the generalized inverse Gaussian distribution:

  1. \eqn{(\chi, \psi, \lambda)}{(chi, psi, lambda)}

  2. \eqn{(delta, \gamma, \lambda)}{(delta, gamma, lambda)}

  3. \eqn{(\alpha, \beta, \lambda)}{(alpha, beta, lambda)}

  4. \eqn{(\omega, \eta, \lambda)}{(omega, eta, lambda)}

  See \enc{Jörgensen}{Jorgensen} (1982) and Dagpunar (1989)
}

\usage{
gigChangePars(from, to, param, noNames = FALSE)
}

\arguments{
  \item{from}{The set of parameters to change from.}
  \item{to}{The set of parameters to change to.}
  \item{param}{\dQuote{\code{from}} parameter vector consisting of 3
    numerical elements.}
  \item{noNames}{Logical. When \code{TRUE}, suppresses the parameter
    \code{names} in the output.}
}

\details{
  The range of \eqn{\lambda}{lambda} is the whole real line.
  In each parameterization, the other two parameters must take positive
  values.
}

\value{
  A numerical vector of length 3 representing \code{param} in the
  \dQuote{\code{to}} parameterization.
}

\references{
  \enc{Jörgensen}{Jorgensen}, B. (1982).
  \emph{Statistical Properties of the Generalized Inverse Gaussian
    Distribution}.
  Lecture Notes in Statistics, Vol. 9, Springer-Verlag, New York.

  Dagpunar, J. S. (1989).
  An easily implemented generalised inverse Gaussian generator,
  \emph{Commun. Statist.---Simula.}, \bold{18}, 703--710.
}

\author{David Scott \email{d.scott@auckland.ac.nz}}

\seealso{\code{\link{dgig}}}

\examples{
param1 <- c(2.5, 0.5, 5)               # Parameterisation 1
param2 <- gigChangePars(1, 2, param1)   # Convert to parameterization 2
param2                                  # Parameterization 2
gigChangePars(2, 1, as.numeric(param2)) # Convert back to parameterization 1
}

\keyword{distribution}
