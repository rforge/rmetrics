\name{gigChangePars}
\alias{gigChangePars}

\title{Change Parameterizations of the Generalized Inverse Gaussian
	Distribution}

\description{
  This function interchanges between the following 4 parameterizations
  of the generalized inverse Gaussian distribution:
	
  1. \eqn{(\lambda,\chi,\psi)}{(lambda, chi, psi)}
	
  2. \eqn{(\lambda,\delta,\gamma)}{(lambda, delta, gamma)}
	
  3. \eqn{(\lambda,\alpha,\beta)}{(lambda, alpha, beta)}
	
  4. \eqn{(\lambda,\omega,\eta)}{(lambda, omega, eta)}

  See J{\o}rgensen (1982) and Dagpunar (1989)
}
  
\usage{
gigChangePars(from, to, Theta, noNames = FALSE)
}

\arguments{
  \item{from}{The set of parameters to change from.}
  \item{to}{The set of parameters to change to.}
  \item{Theta}{\dQuote{\code{from}} parameter vector consisting of 3
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
  A numerical vector of length 3 representing \code{Theta} in the
  \dQuote{\code{to}} parameterization.
}

\references{
  J{\o}rgensen, B. (1982).
  \emph{Statistical Properties of the Generalized Inverse Gaussian
    Distribution}.
  Lecture Notes in Statistics, Vol. 9, Springer-Verlag, New York.
	
  Dagpunar, J. S. (1989).
  An easily implemented generalised inverse {G}aussian generator,
  \emph{Commun. Statist.---Simula.}, \bold{18}, 703--710.
}

\author{David Scott \email{d.scott@auckland.ac.nz}}

\seealso{\code{\link{dgig}}}

\examples{
Theta1 <- c(-0.5,5,2.5)                 # Parameterisation 1
Theta2 <- gigChangePars(1, 2, Theta1)   # Convert to parameterization 2
Theta2                                  # Parameterization 2
gigChangePars(2, 1, as.numeric(Theta2)) # Convert back to parameterization 1
}

\keyword{distribution}
