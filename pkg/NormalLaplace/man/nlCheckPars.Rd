\name{nlCheckPars}
\alias{nlCheckPars}

\title{
  Check Parameters of the Normal Laplace Distribution
}
\description{
  Given a set of parameters for the normal Laplace distribution,
  the functions checks the validity of each parameter and if they
  and if they correspond to the boundary cases.
}
\usage{
nlCheckPars(param)
}
\arguments{
  \item{param}{Numeric. Parameter values for the normal
    Laplace distribution.}
}
\details{
  The vector \code{param} takes the form \code{c(mu, sigma, alpha, beta)}.

  If any of \code{sigma}, \code{alpha} or \code{beta} is negative or NA, an
  error is returned.
}
\references{

  William J. Reed. (2006) The Normal-Laplace Distribution and Its
  Relatives. In \emph{Advances in Distribution Theory, Order Statistics
  and Inference}, pp. 61--74.  \enc{Birkhäuser}{Birkhauser},
  Boston.

}

\author{
  David Scott \email{d.scott@auckland.ac.nz}, Simon Potter
}

\examples{
## Correct parameters
nlCheckPars(c(0, 1.5, 1, 2))
nlCheckPars(c(3, 1, 1.5, 2))

## Incorrect parameters, each error providing a different error message
nlCheckPars(c(2, -1, 1, 1))          # invalid sigma
nlCheckPars(c(2, 1, -1, 2))          # invalid alpha
nlCheckPars(c(0, 1, 2, -1))          # invalid beta
nlCheckPars(c(0, -0.01, -0.1, 1))    # sigma and alpha incorrect
nlCheckPars(c(2, -0.5, 1, -0.2))     # sigma and beta incorrect
nlCheckPars(c(1, 1, -0.2, -1))       # alpha and beta incorrect
nlCheckPars(c(0, -0.1, -0.2, -0.3))  # all three parameters erroneous
nlCheckPars(c(0.5, NA, 1, 1))        # NA introduced
nlCheckPars(c(-1, 1, 1))             # incorrect number of parameters
}
\keyword{distribution}
