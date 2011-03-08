\name{StableMode}
\alias{stableMode}
\title{Mode of the Stable Distribution Function}
\description{
  Computes the mode of the stable distribution function.

  Finds the maximum of \code{\link{dstable}} numerically, using
  \code{\link{uniroot}}.
}
\usage{
stableMode(alpha, beta,
           beta.max = 1 - 1e-11,
           tol = .Machine$double.eps^0.25)
}
\arguments{
  \item{alpha, beta}{numeric parameters:
    value of the index parameter \code{alpha} in the range \eqn{(0,2]},
    and the skewness parameter \code{beta}, in the range \eqn{[-1, 1]}.}
  \item{beta.max}{for numerical purposes, values of beta too close to 1,
    are set to \code{beta.max}.  Do not modify unless you know what
    you're doing.}
  \item{tol}{numerical tolerance for \code{\link{optimize}()}.}
}
\value{
  returns a numeric value, the location of the stable mode.
}

\author{
  Diethelm Wuertz for the Rmetrics \R-port;
  minor cleanup by Martin Maechler.
}

\references{
Chambers J.M., Mallows, C.L. and Stuck, B.W. (1976);
    \emph{A Method for Simulating Stable Random Variables},
    J. Amer. Statist. Assoc. 71, 340--344.

Nolan J.P. (1999);
    \emph{Stable Distributions},
    Preprint, University Washington DC, 30 pages.

Nolan J.P. (1999);
    \emph{Numerical Calculation of Stable Densities and Distribution
    Functions},
    Preprint, University Washington DC, 16 pages.

Samoridnitsky G., Taqqu M.S. (1994);
    \emph{Stable Non-Gaussian Random Processes, Stochastic Models
    with Infinite Variance},
    Chapman and Hall, New York, 632 pages.

Weron, A., Weron R. (1999);
    \emph{Computer Simulation of Levy alpha-Stable Variables and
    Processes},
    Preprint Technical Univeristy of Wroclaw, 13 pages.
}

\examples{
## stableMode -
   stableMode(alpha=1, beta=0)
   stableMode(alpha=1.2, beta=0.1)
}
\keyword{distribution}

