\name{distCheck}
\alias{distCheck}
\title{Distribution Check}

\description{
  Tests properties of an \R implementation of a distribution, i.e. of
  all four of its \dQuote{dpqr} functions.
}

\usage{
distCheck(fun = "norm", n = 1000, robust = TRUE, subdivisions = 100, \dots)
}
\arguments{
    \item{fun}{a character string denoting the name of the distribution.}
    \item{n}{an integer specifying the number of random variates to be
        generated.}
    \item{robust}{logical flag, should robust estimates be used? By
        default \code{TRUE}.}
    \item{subdivisions}{integer specifying the numbers of subdivisions
      in integration.}
    \item{\dots}{the distributional parameters.}
}

\examples{
distCheck("norm", mean = 1, sd = 1)

distCheck("lnorm", meanlog = 0.5, sdlog = 2, robust=FALSE)
## here, true E(X) = exp(mu + 1/2 sigma^2) = exp(.5 + 2) = exp(2.5) = 12.182
## and      Var(X) = exp(2*mu + sigma^2)*(exp(sigma^2) - 1) =       7954.67
}
\keyword{distribution}


