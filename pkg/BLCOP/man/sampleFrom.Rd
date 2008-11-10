\name{sampleFrom}
\alias{sampleFrom}
\title{Sample from a distribution object}
\description{
    Generates samples from a distribution held by an object of class \code{distribution} or
    \code{mvdistribution}.  Intended mainly for internal use.
}
\usage{
sampleFrom(dstn, n = 1)
}

\arguments{
  \item{dstn}{ an object of class \code{distribution} or \code{mvdistribution}.}
  \item{n}{Number of samples to generate}
}

\value{
    A vector or matrix of samples.
}

\author{Francisco Gochez <fgochez@mango-solutions.com>}

\examples{
    x <- distribution("pois", lambda = 5)
    hist(sampleFrom(x, 1000), col = "blue", prob = TRUE)

}
\keyword{utilities}
\keyword{distribution}

