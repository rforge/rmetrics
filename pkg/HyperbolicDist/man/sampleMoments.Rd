\name{Sample Moments}
\alias{skewness}
\alias{kurtosis}
\title{Sample Skewness and Kurtosis}
\description{
  Computes the sample skewness and sample kurtosis.
}
\usage{
skewness(x, na.rm = FALSE)
kurtosis(x, na.rm = FALSE)
}
\arguments{
  \item{x}{A numeric vector containing the values whose skewness  or
    kurtosis is to be computed.}
  \item{na.rm}{A logical value indicating whether \code{NA} values
    should be stripped before the computation proceeds.}
}
\details{
  If \eqn{N = \mathrm{length}(x)}{N = length(x)}, then the skewness of
  \eqn{x} is defined as 
  \deqn{N^{-1} \mathrm{sd}(x)^{-3} \sum_i (x_i - \mathrm{mean}(x))^3.}{
    N^(-1) sd(x)^(-3) sum_i (x_i - mean(x))^3.}

  If \eqn{N = \mathrm{length}(x)}{N = length(x)}, then the kurtosis of
  \eqn{x} is defined as
  \deqn{N^{-1} \mathrm{sd}(x)^{-4}
    \sum_i(x_i - \mathrm{mean}(x))^4 - 3.}{
    N^(-1) sd(x)^(-4) sum_i (x_i - mean(x))^4 - 3.}
}
  

\value{
  The skewness or kurtosis of \code{x}.
}

\note{These functions and the description of them are taken from the
  package \code{e1071}. They are included to avoid having to require an
  additional package.}

\author{Evgenia Dimitriadou, Kurt Hornik, Friedrich Leisch, David Meyer,
  and Andreas Weingessel}

\examples{
x <- rnorm(100)
skewness(x)
kurtosis(x)
}
\keyword{univar}
