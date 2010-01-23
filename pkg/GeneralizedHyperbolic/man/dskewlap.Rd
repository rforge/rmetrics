\name{SkewLaplace}
\alias{dskewlap}
\alias{pskewlap}
\alias{qskewlap}
\alias{rskewlap}


\title{Skew-Laplace Distribution}
\description{
  Density function, distribution function, quantiles and 
  random number generation for the skew-Laplace distribution.
}
\usage{
dskewlap(x, mu = 0, alpha = 1, beta = 1,
         param = c(mu, alpha, beta))
pskewlap(q, mu = 0, alpha = 1, beta = 1,
         param = c(mu, alpha, beta))
qskewlap(p, mu = 0, alpha = 1, beta = 1,
         param = c(mu, alpha, beta))
rskewlap(n, mu = 0, alpha = 1, beta = 1,
         param = c(mu, alpha, beta))
}
\arguments{
  \item{x, q}{Vector of quantiles.}
  \item{p}{Vector of probabilities.}
  \item{n}{Number of observations to be generated.}
  \item{mu}{The location parameter, set to 0 by default.}
  \item{alpha, beta}{The shape parameters, both set to 1 by default.}
  \item{param}{Vector of parameters of the skew-Laplace distribution:
    \eqn{\mu}{mu}, \eqn{\alpha}{alpha} and \eqn{\beta}{beta}}.
}
\details{
  The central skew-Laplace has mode zero, and is a mixture of a (negative)
  exponential distribution with mean \eqn{\beta}{beta}, and the negative of an 
  exponential distribution with mean \eqn{\alpha}{alpha}. The weights of
  the positive and negative components are proportional to their means.
  
  The general skew-Laplace distribution is a shifted central skew-Laplace
  distribution, where the mode is given by \eqn{\mu}{mu}.
  
  The density is given by:
  \deqn{f(x)=\frac{1}{\alpha+\beta} e^{(x - \mu)/\alpha}}{ %
  f(x)=(1/(alpha+beta)) e^((x - mu)/alpha)}
  for \eqn{x\leq\mu}{x <= mu}, and
  \deqn{f(x)=\frac{1}{\alpha+\beta} e^{-(x - \mu)/\beta}}{ %
  f(x)=(1/(alpha+beta)) e^(-(x - mu)/beta)}
  for \eqn{x\geq\mu}{x >= mu}
}
\value{
  \code{dskewlap} gives the density, \code{pskewlap} gives the distribution 
  function, \code{qskewlap} gives the quantile function and \code{rskewlap} 
  generates random variates. The distribution function is obtained by
  elementary integration of the density function. Random variates are
  generated from exponential observations using the characterization of
  the skew-Laplace as a mixture of exponential observations.
}
\references{
  Fieller, N. J., Flenley, E. C. and Olbricht, W. (1992)
  Statistics of particle size data.
  \emph{Appl. Statist.},
  \bold{41}, 127--146.
}
\author{David Scott \email{d.scott@auckland.ac.nz}, 
Ai-Wei Lee, Richard Trendall}

\seealso{\code{\link{hyperbFitStart}}}

\examples{
param <- c(1, 1, 2)
par(mfrow = c(1, 2))
curve(dskewlap(x, param = param), from = -5, to = 8, n = 1000)
title("Density of the\n Skew-Laplace Distribution")
curve(pskewlap(x, param = param), from = -5, to = 8, n = 1000)
title("Distribution Function of the\n Skew-Laplace Distribution")
dataVector <- rskewlap(500, param = param)
curve(dskewlap(x, param = param), range(dataVector)[1], range(dataVector)[2],
      n = 500)
hist(dataVector, freq = FALSE, add = TRUE)
title("Density and Histogram\n of the Skew-Laplace Distribution")
logHist(dataVector, main =
        "Log-Density and Log-Histogram\n of the Skew-Laplace Distribution")
curve(log(dskewlap(x, param = param)), add = TRUE,
      range(dataVector)[1], range(dataVector)[2], n = 500)
}
\keyword{distribution} 
