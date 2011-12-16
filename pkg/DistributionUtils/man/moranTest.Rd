\name{moranTest}
\alias{moranTest}
\title{
Moran's Log Spacings Test
}
\description{
The function implements a goodness fit test using Moran's log spacings
statistic.
}
\usage{
moranTest(x, densFn, param = NULL, ...)
}

\arguments{
  \item{densFn}{Character. The root name of the distribution to be tested.}
  \item{x}{Vector of random generated numbers under tested distribution.}
  \item{param}{Numeric. A vector giving the parameter values for the
    distribution specified by \code{densFn}. If no \code{param} values
    are specified, then the default parameter values of each
    distribution are used instead.}
  \item{\dots}{Additional arguments to allow specification of the
  parameters of the distribution other than specified by param.}
}
\details{
  Moran (1951) gave a statistic for testing the goodness of fit of a
  random sample of \eqn{x}-values to a continuous univariate
  distribution with cumulative distribution function \eqn{F(x,
  \theta)}{F(x, theta)},
  where \eqn{\theta} is a vector of known parameters. This function
  implements the Cheng and Stephens (1989) extended Moran test for
  unknown parameters.

  The test statistic is
  \deqn{T(\hat \theta)=(M(\hat
    \theta)+1/2k-C_1)/C_2}{T(thetahat)=(M(thetahat)+1/2k-C1)/C2}
  Where \eqn{M(\hat \theta)}, the Moran statistic proposed by Moran, is
  \deqn{M(\theta)=-(log(y_1-y_0)+log(y_2-y_1)+...+log(y_m-y_{m-1}))}
  {M(theta)=-(log(y_1-y_0)+log(y_2-y_1)+...+log(y_m-y_m-1))}

  This test has null hypothesis:
  \eqn{H_0}{H_0} : a random sample of \eqn{n} values of \eqn{x} comes
  from distribution \eqn{F(x, \theta)}{F(x, theta)}, where
  \eqn{\theta}{theta} is the vector of parameters.
  Where \eqn{\theta}{theta} is expected to be the maximum
  likelihood estimate \eqn{\hat \theta}{thetahat}, an efficient
  estimate. The test rejects \eqn{H_0}{H_0} at significant level
  \eqn{\alpha}{alpha} if \eqn{T(\hat \theta)}{T(thetahat)} >
  \eqn{\chi^2_n(\alpha)}{chisq(alpha, df = n)}.

}
\value{
  \item{statistic}{Numeric. The value of Moran test statistics.}
  \item{estimate}{Numeric. A vector of parameter estimate of the tested 
                  distribution.}
  \item{parameter}{Numeric. the degrees of freedom for the moran statistic.}
  \item{p.value}{Numeric. The p-value for the test}
  \item{data.name}{Charactor. A charactor string gives the name(s) of the 
                   data}
  \item{method}{Charactor. Type of test performed}
}
\references{
Cheng, R. C. & Stephens, M. A. (1987). A Goodness-of-Fit Test Using
  Moran Statistic with Estimated Parameters. \emph{Biometrika},
  \bold{76 (2)}, 386–-392.
}
\author{
  David Scott \email{d.scott@auckland.ac.nz},
  Xinxing Li \email{xli053@aucklanduni.ac.nz}
}

\examples{

### Normal Distribution
x <- rnorm(100, mean = 0, sd = 1)
muhat <- mean(x)
sigmahat <- sqrt(var(x)*(100 - 1)/100)
result <- moranTest(x, "norm", mean = muhat, sd = sigmahat)
result

### Exponential Distribution
y <- rexp(200, rate = 3)
lambdahat <- 1/mean(y)
result <- moranTest(y, "exp", rate = lambdahat)
result

\keyword{distribution}
\keyword{univariate}

