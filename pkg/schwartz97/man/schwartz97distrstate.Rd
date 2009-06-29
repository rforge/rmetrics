\name{distribution-state}
\docType{methods}
\title{Schwartz two-factor Model: Distribution of State Variables}

\alias{pstate}
\alias{pstate,ANY,ANY,ANY,numeric-method}
\alias{pstate,ANY,ANY,ANY,schwartz2factor-method}

\alias{dstate}
\alias{dstate,ANY,ANY,numeric-method}
\alias{dstate,ANY,ANY,schwartz2factor-method}

\alias{qstate}
\alias{qstate,ANY,ANY,numeric-method}
\alias{qstate,ANY,ANY,schwartz2factor-method}

% <---------------------------------------------------------------------->
\description{

  Density, distribution function and quantile function of the state
  variables with respect to the objective probability measure. The state
  variables are the commodity spot price \code{s0} and the spot
  convenience yield \code{delta0}. The commodity log spot price and the
  convenience yield follow a joint bivariate normal distribution.

}

% <---------------------------------------------------------------------->
\usage{

\S4method{dstate}{ANY,ANY,numeric}(x, time = 1, s0 = 50, delta0 = 0,
       mu = 0.1, sigmaS = 0.3, kappa = 1, alpha = 0,
       sigmaE = 0.5, rho = 0.75, \dots)

\S4method{dstate}{ANY,ANY,schwartz2factor}(x, time = 1, s0, \dots)


\S4method{pstate}{ANY,ANY,ANY,numeric}(lower, upper, time = 1, s0 = 50, delta0 = 0,
       mu = 0.1, sigmaS = 0.3, kappa = 1, alpha = 0,
       sigmaE = 0.5, rho = 0.75, \dots)

\S4method{pstate}{ANY,ANY,ANY,schwartz2factor}(lower, upper, time = 1, s0, \dots)

\S4method{qstate}{ANY,ANY,numeric}(p, time = 1, s0 = 50, delta0 = 0,
       mu = 0.1, sigmaS = 0.3, kappa = 1, alpha = 0,
       sigmaE = 0.5, rho = 0.75, tail = "lower.tail", \dots)

\S4method{qstate}{ANY,ANY,schwartz2factor}(p, time = 1, s0, tail = "lower.tail", \dots)

}

% <---------------------------------------------------------------------->
\arguments{

  \item{lower}{The vector of lower limits of length 2. Note that first
    component stands for lower limit of the commodity spot price
    rather than the log-price.}
  \item{upper}{The vector of upper limits of length 2. Note that first
    component stands for the upper limit of the commodity spot price
    rather than the log-price.}
  \item{x}{Vector or matrix of quantiles. If \code{x} is
    a matrix, each row is taken to be a quantile.}
  \item{p}{Probability.}
  \item{tail}{See \code{\link[mvtnorm:qmvnorm]{qmvnorm}} of package
    \code{\link[mvtnorm:00Index]{mvtnorm}}.}
  \item{time}{Time where the joint stochastic process is evaluated
    (relative to time zero).}
  \item{s0}{Either an object inheriting from class
    \code{\link[=schwartz2factor-class]{schwartz2factor}} or a
    \code{numeric} representing the initial value of the commodity spot
    price.}
  \item{delta0}{Initial value of the convenience yield.}
  \item{mu}{enters the drift of the commodity spot price.}
  \item{kappa}{Speed of mean-reversion of the convenience yield process.}
  \item{alpha}{Mean-level of the convenience yield process.}
  \item{sigmaS}{Diffusion parameter of the spot price-process.}
  \item{sigmaE}{Diffusion parameter of the convenience yield process.}
  \item{rho}{Correlation coefficient between the Brownian motion
    driving the spot price and the convenience yield process.}
  \item{\dots}{Further arguments to be passed to methods of package
    \code{\link[mvtnorm:00Index]{mvtnorm}}.}
}

% <---------------------------------------------------------------------->
\details{

  The above methods rely on the functions
  \code{\link[mvtnorm:pmvnrom]{pmvnorm}},
  \code{\link[mvtnorm:Mvnrom]{dmvnorm}},
  \code{\link[mvtnorm:qmvnrom]{qmvnorm}} and
  \code{\link[mvtnorm:Mvnrom]{rmvnorm}} of the package
  \code{\link[mvtnorm:00Index]{mvtnorm}}.

}

% <---------------------------------------------------------------------->
\value{

  Probabilities, densities, or quantiles of the commodity spot price and
  instantaneous spot convenience yield.

}

% <---------------------------------------------------------------------->
\author{Philipp Erb, David Luethi, Juri Hinz}

% <---------------------------------------------------------------------->
\seealso{

  \code{\link[=schwartz2factor-class]{schwartz2factor}}-class
  description, \code{\link{rstate}} and \code{\link{simstate}} for random
  number generation.

}

% <---------------------------------------------------------------------->
\examples{
## Create a "schwartz2factor"-object
model <- schwartz2factor()

## Probability
pstate(lower = c(0, -Inf), upper = c(45, 0.01), time = 1, model)

## Density
dstate(x = c(50, 0.03), time = 2, model)

## Quantile
qstate(p = 0.5, s0 = model)

}

% <---------------------------------------------------------------------->
\keyword{distribution}
\keyword{models}

