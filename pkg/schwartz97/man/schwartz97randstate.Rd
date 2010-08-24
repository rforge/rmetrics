\name{rand-state}
\docType{methods}
\title{Schwartz two-factor Model: Sampling from the State Variables}

\alias{rstate}
\alias{rstate,ANY,ANY,numeric-method}
\alias{rstate,ANY,ANY,schwartz2f-method}

\alias{simstate}
\alias{simstate,ANY,ANY,numeric-method}
\alias{simstate,ANY,ANY,schwartz2f-method}


% <---------------------------------------------------------------------->
\description{

  Random number and trajectory generation of the state variables. The
  state variables are the commodity spot price \code{s0} and the spot
  convenience yield \code{delta0}. The commodity log spot price and the
  convenience yield follow a bivariate normal distribution.

}

% <---------------------------------------------------------------------->
\usage{

\S4method{rstate}{ANY,ANY,numeric}(n, time = 1, s0 = 50, delta0 = 0,
       mu = 0.1, sigmaS = 0.3, kappa = 1, alpha = 0,
       sigmaE = 0.5, rho = 0.75, method = "chol")

\S4method{rstate}{ANY,ANY,schwartz2f}(n, time = 1, s0, method = "chol")

\S4method{simstate}{ANY,ANY,numeric}(n, time = 1, s0 = 50, delta0 = 0,
         mu = 0.1, sigmaS = 0.3, kappa = 1, alpha = 0,
         sigmaE = 0.5, rho = 0.75, method = "chol")

\S4method{simstate}{ANY,ANY,schwartz2f}(n, time = 1, s0, method = "chol")

}

% <---------------------------------------------------------------------->
\arguments{

  \item{n}{Number of observations.}
  \item{time}{at which random numbers of the state variables are drawn (\code{rstate})
    or horizon of the trajectory (\code{simstate}) relative to now.}
  \item{s0}{Either a \code{numeric} representing the initial value of the
    commodity spot price or an object inheriting from class
  \code{\link[=schwartz2f-class]{schwartz2f}}.}
  \item{delta0}{Initial value of the convenience yield.}
  \item{mu}{enters the drift of the commodity spot price.}
  \item{sigmaS}{Diffusion parameter of the spot price-process.}
  \item{kappa}{Speed of mean-reversion of the convenience yield process.}
  \item{alpha}{Mean-level of the convenience yield process.}
  \item{sigmaE}{Diffusion parameter of the convenience yield process.}
  \item{rho}{Correlation coefficient between the Brownian motion
    driving the spot price and the convenience yield process.}
  \item{method}{See \code{\link[mvtnorm]{rmvnorm}} of package
    \code{mvtnorm}.}

}

% <---------------------------------------------------------------------->
\details{

  The model and its parameters are described in the \bold{Details}
  section of the \code{\link[=schwartz2f-class]{schwartz2f}}-class
  documentation and in the package vignette \emph{Technical Document}.

  The above methods rely on the functions
  \code{\link[mvtnorm]{pmvnorm}},
  \code{\link[mvtnorm]{dmvnorm}},
  \code{\link[mvtnorm]{qmvnorm}} and
  \code{\link[mvtnorm]{rmvnorm}} of the package
  \code{mvtnorm}.

}

% <---------------------------------------------------------------------->
\value{

  Samples or trajectories of the commodity spot price and instantaneous
  spot convenience yield as \code{matrix}.

}

% <---------------------------------------------------------------------->
\author{Philipp Erb, David Luethi, Juri Hinz}

% <---------------------------------------------------------------------->
\seealso{

  \code{\link[=schwartz2f-class]{schwartz2f}}-class
  description.
  \code{\link[=dstate]{d/p/q/state}} for the density, distribution,
  and quantile function of the state variables.


}
% <---------------------------------------------------------------------->
\examples{
## Create a "schwartz2f"-object
model <- schwartz2f()

## and sample from its distribution at time = 2.5.
sim <- rstate(n = 1000, s0 = model, time = 2.5)
par(mfrow = c(1, 2))
hist(sim[,1], main = "Distribution of Spot Price")
hist(sim[,2], main = "Distribution of Convenience Yield")


## Create a trajectory over a 6 years horizon sampled on a weekly basis.
trajectory <- simstate(6 * 52, time = 6, s0 = model)
par(mfrow = c(1, 2))
plot(trajectory[,1], main = "Spot Price", type = "l")
plot(trajectory[,2], main = "Convenience Yield", type = "l")

}

% <---------------------------------------------------------------------->
\keyword{models}
\keyword{datagen}
