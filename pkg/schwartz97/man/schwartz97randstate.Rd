\name{rand-state}
\docType{methods}
\title{Schwartz two-factor Model: Sampling from the State Variables}

\alias{rstate}
\alias{rstate,ANY,ANY,numeric-method}
\alias{rstate,ANY,ANY,schwartz2factor-method}

\alias{simstate}
\alias{simstate,ANY,ANY,numeric-method}
\alias{simstate,ANY,ANY,schwartz2factor-method}


% <---------------------------------------------------------------------->
\description{

  Random number and trajectory generation of the state variables with
  respect to the objective probability measure. The state variables are
  the commodity spot price \code{s0} and the spot convenience yield
  \code{delta0}. The commodity log spot price and the convenience yield
  follow a joint bivariate normal distribution.

}

% <---------------------------------------------------------------------->
\usage{

\S4method{rstate}{ANY,ANY,numeric}(n, time = 1, s0 = 50, delta0 = 0,
        mu = 0.1, sigmaS = 0.3, kappa = 1, alpha = 0,
        sigmaE = 0.5, rho = 0.75, method = "chol")

\S4method{rstate}{ANY,ANY,schwartz2factor}(n, time = 1, s0, method = "chol")

\S4method{simstate}{ANY,ANY,numeric}(n, time = 1, s0 = 50, delta0 = 0,
          mu = 0.1, sigmaS = 0.3, kappa = 1, alpha = 0,
          sigmaE = 0.5, rho = 0.75, method = "chol")

\S4method{simstate}{ANY,ANY,schwartz2factor}(n, time = 1, s0, method = "chol")

}

% <---------------------------------------------------------------------->
\arguments{

  \item{n}{Number of observations.}
  \item{method}{See \code{\link[mvtnorm:Mvnorm]{rmvnorm}} of package
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

  Samples or trajectories of the commodity spot price and instantaneous
  spot convenience yield as \code{matrix}.

}

% <---------------------------------------------------------------------->
\author{Philipp Erb, David Luethi, Juri Hinz}

% <---------------------------------------------------------------------->
\seealso{

  \code{\link[=schwartz2factor-class]{schwartz2factor}}-class
  description.
  \code{\link[=dstate]{d/p/q/state}} for density, distribution function
  and quantile function of the state variables.


}
% <---------------------------------------------------------------------->
\examples{
## Create a "schwartz2factor"-object
model <- schwartz2factor()

## Sample
sim <- rstate(n = 1000, s0 = model)
par(mfrow = c(1, 2))
hist(sim[,1], main = "Spot Price")
hist(sim[,2], main = "Convenience Yield")
}

% <---------------------------------------------------------------------->
\keyword{models}
\keyword{datagen}
