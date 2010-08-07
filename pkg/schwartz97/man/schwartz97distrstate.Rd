\name{distribution-state}
\docType{methods}
\title{Schwartz two-factor Model: Distribution of the State Variables}

\alias{pstate}
\alias{pstate,ANY,ANY,ANY,numeric-method}
\alias{pstate,ANY,ANY,ANY,schwartz2f-method}

\alias{dstate}
\alias{dstate,ANY,ANY,numeric-method}
\alias{dstate,ANY,ANY,schwartz2f-method}

\alias{qstate}
\alias{qstate,ANY,ANY,numeric-method}
\alias{qstate,ANY,ANY,schwartz2f-method}

% <---------------------------------------------------------------------->
\description{

  Density, distribution function and quantile function of the state
  variables. The state variables are the commodity spot price \code{s}
  and the spot convenience yield \code{delta}. The commodity log spot
  price and the convenience yield follow a bivariate normal
  distribution.

}

% <---------------------------------------------------------------------->
\usage{

\S4method{dstate}{ANY,ANY,numeric}(x, time = 1, s0 = 50, delta0 = 0,
       mu = 0.1, sigmaS = 0.3, kappa = 1, alpha = 0,
       sigmaE = 0.5, rho = 0.75, \dots)

\S4method{dstate}{ANY,ANY,schwartz2f}(x, time = 1, s0, \dots)


\S4method{pstate}{ANY,ANY,ANY,numeric}(lower, upper, time = 1, s0 = 50, delta0 = 0,
       mu = 0.1, sigmaS = 0.3, kappa = 1, alpha = 0,
       sigmaE = 0.5, rho = 0.75, \dots)

\S4method{pstate}{ANY,ANY,ANY,schwartz2f}(lower, upper, time = 1, s0, \dots)

\S4method{qstate}{ANY,ANY,numeric}(p, time = 1, s0 = 50, delta0 = 0,
       mu = 0.1, sigmaS = 0.3, kappa = 1, alpha = 0,
       sigmaE = 0.5, rho = 0.75, tail = "lower.tail", \dots)

\S4method{qstate}{ANY,ANY,schwartz2f}(p, time = 1, s0, tail = "lower.tail", \dots)

}

% <---------------------------------------------------------------------->
\arguments{

  \item{x}{Vector or matrix of quantiles. If \code{x} is
    a matrix, each row is taken to be a quantile.}
  \item{time}{Time at which the quantity is computed (relative to time
    zero).} 
  \item{p}{Probability, a scalar.}
  \item{lower}{The vector of lower limits of length 2. Note that first
    component stands for lower limit of the commodity spot price
    rather than the log-price.}
  \item{upper}{The vector of upper limits of length 2. Note that first
    component stands for the upper limit of the commodity spot price
    rather than the log-price.}
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
  \item{tail}{See \code{\link[mvtnorm]{qmvnorm}} of package
    \code{mvtnorm}.}
  \item{\dots}{Further arguments to be passed to methods of package
    \code{mvtnorm}.}
}

% <---------------------------------------------------------------------->
\details{

  The model and its parameters are described in the \bold{Details}
  section of the \code{\link[=schwartz2f-class]{schwartz2f}}-class
  documentation and in the package vignette \emph{Technical Document}.\cr

  The above methods rely on the functions
  \code{\link[mvtnorm]{pmvnorm}},
  \code{\link[mvtnorm]{dmvnorm}}, and
  \code{\link[mvtnorm]{qmvnorm}} 
   of the package
  \code{mvtnorm}.

}

% <---------------------------------------------------------------------->
\value{

  \code{dstate} and \code{pstate} return a \code{numeric}, \code{qstate}
  returns the output of \code{\link[mvtnorm]{qmvnorm}} as a \code{list}.

}

% <---------------------------------------------------------------------->
\author{Philipp Erb, David Luethi, Juri Hinz}

% <---------------------------------------------------------------------->
\seealso{

  \code{\link[=schwartz2f-class]{schwartz2f}}-class description,
  \code{\link{rstate}} and \code{\link{simstate}} for random number
  generation, constructors \code{\link{schwartz2f}} and
  \code{\link{fit.schwartz2f}}.

}

% <---------------------------------------------------------------------->
\examples{
### Create a "schwartz2f"-object
model <- schwartz2f()

### Probability
pstate(lower = c(0, -Inf), upper = c(45, 0.01), time = 1, model)

### Density
dstate(x = c(50, 0.03), time = 2, model) 
dstate(x = rbind(c(50, 0.03), c(50, 0.1)), time = 2, model) # x is a matrix 

### Quantile
qstate(p = 0.5, s0 = model)

### Generate random numbers
object <- schwartz2f(alpha = 0.05)
samples <- rstate(1000, time = 2, object)
### ...and plot histograms
par(mfrow = c(2, 1))
hist(samples[,1])
abline(v = mean(object, time = 2)[1], col = "red")
hist(samples[,2])
abline(v = mean(object, time = 2)[2], col = "red")

}

% <---------------------------------------------------------------------->
\keyword{distribution}
\keyword{models}

