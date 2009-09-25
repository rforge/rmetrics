% --- Source file: vgMeanVarMode.Rd ---
\name{Variance Gamma Mean, Variance, Skewness, Kurtosis and Mode}
\alias{vgMean}
\alias{vgVar}
\alias{vgSkew}
\alias{vgKurt}
\alias{vgMode}
\title{Moments and Mode of the Variance Gamma Distribution}
\description{
  Functions to calculate the mean, variance, skewness, kurtosis and mode of a 
  specific variance gamma distribution.
}

\usage{
vgMean(vgC = 0, sigma = 1, theta = 0, nu = 1, param = c(vgC,sigma,theta,nu))
vgVar(vgC = 0, sigma = 1, theta = 0, nu = 1, param = c(vgC,sigma,theta,nu))
vgSkew(vgC = 0, sigma = 1, theta = 0, nu = 1, param = c(vgC,sigma,theta,nu))
vgKurt(vgC = 0, sigma = 1, theta = 0, nu = 1, param = c(vgC,sigma,theta,nu))
vgMode(vgC = 0, sigma = 1, theta = 0, nu = 1, param = c(vgC,sigma,theta,nu))
}

\arguments{
  \item{vgC}{The location parameter \eqn{c}{c}, default is equal to 0.}
  \item{sigma}{The spread parameter \eqn{\sigma}{sigma}, default is equal to 1, 
    must be positive.}
  \item{theta}{The asymmetry parameter \eqn{\theta}{theta}, default is equal 
    to 0.}
  \item{nu}{The shape parameter \eqn{\nu}{nu}, default is equal to 1, must be 
    positive.}
  \item{param}{Specifying the parameters as a vector which takes the form 
    \code{c(vgC,sigma,theta,nu)}.}
}

\value{
  \code{vgMean} gives the mean of the variance gamma distribution,
  \code{vgVar} the variance, \code{vgSkew} the skewness, \code{vgKurt} the 
  kurtosis, and \code{vgMode} the mode.
  The formulae used for the mean and variance are as given in
  Seneta (2004).
  If \eqn{\nu}{nu} is greater than or equal to 2, the mode is equal to the value
  of the parameter \eqn{c}{c}. Otherwise, it is found by a numerical 
  optimisation using \code{\link{optim}}.

  The parameterisation of the variance gamma distribution used
  for these functions is the \eqn{(c,\sigma,\theta,\nu)}{(c,sigma,theta,nu)}
  one. See \code{\link{vgChangePars}} to transfer between parameterisations.
}
\references{
  Seneta, E. (2004). Fitting the variance-gamma model to financial data.
  \emph{J. Appl. Prob.}, 41A:177--187.
  Kotz, S, Kozubowski, T. J., and \enc{Podgórski}{Podgorski},
  K. (2001). 
  The Laplace Distribution and Generalizations. \emph{Birkhauser},
  Boston, 349 p.          
}

\author{David Scott \email{d.scott@auckland.ac.nz},
  Christine Yang Dong \email{c.dong@auckland.ac.nz}}

\seealso{
  \code{\link{dvg}}, \code{\link{vgChangePars}},\code{\link{vgCalcRange}},
  \code{\link{besselK}}.
}

\examples{
param <- c(2,2,2,0.5)
vgMean(param = param)
## Or to specify parameter values individually, use:
vgMean (2,2,2,0.5)
  
vgVar(param = param)
vgSkew(param = param)
vgKurt(param = param)
vgMode(param = param)
maxDens <- dvg(vgMode(param = param), param = param)
vgRange <- vgCalcRange(param = param, tol = 10^(-2)*maxDens)
curve(dvg(x, param = param), vgRange[1], vgRange[2])
abline(v = vgMode(param = param), col = "blue")
abline(v = vgMean(param = param), col = "red")
}