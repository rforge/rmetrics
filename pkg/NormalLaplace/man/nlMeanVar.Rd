\name{NormalLaplaceMeanVar}
\alias{NormalLaplaceMeanVar}
\alias{nlMean}
\alias{nlVar}
\alias{nlSkew}
\alias{nlKurt}

\title{
Mean, Variance, Skewness and Kurtosis of the Normal Laplace Distribution.
}
\description{
  Functions to calculate the mean, variance, skewness and kurtosis 
  of a specified normal Laplace distribution.

}
\usage{
nlMean(mu = 0, sigma = 1, alpha = 1, beta = 1,
         param = c(mu,sigma,alpha,beta),log = FALSE)
nlVar(mu = 0, sigma = 1, alpha = 1, beta = 1,
         param = c(mu,sigma,alpha,beta),log = FALSE)
nlSkew(mu = 0, sigma = 1, alpha = 1, beta = 1,
         param = c(mu,sigma,alpha,beta),log = FALSE)
nlKurt(mu = 0, sigma = 1, alpha = 1, beta = 1,
         param = c(mu,sigma,alpha,beta), log = FALSE)

}
\arguments{
  \item{mu}{Location parameter \eqn{\mu}{mu}, default is 0.}
  \item{sigma}{Scale parameter \eqn{\sigma}{sigma}, default is 1.}
  \item{alpha}{Skewness parameter \eqn{\alpha}{alpha}, default is 1.}
  \item{beta}{Shape parameter \eqn{\beta}{beta}, default is 1.}
  \item{param}{Specifying the parameters as a vector of the form\cr
    \code{c(mu,sigma,alpha,beta)}.}
  \item{log}{Logical; if \code{log = TRUE}, probabilities are given as
    log(p).}
}

\details{
  Users may either specify the values of the parameters individually or
  as a vector. If both forms are specified, then the values specified by
  the vector \code{param} will overwrite the other ones.

  the mean function is 
  \deqn{E(Y)=\mu+1/\alpha-1/\beta}{E(Y)=mu+1/alpha-1/beta}%
  
  the variance function is 
  \deqn{var(Y)=\sigma^2+1/\alpha^2+1/\beta^2}{var(Y)=sigma^2+1/sigma^2+1/beta^2}%

  the skewness function is 
  \deqn{\Upsilon = [2/\alpha^3-2/\beta^3]/[\sigma^2+1/\alpha^2+1/\beta^2]^{3/2}}{skewness = [2/alpha^3-2/beta^3]/[sigma^2+1/alpha^2+1/beta^2]^3/2}%

  the kurtosis function is 
  \deqn{\Gamma = [6/\alpha^4 + 6/\beta^4]/[\sigma^2+1/\alpha^2+1/\beta^2]^2}{kurtosis = [6/alpha^4 + 6/beta^4]/[sigma^2+1/alpha^2+1/beta^2]^2}%

}
\value{
  \code{nlMean} gives the mean of the skew hyperbolic
  \code{nlVar} the variance,
  \code{nlSkew} the skewness, 
  \code{nlKurt} the kurtosis and
  
}

\references{
  
  William J. Reed. (2006) The Normal-Laplace Distribution and Its
  Relatives. In \emph{Advances in Distribution Theory, Order Statistics
  and Inference}, pp. 61--74.  Birkhauser,  Boston.

}

\author{
David Scott \email{d.scott@auckland.ac.nz}, Jason Shicong Fu
}

\examples{
param <- c(10,1,5,9)
nlMean(param = param)
nlVar(param = param)
nlSkew(param = param)
nlKurt(param = param)


curve(dnl(x, param = param), -10, 10)

}
\keyword{distribution}
