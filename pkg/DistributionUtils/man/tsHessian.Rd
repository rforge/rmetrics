\name{tsHessian}
\alias{tsHessian}

\title{
Calculate Two-Sided Hessian Approximation
}
\description{
Calculates an approximation to the Hessian of a function. Used for
obtaining an approximation to the information matrix for maximum
likelihood estimation.
}
\usage{
tsHessian(param, fun, ...)
}

\arguments{
  \item{param}{
Numeric. The Hessian is to be evaluated at this point.
}
  \item{fun}{
A function of the parameters specified by \code{param}, and possibly
  other parameters.
}
  \item{\dots}{
Values of other parameters of the function \code{fun} if required.
}
}
\details{
  As a typical statistical application, the function \code{fun} is the
  log-likelihood function, \code{param} specifies the maximum likelihood
  estimates of the parameters of the distribution, and the data
  constitutes the other parameter values required for determination of
  the log-likelihood function.
}
\value{
  The approximate Hessian matrix of the function \code{fun} where
  differentiation is with respect to the vector of parameters
  \code{param} at the point given by the vector \code{param}.
}

\author{
  David Scott \email{d.scott@auckland.ac.nz},
  Christine Yang Dong \email{c.dong@auckland.ac.nz}
}
\note{
This code was borrowed from the fBasics package, utils-hessian.R with
slight modification. This was in turn borrowed from Kevin Sheppard's
Matlab garch toolbox as implemented by Alexios Ghalanos in his rgarch package
}

\examples{
### Consider Hessian of log(1 + x + 2y)
### Example from Lang: A Second Course in Calculus, p.74
fun <- function(param){
  x <- param[1]
  y <- param[2]
  return(log(1 + x + 2*y))
}

### True value of Hessian at (0,0)
trueHessian <- matrix( c(-1,-2,
                         -2,-4), byrow = 2, nrow = 2)
trueHessian

### Value from tsHessian
approxHessian <- tsHessian(c(0,0), fun = fun)
approxHessian
maxDiff <- max(abs(trueHessian - approxHessian))
### Should be approximately 0.045
maxDiff

}

\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
