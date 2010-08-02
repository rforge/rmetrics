\name{hyperbHessian}
\alias{hyperbHessian}
\alias{sumX}

\title{
Calculate Two-Sided Hessian for the Hyperbolic Distribution
}
\description{
Calculates the Hessian of a function, either exactly or approximately. Used to
obtaining the information matrix for maximum likelihood estimation.
}
\usage{
hyperbHessian(x, param, hessianMethod = c("exact", "tsHessian"),
              whichParam = 1:5, ...)
sumX(x, mu, delta, r, k)
}
\arguments{
  \item{x}{Data vector.}
  \item{param}{The maximum likelihood estimates parameter vector of the
    hyperbolic distribution. There are five different sets of
    parameterazations can be used in this function, the first four sets
    are listed in \code{hyperbChangePars} and the last set is the log
    scale of the first set of the parameterization, i.e.,
    \code{mu,log(delta),Pi,log(zeta)}.  }
  \item{hessianMethod}{Two methods are available to calculate the
      Hessian exactly ("exact") or approximately ("tsHessian").  }   
  \item{whichParam}{Numeric. A number between 1 to 5 indicating which
    set of the parameterization is the specified value in argument
    \code{param} belong to.} 
  \item{\dots}{Values of other parameters of the function \code{fun} if
    required.} 
  \item{mu}{Value of the parameter \eqn{\mu}{mu} of the
    hyperbolic distribution.}
  \item{delta}{Value of the parameter \eqn{\delta}{delta} of the
    hyperbolic distribution.} 
  \item{r}{Parameter used in calculating a cumulative sum of the data
    vector x.} 
  \item{k}{Parameter used in calculating a cumulative sum of the data
    vector x.}
}
\details{
  
    The formulae for the exact Hessian are derived by \code{Maple}
    software with some simplifications. For now, the exact Hessian can
    only be obtained based on the first, second or the last
    parameterization sets.  The approximate Hessian is obtained via a
    call to \code{tsHessian} from the package \code{DistributionUtils}.
    \code{summary.hyperbFit} calls the function \code{hyperbHessian} to
    calculate the Hessian matrix when the argument \code{hessian =
    TRUE}.   
}
\value{
  \code{hyperbHessian} gives the approximate or exact Hessian matrix for
  the data vector \code{x} and the estimated parameter vector
  \code{param}.  \code{sumX} is a sum term used in calculating the exact
  Hessian. It is called by \code{hyperbHessian} when the argument
  \code{hessianMethod = "exact"}.  It is not expected to be called
  directly by users.
}
\author{
  David Scott \email{d.scott@auckland.ac.nz},
  Christine Yang Dong \email{c.dong@auckland.ac.nz}
}

\examples{
### Calculate the exact Hessian using hyperbHessian:
param <- c(2, 2, 2, 1)
dataVector <- rhyperb(500, param = param)
fit <- hyperbFit(dataVector, method = "BFGS")
coef=coef(fit)
hyperbHessian(x=dataVector, param=coef, hessianMethod = "exact",
              whichParam = 2)
              
### Or calculate the exact Hessian using summary.hyperbFit method:
summary(fit, hessian = TRUE)


## Claculate the approximate Hessian:
summary(fit, hessian = TRUE, hessianMethod = "tsHessian")
}



